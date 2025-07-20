/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cctype>
#include <cstdint>
#include <filesystem>
#include <ios>
#include <iostream>
#include <locale.h>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <variant>
#include <wchar.h>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/StringUtil.h>
#include <Util/TokenLocation.h>
#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>

namespace Arwen {

namespace fs = std::filesystem;

using namespace Util;
using namespace Interpreter;
using namespace IR;

using Interp = Arwen::Interpreter::Interpreter;

std::optional<std::wstring> get_command_string()
{
    std::cout << "*> " << std::flush;
    std::wstring ret;
    std::getline(std::wcin, ret);
    if (std::wcin.eof()) {
        return {};
    }
    return ret;
}

void dump_scopes(Interp &interpreter)
{
    std::cout << "Scopes: " << interpreter.scopes.size() << " top: 0x" << std::hex << interpreter.stack.top << std::dec << std::endl;
    if (interpreter.stack.top == 0) {
        std::cout << std::endl;
        return;
    }
    auto scopes { 0 };
    for (auto ix = 0; ix < interpreter.scopes.size(); ++ix) {
        auto &scope { interpreter.scopes[ix] };
        auto  next_bp = (ix < interpreter.scopes.size() - 1) ? interpreter.scopes[ix + 1].bp : interpreter.stack.top;
        if (scopes == 0) {
            std::cout << std::right << std::setw(2) << std::setfill(' ') << scope.bp;
        }
        std::cout << '.';
        ++scopes;
        if (next_bp > scope.bp) {
            std::cout << std::right << std::setw(4 - scopes) << std::setfill(' ') << " ";
            for (auto stix = scope.bp; stix < next_bp; stix += 8) {
                if (stix == scope.bp) {
                    std::cout << "   ";
                } else {
                    std::cout << "          ";
                }
                for (auto bix { 0 }; bix < 8 && stix + bix < next_bp; ++bix) {
                    std::cout << " 0x" << std::hex << std::setw(2) << std::setfill('0') << (interpreter.stack.stack[stix + bix] & 0xFF);
                }
                std::cout << " ";
                for (auto bix { 0 }; bix < 8 && stix + bix < next_bp; ++bix) {
                    if (isprint(interpreter.stack.stack[stix + bix])) {
                        std::cout << " " << interpreter.stack.stack[stix + bix];
                    } else {
                        std::cout << " .";
                    }
                }
                std::cout << "  ";
                for (auto &var : scope.variables) {
                    if (var.second.address == stix) {
                        std::wcout << var.first;
                    }
                }
                std::cout << std::endl;
            }
            scopes = 0;
        } else if (ix == interpreter.scopes.size() - 1) {
            std::cout << std::endl;
        }
    }
    std::cout << std::endl;
}

std::optional<std::wstring> load_std_lib()
{
    fs::path arw_dir { getenv("ARW_DIR") ? getenv("ARW_DIR") : ARWEN_DIR };
    if (arw_dir.empty()) {
        arw_dir = "/usr/share/arwen";
    }
    auto std_arw { arw_dir / "share" / "std.arw" };
    if (!fs::exists(std_arw)) {
        fatal("{} not found", std_arw.string());
    }
    if (auto contents_maybe = read_file_by_name<wchar_t>(std_arw.string()); !contents_maybe.has_value()) {
        std::wcerr << L"Could not read standard library `" << std_arw << L"`" << std::endl;
        return {};
    } else {
        return contents_maybe.value();
    }
}

int debugger_main(int argc, char const **argv)
{
    struct Context {
        std::wstring file_name;
        std::wstring contents;
        Parser       parser;
        pSyntaxNode  syntax;
        pSyntaxNode  normalized;
        IR::IRNode   ir { std::monostate {} };
        Value        exit_code;

        void reset()
        {
            file_name = L"";
            contents = L"";
            syntax = nullptr;
            normalized = nullptr;
            ir = std::monostate {};
        }
    };
    Context ctx;
    auto    arg_ix = parse_options(argc, argv);

    auto load_file = [&ctx](std::wstring_view fname) -> bool {
        ctx.contents = L"";
        ctx.file_name = L"";
        if (auto contents_maybe = read_file_by_name<wchar_t>(as_utf8(fname)); !contents_maybe.has_value()) {
            std::wcerr << L"Could not read file `" << fname << '`' << std::endl;
            return false;
        } else {
            ctx.contents = contents_maybe.value();
            ctx.file_name = fname;
            return true;
        }
    };

    auto parse_file = [&ctx](bool verbose = true) -> bool {
        if (verbose) {
            std::wcout << "STAGE 1 - Parsing" << std::endl;
        }
        ctx.parser.program = parse<Arwen::Program>(ctx.parser, "std.arw", load_std_lib().value_or(L""));
        ctx.parser.push_namespace(ctx.parser.program->ns);
        auto mod = parse<Arwen::Module>(ctx.parser, as_utf8(ctx.file_name), std::move(ctx.contents));
        for (auto const &err : ctx.parser.errors) {
            std::wcerr << err.location.line + 1 << ':' << err.location.column + 1 << " " << err.message << std::endl;
        }
        if (!mod) {
            std::cerr << "Syntax error(s) found" << std::endl;
            return false;
        }
        ctx.parser.program->modules[mod->name] = mod;
        ctx.parser.errors.clear();
        ctx.syntax = ctx.parser.program;

        if (verbose) {
            std::wcout << "STAGE 2 - Folding" << std::endl;
        }
        ctx.parser.namespaces.clear();
        ctx.parser.push_namespace(ctx.parser.program->ns);
        ctx.normalized = normalize_node(ctx.syntax, ctx.parser);
        if (!ctx.normalized) {
            std::cerr << "Internal error(s) encountered" << std::endl;
            ctx.reset();
            return false;
        }
        for (auto const &err : ctx.parser.errors) {
            std::wcerr << err.location.line << ':' << err.location.column << " " << err.message << std::endl;
            return false;
        }

        if (verbose) {
            std::wcout << "STAGE 3 - Binding. Pass ";
        }
        ctx.parser.pass = 0;
        ctx.parser.unbound = std::numeric_limits<int>::max();
        int prev_pass;
        do {
            if (verbose) {
                std::wcout << ctx.parser.pass << " ";
                std::flush(std::wcout);
            }
            prev_pass = ctx.parser.unbound;
            ctx.parser.unbound = 0;
            ctx.parser.unbound_nodes.clear();
            auto bound_type = bind_node(ctx.normalized, ctx.parser);
            if (!bound_type) {
                if (verbose) {
                    std::wcout << std::endl;
                }
                std::cerr << "Internal error(s) encountered" << std::endl;
                ctx.reset();
                return false;
            }
            if (!ctx.parser.errors.empty()) {
                if (verbose) {
                    std::wcout << std::endl;
                }
                for (auto const &err : ctx.parser.errors) {
                    std::wcerr << err.location.line << ':' << err.location.column << " " << err.message << std::endl;
                }
                return false;
            }
            ++ctx.parser.pass;
        } while (ctx.parser.unbound > 0 && ctx.parser.unbound < prev_pass);
        if (verbose) {
            std::cout << std::endl;
        }
        return true;
    };

    auto trace_program = [&ctx]() {
        enum class DebuggerState {
            Run,
            Step,
        };

        DebuggerState state { DebuggerState::Step };
        auto          cb = [&state](Interp::CallbackType cb_type, Interp &interpreter, Interp::CallbackPayload const &payload) -> bool {
            switch (cb_type) {
            case Interp::CallbackType::BeforeOperation:
                std::wcout << std::get<Operation>(payload) << std::endl;
                if (state == DebuggerState::Step) {
                    while (true) {
                        std::cout << "==> " << std::flush;
                        std::wstring ret;
                        std::getline(std::wcin, ret);
                        if (ret == L"s") {
                            break;
                        } else if (ret == L"c") {
                            state = DebuggerState::Run;
                            break;
                        }
                    }
                }
                // dump_scopes(interpreter);
                break;
            case Interp::CallbackType::AfterOperation:
                dump_scopes(interpreter);
                break;
            case Interp::CallbackType::StartModule:
                std::wcout << "Initializing module " << std::get<IR::pModule>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndModule:
                std::wcout << "Module " << std::get<IR::pModule>(payload)->name << " initialized" << std::endl;
                break;
            case Interp::CallbackType::StartFunction:
                std::wcout << "Entering function " << std::get<pFunction>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndFunction:
                std::wcout << "Leaving function " << std::get<pFunction>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::OnScopeStart:
                break;
            case Interp::CallbackType::AfterScopeStart:
                std::wcout << "Scope created" << std::endl;
                dump_scopes(interpreter);
                break;
            case Interp::CallbackType::OnScopeDrop:
                break;
            case Interp::CallbackType::AfterScopeDrop: {
                dump_scopes(interpreter);
            } break;
            }
            return true;
        };
        Interp interpreter;
        interpreter.callback = cb;
        auto ret = interpreter.execute(ctx.ir);
        std::wcout << ret << "\n";
        ctx.exit_code = ret;
    };

    setlocale(LC_ALL, "en_US.UTF-8");
    set_logging_config(LoggingConfig { has_option("trace") ? LogLevel::Trace : LogLevel::Info });
    auto quit { false };
    do {
        auto         from_argv { arg_ix < argc };
        std::wstring cmdline {};
        for (; arg_ix < argc; ++arg_ix) {
            cmdline += as_wstring(argv[arg_ix]);
            cmdline += ' ';
        }
        if (cmdline.empty()) {
            auto cmdline_maybe = get_command_string();
            if (!cmdline_maybe.has_value()) {
                break;
            }
            cmdline = *cmdline_maybe;
        }
        if (auto stripped = strip(std::wstring_view { cmdline }); !stripped.empty()) {
            auto parts = split(stripped, ' ');
            if (parts[0] == L"quit") {
                quit = true;
            } else if (parts[0] == L"load") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    load_file(parts[1]);
                }
            } else if (parts[0] == L"parse") {
                if (ctx.contents.empty()) {
                    std::cerr << "Error: no file loaded\n";
                } else {
                    parse_file();
                }
            } else if (parts[0] == L"build") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    load_file(parts[1]);
                    parse_file();
                    if (ctx.normalized == nullptr || ctx.normalized->status != SyntaxNode::Status::Bound) {
                        std::cerr << "Error: parse failed\n";
                    } else {
                        ctx.ir = IR::generate_ir(ctx.normalized);
                    }
                }
            } else if (parts[0] == L"print") {
                if (parts.size() != 2) {
                    std::cerr << "Error: name of tree to print missing\n";
                } else {
                    if (parts[1] == L"syntax") {
                        if (ctx.syntax == nullptr) {
                            std::cerr << "Error: no syntax tree available\n";
                        } else {
                            ctx.syntax->dump();
                        }
                    } else if (parts[1] == L"normalized") {
                        if (ctx.normalized == nullptr) {
                            std::cerr << "Error: no normalized tree available\n";
                        } else {
                            ctx.normalized->dump();
                        }
                    }
                }
            } else if (parts[0] == L"generate") {
                if (ctx.normalized == nullptr || ctx.normalized->status != SyntaxNode::Status::Bound) {
                    std::cerr << "Error: no bound tree available\n";
                } else {
                    ctx.ir = IR::generate_ir(ctx.normalized);
                }
            } else if (parts[0] == L"list") {
                if (std::holds_alternative<std::monostate>(ctx.ir)) {
                    std::cerr << "Error: no IR available\n";
                } else {
                    IR::list(ctx.ir);
                }
            } else if (parts[0] == L"run") {
                if (ctx.syntax == nullptr) {
                    if (parts.size() < 2) {
                        std::cerr << "Error: filename missing\n";
                    } else {
                        load_file(parts[1]);
                        parse_file(false);
                        if (ctx.normalized == nullptr || ctx.normalized->status != SyntaxNode::Status::Bound) {
                            std::cerr << "Error: parse failed\n";
                        } else {
                            ctx.ir = IR::generate_ir(ctx.normalized);
                        }
                    }
                }
                if (std::holds_alternative<IR::pProgram>(ctx.ir)) {
                    auto ret = Arwen::Interpreter::execute_ir(ctx.ir);
                    if (from_argv) {
                        return as<int32_t>(ret);
                    }
                    std::wcout << ret << "\n";
                    ctx.exit_code = ret;
                } else {
                    std::cerr << "Error: no IR available\n";
                }
            } else if (parts[0] == L"trace") {
                if (std::holds_alternative<IR::pProgram>(ctx.ir)) {
                    trace_program();
                } else {
                    std::cerr << "Error: no IR available\n";
                }
            } else {
                std::cout << "?\n";
            }
        }
    } while (!quit);
    return 0;
}

}

int main(int argc, char const **argv)
{
    return Arwen::debugger_main(argc, argv);
}
