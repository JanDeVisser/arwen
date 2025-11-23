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
#include <iterator>
#include <locale.h>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/StringUtil.h>
#include <Util/TokenLocation.h>
#include <Util/Utf8.h>

#include <App/Config.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>
#include <system_error>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#ifdef IS_ARM64
#include <Arch/Arm64/Arm64.h>
#endif
#ifdef IS_X86_64
#include <Arch/Linux/x86_64/X86_64.h>
#endif

namespace Arwen {

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

std::optional<std::wstring> load_file(fs::path const &file)
{
    if (!fs::exists(file)) {
        std::cerr << "File `" << file << "` does not exist\n";
        return {};
    }
    auto contents_maybe = read_file_by_name<wchar_t>(file.string());
    if (!contents_maybe.has_value()) {
        std::cerr << "Could not read file `" << file << "`: " << contents_maybe.error().description << std::endl;
        return {};
    }
    return contents_maybe.value();
};

std::optional<std::wstring> load_file(std::wstring_view fname)
{
    return load_file(fs::path { fname });
}

std::optional<std::wstring> load_directory(fs::path directory)
{
    std::wstring ret;
    for (auto const &entry : fs::directory_iterator(directory)) {
        if (entry.is_regular_file() && entry.path().extension() == ".arw") {
            if (auto contents_maybe = load_file(entry.path()); !contents_maybe) {
                return {};
            } else {
                ret += contents_maybe.value();
            }
        }
    }
    return ret;
}

std::optional<std::wstring> load_files(std::vector<fs::path> const &files)
{
    std::wstring ret;
    for (auto const &file : files) {
        if (fs::is_regular_file(file)) {
            if (auto contents_maybe = load_file(file); !contents_maybe) {
                return {};
            } else {
                ret += contents_maybe.value();
            }
        }
    }
    return ret;
}

struct Builder {
    using Source = std::variant<fs::path, std::vector<fs::path>>;

    fs::path     arw_dir;
    Source       source;
    std::string  program_name;
    Parser       parser {};
    std::wstring source_text;
    bool         verbose { false };
    IR::IRNodes  ir {};

    static std::error_code error;

    bool build()
    {
        if (!parse()) {
            return false;
        }
        if (has_option("stop-after-parse")) {
            return true;
        }
        if (!normalize() || !bind()) {
            return false;
        }
        if (has_option("stop-after-bind")) {
            return true;
        }
        if (!generate_ir() /*|| !generate_code()*/) {
            return false;
        }
        if (has_option("list")) {
            list(ir, std::wcout);
        }
        return true;
    }

    bool parse()
    {
        auto std_lib = load_std_lib();
        if (!std_lib) {
            return false;
        }
        auto program = Arwen::parse<Arwen::Program>(parser, program_name, *std_lib);
        if (!program) {
            std::cerr << "Error(s) parsing builtins\n";
            return false;
        }
        if (auto source_text_maybe = std::visit(
                overloads {
                    [this](fs::path const &p) -> std::optional<std::wstring> {
                        if (fs::is_directory(p)) {
                            return load_directory(p);
                        } else if (fs::is_regular_file(p)) {
                            return load_file(p);
                        } else {
                            std::cerr << "File `" << p << "` is not a regular file or directory\n";
                            return {};
                        }
                    },
                    [this](std::vector<fs::path> const &paths) -> std::optional<std::wstring> {
                        return load_files(paths);
                    } },
                source);
            !source_text_maybe) {
            return false;
        } else {
            source_text = *source_text_maybe;
            auto mod = Arwen::parse<Arwen::Module>(parser, as_utf8(program_name), source_text);
            if (!parser.errors.empty()) {
                std::cerr << "Syntax error(s) found:\n";
                for (auto const &err : parser.errors) {
                    std::wcerr << err.location.line + 1 << ':' << err.location.column + 1 << " " << err.message << std::endl;
                }
                return false;
            }
            return true;
        }
    }

    bool normalize()
    {
        auto normalized = parser.program->normalize();
        if (!parser.errors.empty()) {
            std::cerr << "Error(s) found during normalization:\n";
            for (auto const &err : parser.errors) {
                std::wcerr << err.location.line + 1 << ':' << err.location.column + 1 << " " << err.message << std::endl;
            }
            return false;
        }
        assert(normalized);
        parser.program = normalized;
        return true;
    }

    bool bind()
    {
        parser.pass = 0;
        parser.unbound = std::numeric_limits<int>::max();
        int prev_pass;
        do {
            if (verbose) {
                std::wcout << parser.pass << " ";
                std::flush(std::wcout);
            }
            prev_pass = parser.unbound;
            parser.unbound = 0;
            parser.unbound_nodes.clear();
            auto bound_type = parser.program->bind();
            if (!bound_type) {
                if (verbose) {
                    std::wcout << std::endl;
                }
                std::cerr << "Internal error(s) encountered" << std::endl;
                return false;
            }
            if (!parser.errors.empty()) {
                if (verbose) {
                    std::wcout << std::endl;
                }
                std::cerr << "Error(s) found during binding:\n";
                for (auto const &err : parser.errors) {
                    std::wcerr << err.location.line << ':' << err.location.column << " " << err.message << std::endl;
                }
                return false;
            }
            ++parser.pass;
        } while (parser.unbound > 0 && parser.unbound < prev_pass);
        if (verbose) {
            std::cout << std::endl;
        }
        return true;
    }

    bool generate_ir()
    {
        IR::generate_ir(parser.program, ir);
        return true;
    }

    bool generate_code()
    {
#ifdef IS_ARM64
        MUST(Arm64::generate_arm64(ir));
#endif
#ifdef IS_X86_64
        MUST(X86_64::generate_x86_64(ir));
#endif
        return true;
    }

private:
    Builder()
        : Builder(fs::current_path())
    {
    }

    Builder(fs::path root)
        : arw_dir(arwen_dir())
        , source(std::move(root))
        , program_name(root.stem().string())
        , verbose(log_config.level == LogLevel::Trace)
    {
    }

    Builder(std::vector<fs::path> source)
        : arw_dir(arwen_dir())
        , source(std::move(source))
        , program_name(({ auto const &__s = std::get<1>(this->source); assert(!__s.empty()); __s[0].stem().string(); }))
        , verbose(log_config.level == LogLevel::Trace)
    {
    }

    std::optional<std::wstring> load_std_lib()
    {
        return load_file(fs::path { arwen_dir() / "share" / "std.arw" });
    }

    template<typename T>
        requires std::is_same_v<T, fs::path> || std::is_same_v<T, std::vector<fs::path>>
    friend std::optional<Builder> make_builder(T);
};

template<typename T>
    requires std::is_same_v<T, fs::path> || std::is_same_v<T, std::vector<fs::path>>
std::optional<Builder> make_builder(T source)
{
    return Builder { source };
}

int debugger_main()
{
    std::cerr << "Debugger has been disabled\n\n";
    return 0;
#if 0    
    struct Context {
        std::wstring file_name;
        std::wstring contents;
        Parser       parser;
        ASTNode      syntax;
        ASTNode      normalized;
        IR::IRNodes  ir {};
        Value        exit_code;

        void reset()
        {
            file_name = L"";
            contents = L"";
            syntax = nullptr;
            normalized = nullptr;
            ir = {};
        }
    };
    Context ctx;

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
                std::wcout << "Initializing module " << std::get<IR::pIR>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndModule:
                std::wcout << "Module " << std::get<IR::pIR>(payload)->name << " initialized" << std::endl;
                break;
            case Interp::CallbackType::StartFunction:
                std::wcout << "Entering function " << std::get<pIR>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndFunction:
                std::wcout << "Leaving function " << std::get<pIR>(payload)->name << std::endl;
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
        auto ret = interpreter.execute(ctx.ir.program);
        std::wcout << ret << "\n";
        ctx.exit_code = ret;
    };

    auto quit { false };
    do {
        auto cmdline_maybe = get_command_string();
        if (!cmdline_maybe.has_value()) {
            break;
        }
        auto cmdline = *cmdline_maybe;
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
                    ctx.file_name = parts[1];
                    if (auto contents_maybe = load_file(ctx.file_name); contents_maybe) {
                        ctx.contents = contents_maybe.value();
                        ctx.contents = parse_file(load_file(ctx.file_name));
                        if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                            std::cerr << "Error: parse failed\n";
                        } else {
                            ctx.ir = IR::generate_ir(ctx.normalized);
                        }
                    }
                }
            } else if (parts[0] == L"cat") {
                std::wcout << ctx.contents << std::endl;
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
                if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                    std::cerr << "Error: no bound tree available\n";
                } else {
                    ctx.ir = IR::generate_ir(ctx.normalized);
                }
            } else if (parts[0] == L"list") {
                IR::list(ctx.ir, std::wcout);
            } else if (parts[0] == L"run") {
                if (ctx.syntax == nullptr) {
                    if (parts.size() < 2) {
                        std::cerr << "Error: filename missing\n";
                    } else {
                        load_file(parts[1]);
                        parse_file(false);
                        if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                            std::cerr << "Error: parse failed\n";
                        } else {
                            ctx.ir = IR::generate_ir(ctx.normalized);
                        }
                    }
                }
                auto ret = Arwen::Interpreter::execute_ir(ctx.ir);
                std::wcout << ret << "\n";
                ctx.exit_code = ret;
            } else if (parts[0] == L"trace") {
                trace_program();
            } else if (parts[0] == L"compile") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    load_file(parts[1]);
                    parse_file();
                    if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                        std::cerr << "Error: parse failed\n";
                    } else {
                        ctx.ir = IR::generate_ir(ctx.normalized);
#ifdef IS_ARM64
                        MUST(Arm64::generate_arm64(ctx.ir));
#endif
#ifdef IS_X86_64
                        MUST(X86_64::generate_x86_64(ctx.ir));
#endif
                    }
                }
            } else {
                std::cout << "?\n";
            }
        }
    } while (!quit);
    return 0;
#endif
}

void usage()
{
    std::cout << "arwen - Arwen language compiler\n\n";
    std::cout << "Usage:\n";
    std::cout << "   arwen --help - This text\n";
    std::cout << "   arwen [--trace] [--list] build\n";
    std::cout << "   arwen [--trace] [--list] compile <file> ...\n";
    std::cout << "   arwen [--trace] [--list] run <file> ... [-- <arg> ...]\n";
    std::cout << "   arwen [--trace] [--list] eval <file> ... [-- <arg> ...]\n";
    std::cout << "   arwen [--trace] [--list] - Start debugger\n\n";
    exit(1);
}

int main(int argc, char const **argv)
{
    setlocale(LC_ALL, "en_US.UTF-8");
    auto arg_ix = parse_options(argc, argv);
    set_logging_config(LoggingConfig { has_option("trace") ? LogLevel::Trace : LogLevel::Info });
    if (has_option("help") || has_option("usage")) {
        usage();
    }
    if (arg_ix == argc) {
        return debugger_main();
    }
    if (strcmp(argv[arg_ix], "build") == 0) {
        if (auto builder_maybe = make_builder(fs::current_path()); builder_maybe) {
            if (!builder_maybe->build()) {
                return 1;
            }
        }
    } else if (strcmp(argv[arg_ix], "compile") == 0 && argc - arg_ix > 1) {
        std::vector<fs::path> files;
        for (size_t ix = arg_ix + 1; ix < argc; ++ix) {
            files.emplace_back(argv[ix]);
        }
        if (auto builder_maybe = make_builder(files); builder_maybe) {
            if (!builder_maybe->build()) {
                return 1;
            }
        }
    } else {
        usage();
    }
    return 0;
}

}

int main(int argc, char const **argv)
{
    return Arwen::main(argc, argv);
}
