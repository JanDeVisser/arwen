/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <filesystem>
#include <iostream>
#include <locale.h>
#include <optional>
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

extern int debugger_main();

using namespace Util;
using namespace Interpreter;
using namespace Arwen::IR;

using Interp = Arwen::Interpreter::Interpreter;

std::optional<std::wstring> load_file(fs::path const &file)
{
    if (!fs::exists(file)) {
        log_error("File `{}` does not exist", file.string());
        return {};
    }
    auto contents_maybe = read_file_by_name<wchar_t>(file.string());
    if (!contents_maybe.has_value()) {
        log_error("Could not read file `{}`: {}", file.string(), contents_maybe.error().description);
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

    bool gen_ir()
    {
        if (!parse()) {
            std::cerr << "Syntactic parsing failed\n";
            return false;
        }
        if (has_option("stop-after-parse")) {
            return true;
        }
        if (!normalize() || !bind()) {
            log_error("Semantic analysis failed");
            return false;
        }
        if (has_option("stop-after-analysis")) {
            return true;
        }
        if (!generate_ir()) {
            log_error("Intermediate representation generation failed");
            return false;
        }
        if (has_option("list")) {
            save(ir);
        }
        return true;
    }

    bool build()
    {
        if (!gen_ir()) {
            return false;
        }
        if (!generate_code()) {
            log_error("Code generation failed");
            return false;
        }
        return true;
    }

    bool eval()
    {
        if (!gen_ir()) {
            return false;
        }
        if (!generate_code()) {
            log_error("Code generation failed");
            return false;
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
            log_error("Error(s) parsing builtins");
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
                            log_error("File `{}` is not a regular file or directory", p.string());
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
                log_error("Syntax error(s) found:");
                for (auto const &err : parser.errors) {
                    log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
                }
                return false;
            }
            info("Syntactic parsing succeeded", parser.pass);
            return true;
        }
    }

    bool normalize()
    {
        auto normalized = Arwen::normalize(parser.program);
        if (!parser.errors.empty()) {
            log_error("Error(s) found during first phase of sematic analysis:");
            for (auto const &err : parser.errors) {
                log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
            }
            return false;
        }
        assert(normalized);
        parser.program = normalized;
        info("First phase of sematic analysis succeeded");
        return true;
    }

    bool bind()
    {
        parser.pass = 0;
        parser.unbound = std::numeric_limits<int>::max();
        int prev_pass;
        do {
            prev_pass = parser.unbound;
            parser.unbound = 0;
            parser.unbound_nodes.clear();
            Arwen::bind(parser.program);
            ++parser.pass;
        } while (parser.program->status != ASTStatus::Bound && parser.unbound < prev_pass);
        if (parser.program->status != ASTStatus::Bound) {
            info("Second phase of semantic analysis failed after {} pass(es)", parser.pass);
            if (!parser.errors.empty()) {
                log_error("Error(s) found during second phase of semantic analysis:");
                for (auto const &err : parser.errors) {
                    log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
                }
            } else if (parser.program->status != ASTStatus::Undetermined) {
                log_error("Internal error(s) encountered during semantic analysis");
            }
            if (!parser.unbound_nodes.empty()) {
                info("The following nodes could not be bound:");
                for (auto const &n : parser.unbound_nodes) {
                    info(L"{}", n);
                }
            }
            return false;
        }
        info("Second phase of semantic analysis succeeded after {} pass(es)", parser.pass);
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
        return true;
#endif
#ifdef IS_X86_64
        return X86_64::generate_x86_64(ir);
#endif
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

void usage()
{
    std::cout << "arwen - arwen language compiler  https://www.arwen-lang.org\n\n";
    std::cout << "Usage:\n";
    std::cout << "   arwen --help - This text\n";
    std::cout << "   arwen [OPTIONS] build\n";
    std::cout << "   arwen [OPTIONS] compile <file> ...\n";
    std::cout << "   arwen [OPTIONS] run <file> ... [-- <arg> ...]\n";
    std::cout << "   arwen [OPTIONS] eval <file> ... [-- <arg> ...]\n";
    std::cout << "   arwen [OPTIONS] debug - Start debugger\n\n";
    std::cout << "Options:\n";
    std::cout << "  --keep-assembly       Do not delete assembly files after compiling\n";
    std::cout << "  --keep-objects        Do not delete object files after linking\n";
    std::cout << "  --list                Write intermediate representation to .arwen/<program>.ir\n";
    std::cout << "  --stop-after-analysis Stop after semantic analysis\n";
    std::cout << "  --stop-after-parse    Stop after syntactic parsing\n";
    std::cout << "  --trace               Print debug tracing information\n";
    std::cout << "  --verbose             Provide progress information\n";
    std::cout << "\n";
    exit(1);
}

int main(int argc, char const **argv)
{
    setlocale(LC_ALL, "en_US.UTF-8");
    auto arg_ix = parse_options(argc, argv);
    set_logging_config(LoggingConfig { has_option("trace") ? LogLevel::Trace : (has_option("verbose") ? LogLevel::Info : LogLevel::Error) });
    trace("Tracing is ON");
    info("Verbose mode is ON");
    if (arg_ix == argc || has_option("help") || has_option("usage")) {
        usage();
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
    } else if (strcmp(argv[arg_ix], "eval") == 0 && argc - arg_ix > 1) {
        std::vector<fs::path> files;
        for (size_t ix = arg_ix + 1; ix < argc; ++ix) {
            files.emplace_back(argv[ix]);
        }
        if (auto builder_maybe = make_builder(files); builder_maybe) {
            if (!builder_maybe->eval()) {
                return 1;
            }
        }
    } else if (strcmp(argv[arg_ix], "debug") == 0 && argc - arg_ix == 1) {
        return debugger_main();
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
