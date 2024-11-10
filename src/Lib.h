/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <any>
#include <format>
#include <map>
#include <optional>
#include <print>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>

#include <Logging.h>
#include <Result.h>

namespace Arwen {

static uint8_t const CMDLINE_OPTION_FLAG_OPTIONAL_ARG = 0x0001;
static uint8_t const CMDLINE_OPTION_FLAG_REQUIRED_ARG = 0x0002;
static uint8_t const CMDLINE_OPTION_FLAG_MANY_ARG = 0x0004;
static uint8_t const CMDLINE_OPTION_FLAG_ALLOWS_ARG = 0x0007;

static uint8_t const APP_FLAG_OPTIONAL_ARG = 0x0001;
static uint8_t const APP_FLAG_ARG_REQUIRED = 0x0002;
static uint8_t const APP_FLAG_MANY_ARG = 0x0004;
static uint8_t const APP_FLAG_ALLOWS_ARG = 0x0007;

struct CmdLineOption {
    char             shortopt;
    std::string_view longopt;
    std::string_view description;
    int              flags;
};

struct AppDescription {
    std::string_view           name;
    std::string_view           shortdescr;
    std::string_view           description;
    std::string_view           legal;
    std::vector<CmdLineOption> options;
    int                        flags;
};

struct Application {
    AppDescription                                    descr;
    int                                               argc;
    std::vector<std::string_view>                     argv;
    std::string_view                                  executable;
    std::multimap<std::string_view, std::string_view> options;
    std::vector<std::string_view>                     args;

    [[noreturn]] void help()
    {
        if (!descr.name.empty()) {
            std::print(stderr, "{}", descr.name);
            if (!descr.shortdescr.empty()) {
                std::print(stderr, " - {}", descr.shortdescr);
            }
            std::println(stderr, "\n");
        } else {
            std::println(stderr, "{}\n", executable);
        }
        if (!descr.description.empty()) {
            std::println(stderr, "{}\n", descr.description);
        }
        if (!descr.legal.empty()) {
            std::println(stderr, "{}\n", descr.legal);
        }
        for (auto const &opt : descr.options) {
            std::print(stderr, "\t--{}", opt.longopt);
            if (opt.shortopt != 0) {
                std::println(stderr, ", -{}", opt.shortopt);
            }
            if (!opt.description.empty()) {
                std::println(stderr, "\t{}", opt.description);
            }
            std::println(stderr, "\n");
        }
        std::println("\t--help, -h\tDisplay this message");
        exit(1);
    }

    [[noreturn]] void report_error(std::string_view const &error)
    {
        std::println(stderr, "ERROR: {}\n", error);
        help();
    }

    CmdLineOption const &find_longopt(std::string_view opt)
    {
        for (auto const &optdef : descr.options) {
            if (optdef.longopt == opt) {
                return optdef;
            }
        }
        report_error(std::format("Unrecognized long option '{}'", opt));
    }

    CmdLineOption const &find_shortopt(char opt)
    {
        for (auto const &optdef : descr.options) {
            if (optdef.shortopt == opt) {
                return optdef;
            }
        }
        report_error(std::format("Unrecognized short option '-{}'", opt));
    }

    int parse_option(CmdLineOption const &opt, int ix)
    {
        auto arg = argv[ix];
        if ((ix == argc - 1) ||                              /* Option is last arg                 */
            (arg.length() > 2 && arg[1] != '-') ||           /* Arg is a sequence of short options */
            (argv[ix + 1][0] == '-') ||                      /* Next arg is an option              */
            !(opt.flags & CMDLINE_OPTION_FLAG_ALLOWS_ARG)) { /* Option doesn't allow args          */
            options.emplace(opt.longopt, "true");
            return ix;
        }

        if (opt.flags & CMDLINE_OPTION_FLAG_REQUIRED_ARG && (ix == argc - 1 || argv[ix + 1][0] == '-')) {
            report_error(std::format("Option '--{}' requires an argument", opt.longopt));
        }

        if (opt.flags & CMDLINE_OPTION_FLAG_MANY_ARG) {
            for (++ix; ix < argc && argv[ix][0] != '-'; ix++) {
                options.emplace(opt.longopt, argv[ix]);
            }
            return ix - 1;
        } else {
            options.emplace(opt.longopt, argv[ix + 1]);
            return ix + 1;
        }
    }

    void parse_cmdline(int argc_, char const **argv_)
    {
        argc = argc_;
        for (auto ix = 0; ix < argc; ix++) {
            argv.emplace_back(argv_[ix]);
        }

        executable = argv[0];
        int ix { 1 };
        for (; ix < argc; ++ix) {
            auto arg = argv[ix];
            if (arg == "--help") {
                help();
            } else if (arg.length() > 1 && arg[0] == '-') {
                if (arg.length() > 2 && arg[1] == '-') {
                    auto opt = find_longopt(arg.substr(2));
                    ix = parse_option(opt, ix);
                } else if (arg.length() == 2 && arg[1] == '-') {
                    ix++;
                    break;
                } else {
                    for (auto ixx = 1; ixx < arg.length(); ++ixx) {
                        auto const &opt = find_shortopt(arg[ixx]);
                        if (arg.length() > 2 && opt.flags & CMDLINE_OPTION_FLAG_REQUIRED_ARG) {
                            report_error(std::format("Short option '-%c' requires an argument", arg[ixx]));
                        }
                        ix = parse_option(opt, ix);
                    }
                }
            } else {
                break;
            }
        }

        for (; ix < argc; ++ix) {
            args.emplace_back(argv[ix]);
        }

        if (descr.flags & APP_FLAG_ARG_REQUIRED && args.empty()) {
            report_error("No argument(s) provided");
        }
        if (!(descr.flags & APP_FLAG_MANY_ARG) && args.size() > 1) {
            report_error("Only one argument allowed");
        }
    }
};

inline bool ichar_equals(char a, char b)
{
    return std::tolower(static_cast<unsigned char>(a)) == std::tolower(static_cast<unsigned char>(b));
}

inline bool iequals(std::string_view const a, std::string_view const b)
{
    return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin(), ichar_equals);
}

inline std::string_view trim(std::string_view const &s)
{
    if (s.empty())
        return s;
    auto start = 0;
    while (start < s.length() && isspace(s[start]))
        ++start;
    if (start == s.length())
        return {};

    auto end = s.length() - 1;
    while (static_cast<ssize_t>(end) >= 0 && isspace(s[end]))
        --end;
    return s.substr(start, end + 1);
}

template<typename T>
inline std::optional<T> decode(std::string_view s, ...)
{
    UNREACHABLE();
}

template<typename T>
inline constexpr std::string_view to_string(T const &)
{
    return "";
}

struct SimpleFormatParser {
    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end())
            return it;
        if (*it != '}')
            throw std::format_error("Invalid format args for SymbolType.");
        return it;
    }
};

}
