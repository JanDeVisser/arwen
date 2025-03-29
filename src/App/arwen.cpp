/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <string_view>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Result.h>
#include <App/Parser.h>

namespace Arwen {

void usage()
{
    std::cerr << "arwen -" << std::endl;
}

[[noreturn]] void usage(std::string_view msg)
{
    usage();
    std::cerr << std::endl
              << msg << std::endl;
    exit(1);
}

void compile_file(std::string_view file_name)
{
    if (auto contents_maybe = read_file_by_name<wchar_t>(file_name); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        Parser parser;
        auto   mod = parser.parse_module(file_name,contents);
        if (mod) {
            mod->dump();
            return;
        }
        std::cerr << "Syntax error" << std::endl;
    } else {
        std::cerr << "Could not open '" << file_name << "': " << contents_maybe.error().to_string() << std::endl;
    }
}

int main(int argc, char const **argv)
{
    int cmd_ix = parse_options(argc, argv);
    if (cmd_ix >= argc) {
        usage("No command specified");
    }
    if (!strcmp(argv[cmd_ix], "compile")) {
        if (cmd_ix >= argc - 1) {
            usage("arwen compile <main file name>");
        }
        compile_file(argv[cmd_ix + 1]);
    } else {
        usage(std::format("Invalid command '{}'", argv[cmd_ix]));
    }
    return 0;
}

} // namespace Arwen

int main(int argc, char const **argv)
{
    return Arwen::main(argc, argv);
}
