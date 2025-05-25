/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/SyntaxNode.h"
#include "Util/TokenLocation.h"
#include "Util/Utf8.h"
#include <iostream>
#include <limits>
#include <ostream>
#include <string_view>

#include <App/Parser.h>
#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Result.h>

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
    Parser parser;
    parser.program = make_node<Program>(TokenLocation {}, as_wstring(file_name), std::make_shared<Namespace>(parser.root));
    if (auto contents_maybe = read_file_by_name<wchar_t>(file_name); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        std::wcout << "STAGE 1 - Parsing" << std::endl;
        parser.namespaces.clear();
        parser.push_namespace(parser.root);
        parser.push_namespace(parser.program->ns);
        auto mod = parser.parse_module(file_name, std::move(contents));
        for (auto const &err : parser.errors) {
            std::wcerr << err.location.line + 1 << ':' << err.location.column + 1 << " " << err.message << std::endl;
        }
        if (!mod) {
            std::cerr << "Syntax error(s) found" << std::endl;
            return;
        }
        parser.program->modules[mod->name] = mod;
        parser.errors.clear();

        std::wcout << "STAGE 2 - Folding" << std::endl;
        parser.namespaces.clear();
        parser.push_namespace(parser.root);
        parser.push_namespace(parser.program->ns);
        auto normalized = parser.program->normalize(parser);
        if (!normalized) {
            std::cerr << "Internal error(s) encountered" << std::endl;
            return;
        }
        for (auto const &err : parser.errors) {
            std::wcerr << err.location.line << ':' << err.location.column << " " << err.message << std::endl;
            return;
        }
        normalized->dump();

        std::wcout << "STAGE 3 - Binding. Pass ";
        parser.pass = 0;
        parser.unbound  = std::numeric_limits<int>::max();
        int prev_pass;
        do {
            std::wcout << parser.pass << " ";
            std::flush(std::wcout);
            prev_pass = parser.unbound;
            parser.unbound = 0;
            parser.unbound_nodes.clear();
            auto bound_type = bind_node(normalized, parser);
            if (!bound_type) {
                std::wcout << std::endl;
                std::cerr << "Internal error(s) encountered" << std::endl;
                return;
            }
            if (!parser.errors.empty()) {
                std::wcout << std::endl;
                for (auto const &err : parser.errors) {
                    std::wcerr << err.location.line << ':' << err.location.column << " " << err.message << std::endl;
                    return;
                }
            }
            ++parser.pass;
        } while (parser.unbound > 0 && parser.unbound < prev_pass);
        std::wcout << std::endl;
        if (!parser.unbound_nodes.empty()) {
            std::wcout << std::endl;
            for (auto const &node : parser.unbound_nodes) {
                std::wcerr << node->location.line << ':' << node->location.column << " ";
                node->header_line(std::wcerr);
                std::wcerr << std::endl;
                return;
            }
        }
        normalized->dump();
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
