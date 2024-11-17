//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <print>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Grammar/Grammar.h>
#include <Grammar/Parser.h>
#include <GrammarParser/GrammarParser.h>

#include <FileBuffer.h>
#include <Lib.h>
#include <Options.h>

namespace Arwen {

#include <arwen.grammar>

void arwen_main(int argc, char const **argv)
{
    AppDescription app_descr {
        .name { "arwen" },
        .shortdescr { "Yet another stupid programming language" },
        .description {"This is a language."},
        .legal { "Licensed under the MIT License" },
        .options {
            CmdLineOption { .shortopt = 0, .longopt = "grammar-dump", .description = "Dump the grammar", .flags = 0 },
        },
        .flags = APP_FLAG_MANY_ARG,
    };
    Application app {
        .descr = app_descr,
    };
    app.parse_cmdline(argc, argv);

    GrammarParser gp { arwen_grammar };
    Grammar       grammar {};
    gp.parse(grammar).must();
    if (app.options.contains("grammar-dump")) {
        grammar.dump();
    }
    Parser<ArwenParser> p { grammar };
    p.log = true;
    for (auto const file_name : app.args) {
        if (auto fb = FileBuffer::from_file(file_name); fb.is_error()) {
            std::println(stderr, "Error opening {}: {} ={}=", file_name, fb.error().to_string(), fs::current_path().string());
            exit(1);
        } else {
            std::println("{}", fb->contents());
            p.parse(fb->contents(), file_name).must();
            p.impl.dump();

            Binder binder { p.impl.node_cache };
            auto bound_program = binder.bind(p.impl.program).on_error(
                [&binder](auto) {
                    for (auto const& err : binder.errors) {
                        auto const &node = binder.bound_nodes[err];
                        std::println("{}: {}", node.location, std::get<BindError>(node.impl).message);
                    }
                    return binder.entrypoint;
                });
            binder.dump(*bound_program, "Program");
        }
    }
}

}

int main(int argc, char const **argv)
{
    Arwen::arwen_main(argc, argv);
    return 0;
}
