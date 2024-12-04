//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <_string.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <print>
#include <string_view>
#include <utility>
#include <vector>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Grammar/Grammar.h>
#include <Grammar/Parser.h>
#include <GrammarParser/GrammarParser.h>
#include <IR/Execute.h>
#include <IR/IR.h>

#include <FileBuffer.h>
#include <Lib.h>
#include <Options.h>
#include <Result.h>
#include <Unescape.h>

extern "C" {
void arwen$write();

void unused_make_sure_rt_is_linked()
{
    arwen$write();
}
}

namespace Arwen {

using namespace Arwen::IR;

#include "arwen.grammar"
#include "builtin.arw"

void arwen_main(int argc, char const **argv)
{
    AppDescription app_descr {
        .name { "arwen" },
        .shortdescr { "Yet another stupid programming language" },
        .description { "This is a language." },
        .legal { "Licensed under the MIT License" },
        .options {
            CmdLineOption { .shortopt = 0, .longopt = "grammar-dump", .description = "Dump the grammar", .flags = 0 },
            CmdLineOption { .shortopt = 'l', .longopt = "log", .description = "Turn on logging", .flags = 0 },
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
    bool                log = app.options.contains("log");
    Parser<ArwenParser> p { grammar };
    p.log = log;

    auto builtin_buffer = strdup(arwen_builtin);
    unescape(builtin_buffer, strlen(arwen_builtin));
    p.parse(builtin_buffer, "#builtin").must();
    if (log) {
        p.impl.dump();
    }

    std::vector<FileBuffer> buffers;
    auto parse_file = [&buffers,&p,log](std::string_view const &file_name) {
        auto unescapify = [](char *buffer, size_t size) -> size_t {
            return unescape(buffer, size);
        };
        if (auto fb = FileBuffer::from_file_filter(file_name, unescapify); fb.is_error()) {
            std::println(stderr, "Error opening {}: {} ={}=", file_name, fb.error().to_string(), fs::current_path().string());
            exit(1);
        } else {
            buffers.emplace_back(std::move(fb.value()));
            p.parse(buffers.back().contents(), file_name).must();
            if (log) {
                p.impl.dump();
            }
        }

    };

    for (auto const file_name : app.args) {
        parse_file(file_name);
    }

    Binder binder { p.impl.node_cache };
    binder.log = log;
    auto               bound_program = binder.bind(p.impl.program).must([&binder](auto const &e) {
        for (auto const &err : binder.errors) {
            auto const &node = binder.bound_nodes[err];
            std::println("{}: {}", node.location, std::get<BindError>(node.impl).message);
        }
        return binder.entrypoint;
    });

    Arwen::IR::Program ir { binder };
    ir.generate().must();
    if (log) {
        ir.list();
    }

    Machine machine { ir };
    machine.log = log;
    auto v = machine.run();
    if (v) {
        std::println("Result: {}", *v);
    }
}

}

int main(int argc, char const **argv)
{
    Arwen::arwen_main(argc, argv);
    return 0;
}
