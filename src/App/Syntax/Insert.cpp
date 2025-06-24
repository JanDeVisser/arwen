/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <memory>

#include <Util/IO.h>
#include <Util/Logging.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>

namespace Arwen {

using namespace Util;
using namespace Arwen::Interpreter;

Insert::Insert(std::wstring_view script_text)
    : SyntaxNode(SyntaxNodeType::Insert)
    , script_text(script_text)
{
}

pSyntaxNode Insert::normalize(Parser &parser)
{
    Parser insert_parser;
    auto   script = insert_parser.parse_script(script_text);
    if (script) {
        script->ns->parent = parser.namespaces.back();
        std::cout << "Parsed compile time script\n\n";
        script->dump();
        script = normalize_node(script, insert_parser);
        insert_parser.pass = 0;
        while (script->bound_type == nullptr || script->bound_type == TypeRegistry::undetermined) {
            bind_node(script, insert_parser);
            if (!insert_parser.errors.empty()) {
                for (auto &err : insert_parser.errors) {
                    parser.errors.push_back(err);
                }
                return nullptr;
            }
            ++insert_parser.pass;
        }
        std::cout << "Running compile time script\n\n";
        script->dump();

        auto        script_ir = IR::generate_ir(script);
        auto        output_val = execute_ir(script_ir);
        auto const  output_dynarr = as<DynamicArray>(output_val);
        auto const  output = std::wstring { static_cast<wchar_t *>(output_dynarr.ptr), static_cast<size_t>(output_dynarr.size) };

        // trace(L"@insert({})", output);
        Parser include_parser;
        include_parser.level = parser.level;
        if (auto node = include_parser.parse_script(output); node) {
            std::cerr << "@insert after parsing\n";
            node->dump();
            node = normalize_node(node, parser);
            std::cerr << "@insert after normalizing\n";
            node->dump();
            if (node) {
                node->location = location;
            }
            return node;
        }
        for (auto &err : include_parser.errors) {
            parser.errors.push_back(err);
        }
    }
    return nullptr;
}

pType Insert::bind(Parser &parser)
{
    return parser.bind_error(location, L"`@insert` statement should have been elided");
}

}
