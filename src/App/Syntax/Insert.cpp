/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>

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
    : script_text(script_text)
{
}

ASTNode Insert::normalize(ASTNode const &n)
{
    auto script = parse<Block>(*(n.repo), "*Insert*", script_text);
    if (script) {
        std::cout << "Parsed compile time script\n\n";
        script->dump();
        script = script->normalize();
        while (script->bound_type == nullptr || script->bound_type == TypeRegistry::undetermined) {
            if (script->bind() == nullptr) {
                return nullptr;
            }
        }
        std::cout << "Running compile time script\n\n";
        script->dump();

        IRNodes script_ir {};
        IR::generate_ir(script, script_ir);
        auto       output_val = execute_ir(script_ir);
        auto const output_dynarr = as<DynamicArray>(output_val);
        auto const output = std::wstring { static_cast<wchar_t *>(output_dynarr.ptr), static_cast<size_t>(output_dynarr.size) };

        if (auto node = parse<Block>(*(n.repo), "*Insert Eval*", output); node) {
            std::cerr << "@insert after parsing\n";
            node->location = n->location;
            node->dump();
            node = node->normalize();
            std::cerr << "@insert after normalizing\n";
            node->dump();
            return node;
        }
    }
    return nullptr;
}

}
