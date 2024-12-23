/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <optional>
#include <ostream>
#include <string_view>
#include <variant>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Logging.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundContinue

template<>
BoundNodeReference bind<Continue>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Continue>(ast_node.impl);
    auto        ref = add_node<BoundContinue>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.block = binder.bound_nodes.size();
    std::optional<std::string_view> label {};
    if (ast_impl.label) {
        label = std::get<Label>(binder.ast[*ast_impl.label].impl).label;
    }
    do {
        BoundNode const &p = binder[parent];
        if (std::holds_alternative<BoundBlock>(p.impl) && I(BoundBlock, parent).label == label) {
            IMPL.block = parent;
            auto const &block_parent = binder[p.parent];
            if (!std::holds_alternative<BoundFor>(block_parent.impl) &&
                !std::holds_alternative<BoundLoop>(block_parent.impl) &&
                !std::holds_alternative<BoundWhile>(block_parent.impl)) {
                return add_error(binder, ref, "'continue' statement outside loop");
            }
            break;
        }
        if (std::holds_alternative<BoundFunctionImplementation>(p.impl)) {
            break;
        }
        parent = p.parent;
    } while (parent);
    if (IMPL.block >= binder.bound_nodes.size()) {
        if (ast_impl.label) {
            return add_error(binder, ref, "Unknown label '{}'", *ast_impl.label);
        } else {
            return add_error(binder, ref, "'continue' statement outside block");
        }
    }
    binder[ref].type = VoidType;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundContinue const &impl)
{
    out << '#' << impl.block;
}

}
