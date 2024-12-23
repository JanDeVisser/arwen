/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <optional>
#include <ostream>
#include <string_view>
#include <variant>

#include <Logging.h>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundBreak

template<>
BoundNodeReference bind<Break>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Break>(ast_node.impl);
    auto        ref = add_node<BoundBreak>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.block = binder.bound_nodes.size();
    IMPL.block_is_loop = false;
    std::optional<std::string_view> label {};
    if (ast_impl.label) {
        label = std::get<Label>(binder.ast[*ast_impl.label].impl).label;
    }
    do {
        BoundNode const &p = binder[parent];
        auto block_label = I(BoundBlock, parent).label;
        if (std::holds_alternative<BoundBlock>(p.impl) && (!block_label || I(BoundBlock, parent).label == label)) {
            IMPL.block = parent;
            auto const &block_parent = binder[p.parent];
            if (std::holds_alternative<BoundFor>(block_parent.impl) ||
                std::holds_alternative<BoundLoop>(block_parent.impl) ||
                std::holds_alternative<BoundWhile>(block_parent.impl)) {
                IMPL.block = p.parent;
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
            return add_error(binder, ref, "'break' statement outside block");
        }
    }
    if (ast_impl.expression) {
        IMPL.expression = binder.bind_node(*ast_impl.expression, ref);
    }
    return ref;
}

template<>
BoundNodeReference rebind<BoundBreak>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.expression) {
        IMPL.expression = binder.rebind_node(*IMPL.expression);
        binder[ref].type = binder[*IMPL.expression].type;
    } else {
        binder[ref].type = VoidType;
    }
    if (binder[IMPL.block].type && binder[ref].type) {
        if (binder[IMPL.block].type != binder[ref].type) {
            return add_error(binder, ref, "Break expression type '{}' does not match block type '{}'", binder.registry[*binder[ref].type].name, binder.registry[*binder[IMPL.block].type].name);
        }
    }
    if (!binder[IMPL.block].type && binder[ref].type) {
        binder[IMPL.block].type = binder[ref].type;
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundBreak const &impl)
{
    out << '#' << impl.block;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundBreak const &impl, int indent)
{
    binder.dump(out, impl.expression, "expression", indent);
}

}
