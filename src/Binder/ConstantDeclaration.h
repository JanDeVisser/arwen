/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Logging.h>
#include <print>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundConstantDeclaration

template<>
inline BoundNodeReference bind<ConstantDeclaration>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ConstantDeclaration>(ast_node.impl);
    auto        ref = add_node<BoundConstantDeclaration>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type, ref);
    }
    IMPL.initializer = binder.bind_node(ast_impl.initializer, ref);
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundConstantDeclaration>(Binder &binder, BoundNodeReference ref)
{
    IMPL.initializer = binder.rebind_node(IMPL.initializer);
    binder.set_name(IMPL.name, ref);
    if (!IMPL.type) {
        binder[ref].type = binder[IMPL.initializer].type;
    } else if (binder[IMPL.initializer].type) {
        auto left_type = binder.registry[*binder[*IMPL.type].type];
        auto right_types = binder.alternatives(IMPL.initializer, left_type.ref);
        if (right_types.preferred == left_type.ref) {
            binder[ref].type = binder[*IMPL.type].type;
            return ref;
        } else {
            for (auto type : right_types.alternatives) {
                if (type == left_type.ref) {
                    IMPL.initializer = binder.accept(IMPL.initializer, type);
                    binder[ref].type = binder[*IMPL.type].type;
                    return ref;
                }
            }
            std::println("left: {} ({}) right preferred: {} ({}) alternatives: {}", left_type, left_type.ref, binder.registry[right_types.preferred], right_types.preferred, right_types.alternatives);
            return add_error(binder, ref, "Cannot initialize constant of type '{}' with expression of type '{}'", left_type.name, binder.registry[*binder[IMPL.initializer].type].name);
        }
    }
    return ref;
}

template<>
inline void to_string(std::ostream &out, Binder &binder, BoundConstantDeclaration const &impl)
{
    out << impl.name;
    if (impl.type) {
        out << ": ";
        binder.to_string(out, *impl.type);
    }
}

template<>
inline void dump(std::ostream &out, Binder &binder, BoundConstantDeclaration const &impl, int indent)
{
    binder.dump(out, impl.type, "Type", indent);
    binder.dump(out, impl.initializer, "Initializer", indent);
}

}
