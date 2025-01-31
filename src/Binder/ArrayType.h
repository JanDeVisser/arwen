/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <optional>
#include <ostream>
#include <string_view>

#include <Logging.h>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundArrayType

template<>
BoundNodeReference bind<ArrayType>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ArrayType>(ast_node.impl);
    auto        ref = add_node<BoundArrayType>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.element_type = binder.bind_node(ast_impl.element_type, ref);
    IMPL.size = binder.bind_node(ast_impl.size, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundArrayType>(Binder &binder, BoundNodeReference ref)
{
    IMPL.element_type = binder.rebind_node(IMPL.element_type);
    if (binder[IMPL.element_type].type) {
        binder[ref].type = binder.registry.resolve_array(*binder[IMPL.element_type].type);
    }
    IMPL.size = binder.rebind_node(IMPL.size);
    if (binder[IMPL.size].type) {
        auto size_types = binder.alternatives(IMPL.size, U64Type);
        if (binder.registry[size_types.preferred].is_signed()) {
            return ref;
        } else {
            for (auto type : size_types.alternatives) {
                if (binder.registry[type].is_signed()) {
                    IMPL.size = binder.accept(IMPL.size, type);
                    return ref;
                }
            }
            return add_error(binder, ref, "Array size must be an signed integral type");
        }
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundArrayType const &impl, int indent)
{
    binder.dump(out, impl.element_type, "Element Type", indent);
    binder.dump(out, impl.size, "Size", indent);
}

}
