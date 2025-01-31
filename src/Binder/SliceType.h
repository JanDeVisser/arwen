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
#define STRUCT BoundSliceType

template<>
BoundNodeReference bind<SliceType>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ArrayType>(ast_node.impl);
    auto        ref = add_node<BoundArrayType>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.element_type = binder.bind_node(ast_impl.element_type, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundSliceType>(Binder &binder, BoundNodeReference ref)
{
    IMPL.element_type = binder.rebind_node(IMPL.element_type);
    if (binder[IMPL.element_type].type) {
        binder[ref].type = binder.registry.resolve_slice(*binder[IMPL.element_type].type);
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundSliceType const &impl, int indent)
{
    binder.dump(out, impl.element_type, "Element Type", indent);
}

}
