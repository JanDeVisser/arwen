/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>

#include <Logging.h>
#include <ScopeGuard.h>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundFor

template<>
inline BoundNodeReference bind<For>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<For>(ast_node.impl);
    auto const &ast_ident = std::get<Identifier>(binder.ast[ast_impl.identifier].impl);
    auto        ref = add_node<BoundFor>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.range = binder.bind_node(ast_impl.range, ref);
    auto var_decl_ref = add_node<BoundVariableDeclaration>(binder, ast_impl.identifier, ast_node.location, ref);
    auto &var_decl = I(BoundVariableDeclaration, var_decl_ref);
    var_decl.name = ast_ident.text;
    var_decl.initializer = I(BoundRange, IMPL.range).begin;

    IMPL.variable_decl = var_decl_ref;
    IMPL.body = binder.bind_node(ast_impl.body, ref);
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundFor>(Binder &binder, BoundNodeReference ref)
{
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };
    IMPL.range = binder.rebind_node(IMPL.range);
    auto const& var_decl = binder[IMPL.variable_decl];
    binder.set_name(I(BoundVariableDeclaration, IMPL.variable_decl).name, IMPL.variable_decl);
    if (binder[IMPL.range].type) {
        auto const& range_type = binder.registry[*binder[IMPL.range].type];
        if (range_type.typespec.tag() == TypeKind::Range) {
            auto const& range = range_type.typespec.get<TypeKind::Range>();
            binder[IMPL.variable_decl].type = range.range_type;
        } else {
            return add_error(binder, ref, "For loop range must be a range, got '{}'", range_type.name);
        }
    }
    IMPL.body = binder.rebind_node(IMPL.body);
    if (binder[IMPL.variable_decl].type && binder[IMPL.body].type) {
        binder[ref].type = binder[IMPL.body].type;
    }
    return ref;
}

template<>
inline void dump(std::ostream &out, Binder &binder, BoundFor const &impl, int indent)
{
    binder.dump(out, impl.variable_decl, "Variable", indent);
    binder.dump(out, impl.range, "Range", indent);
    binder.dump(out, impl.body, "Body", indent);
}

}
