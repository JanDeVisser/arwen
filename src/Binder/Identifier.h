/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>
#include <variant>

#include <Logging.h>
#include <AST/AST.h>
#include <Binder/Binder.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundIdentifier

template<>
inline BoundNodeReference bind<Identifier>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Identifier>(ast_node.impl);
    auto        ref = add_node<BoundIdentifier>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.name = ast_impl.text;
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundIdentifier>(Binder &binder, BoundNodeReference ref)
{
    // ScopeGuard sg {
    //     [&binder, ref]() {
    //         std::println("rebind<BoundIdentifier> {}. {} {} {}",
    //             ref,
    //             IMPL.name,
    //             IMPL.declaration ? "resolved" : "unresolved",
    //             binder[ref].type ? "bound" : "unbound");
    //     }
    // };

    auto variable = binder.resolve(IMPL.name);
    if (variable) {
        IMPL.declaration = *variable;
        auto &var_node = binder[*IMPL.declaration];
        return std::visit(
            overload {
                [&](BoundConstantDeclaration const &c) -> BoundNodeReference {
                    if (std::holds_alternative<BoundConstant>(binder[c.initializer].impl)) {
                        auto const &constant_node = binder[c.initializer];
                        auto        constant = std::get<BoundConstant>(constant_node.impl);
                        auto        copy = add_node<BoundConstant>(binder, constant_node.ast_ref, constant_node.location, binder[ref].parent);
                        binder[copy].type = constant.value.type();
                        std::get<BoundConstant>(binder[copy].impl).value = constant.value;
                        return copy;
                    } else {
                        binder[ref].type = var_node.type;
                    }
                    return ref;
                },
                [&](auto const &) -> BoundNodeReference {
                    binder[ref].type = var_node.type;
                    return ref;
                } },
            var_node.impl);
    } else if (binder.pass > 0) {
        return add_error(binder, ref, "Undefined variable '{}'", IMPL.name);
    }
    return ref;
}

template<>
inline void to_string(std::ostream &out, Binder &, BoundIdentifier const &impl)
{
    out << impl.name;
}

}
