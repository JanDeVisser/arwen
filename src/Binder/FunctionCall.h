/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <ostream>
#include <print>
#include <string>

#include <Logging.h>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundFunctionCall

template<>
BoundNodeReference rebind<BoundFunctionCall>(Binder &binder, BoundNodeReference ref)
{
    // ScopeGuard sg {
    //     [&binder, ref]() {
    //         std::println("rebind<BoundFunctionCall> {}. {} #Args {} {} {}",
    //             ref,
    //             IMPL.name,
    //             IMPL.arguments.size(),
    //             *IMPL.function ? "resolved" : "unresolved",
    //             binder[ref].type ? "bound" : "unbound");
    //         for (auto ix = 0; ix < IMPL.arguments.size(); ++ix) {
    //             std::println("* {}. {} {} {}",
    //                 ix,
    //                 binder[IMPL.arguments[ix]].type_name(),
    //                 IMPL.arguments[ix],
    //                 binder[IMPL.arguments[ix]].type.has_value() ? "bound" : "unbound");
    //         }
    //     }
    // };

    bool all_bound { true };
    for (auto ix = 0; ix < IMPL.arguments.size(); ++ix) {
        IMPL.arguments[ix] = binder.rebind_node(IMPL.arguments[ix]);
        all_bound &= binder[IMPL.arguments[ix]].type.has_value();
    }

    if (!IMPL.function) {
        auto n = IMPL.name;
        IMPL.function = binder.resolve(n);
        if (!IMPL.function && binder.pass > 0) {
            return add_error(binder, ref, "Undefined function '{}'", n);
        }
    }
    if (IMPL.function) {
        auto &func_decl = I(BoundFunction, *IMPL.function);
        if (func_decl.parameters.size() != IMPL.arguments.size()) {
            return add_error(binder, ref, "In call to '{}': Expected {} arguments, got {}", func_decl.name, func_decl.parameters.size(), IMPL.arguments.size());
        }
        if (*binder[*IMPL.function].type == VoidType || binder.type_of(binder[ref].parent) == BoundNodeType::BoundBlock) { // Hack. FIXME later
            IMPL.discard_result = true;
        }
        size_t ix { 0 };
        for (auto ix = 0; ix < func_decl.parameters.size(); ++ix) {
            auto param = func_decl.parameters[ix];
            auto arg = IMPL.arguments[ix];
            if (!binder[param].type || !binder[arg].type) {
                all_bound = false;
                continue;
            }
            if (binder[arg].type == binder[param].type) {
                continue;
            }
            auto const &param_type = binder.registry[*binder[param].type];
            auto        arg_types = binder.alternatives(arg, param_type.ref);
            if (arg_types.preferred == param_type.ref) {
                binder[arg].type = param_type.ref;
                continue;
            } else {
                bool matched { false };
                for (auto type : arg_types.alternatives) {
                    if (type == param_type.ref) {
                        IMPL.arguments[ix] = binder.accept(arg, type);
                        matched = true;
                        break;
                    }
                }
                if (!matched) {
                    all_bound = false;
                    add_error(binder, arg, "In call to '{}', argument '{}': Type mismatch: expected '{}', got '{}'",
                        func_decl.name,
                        I(BoundParameter, param).name,
                        binder.registry[*binder[param].type].name,
                        binder.registry[*binder[arg].type].name);
                }
            }
        }
        if (all_bound && binder[*IMPL.function].type) {
            binder[ref].type = binder[*IMPL.function].type;
        }
    }
    return ref;
}

template<>
BoundNodeReference bind<FunctionCall>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionCall>(ast_node.impl);
    auto        ref = add_node<BoundFunctionCall>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.name = ast_impl.name;
    for (auto arg : ast_impl.arguments) {
        auto arg_ref = binder.bind_node(arg, ref);
        IMPL.arguments.push_back(arg_ref);
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundFunctionCall const &impl)
{
    out << impl.name;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundFunctionCall const &impl, int indent)
{
    if (impl.function) {
        out << std::string(indent, ' ') << "Function: ";
        binder.to_string(out, *impl.function);
        out << "\n";
    }
    binder.dump(out, impl.arguments, "Argument", indent);
}

}
