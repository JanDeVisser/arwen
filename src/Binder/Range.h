/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>
#include <print>

#include <Binder/Binder.h>
#include <Logging.h>

#include <AST/AST.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundRange

template<>
inline BoundNodeReference bind<RangeNode>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<RangeNode>(ast_node.impl);
    auto        ref = add_node<BoundRange>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.begin = binder.bind_node(ast_impl.begin, ref);
    IMPL.end = binder.bind_node(ast_impl.end, ref);
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundRange>(Binder &binder, BoundNodeReference ref)
{
    IMPL.begin = binder.rebind_node(IMPL.begin);
    IMPL.end = binder.rebind_node(IMPL.end);
    if (binder[IMPL.begin].type && binder[IMPL.end].type) {
        auto const &begin_type = binder.registry[*binder[IMPL.begin].type].decay();
        auto const &end_type = binder.registry[*binder[IMPL.end].type].decay();
        auto        begin_types = binder.alternatives(IMPL.begin, end_type.ref);
        auto        end_types = binder.alternatives(IMPL.end, begin_type.ref);

        auto check_types = [&]() -> bool {
            if (begin_type.ref == end_type.ref) {
                return true;
            }
            for (auto rhs : end_types.alternatives) {
                if (begin_type.ref == rhs) {
                    IMPL.end = binder.accept(IMPL.end, rhs);
                    return true;
                }
            }
            for (auto lhs : begin_types.alternatives) {
                if (lhs == end_type.ref) {
                    IMPL.begin = binder.accept(IMPL.begin, lhs);
                    return true;
                }
            }
            for (auto lhs : begin_types.alternatives) {
                for (auto rhs : end_types.alternatives) {
                    if (lhs == rhs) {
                        IMPL.begin = binder.accept(IMPL.begin, lhs);
                        IMPL.end = binder.accept(IMPL.end, rhs);
                        return true;
                    }
                }
            }
            return false;
        };
        if (!check_types()) {
            return add_error(binder, ref, "Range begin type '{}' and end type '{}' are not compatible", begin_type.name, end_type.name);
        }

        if (!begin_type.is_iterable()) {
            return add_error(binder, ref, "Range type must be an iterable type, got '{}'", begin_type.name);
        }
        binder[ref].type = binder.registry.resolve_range(begin_type.ref);
    }
    return ref;
}

template<>
inline void dump(std::ostream &out, Binder &binder, BoundRange const &impl, int indent)
{
    binder.dump(out, impl.begin, "Begin", indent);
    binder.dump(out, impl.end, "End", indent);
}

}
