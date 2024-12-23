/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>
#include <print>
#include <string_view>
#include <variant>

#include <Lib.h>
#include <Logging.h>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <Type/Type.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundBinaryExpression

template<>
BoundNodeReference bind<BinaryExpression>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BinaryExpression>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundBinaryExpression>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.left = binder.bind_node(ast_impl.left, ref);
    IMPL.op = ast_impl.op;
    IMPL.right = binder.bind_node(ast_impl.right, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundBinaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (!binder[IMPL.left].type || !binder[IMPL.right].type) {
        return ref;
    }
    auto const &left_type = binder.registry[*binder[IMPL.left].type];
    auto const &right_type = binder.registry[*binder[IMPL.right].type];
    switch (IMPL.op) {
    case BinaryOperator::MemberAccess: {
        if (left_type.typespec.tag() != TypeKind::Object) {
            return add_error(binder, ref, "Member access requires an object LHS value");
        }
        auto const &obj = left_type.typespec.get<TypeKind::Object>();
        if (!std::holds_alternative<BoundMember>(binder[IMPL.right].impl)) {
            return add_error(binder, ref, "Member access requires an identifier RHS value");
        }
        std::string_view n = std::get<BoundMember>(binder[IMPL.right].impl).name;
        binder[ref].type = {};
        for (auto const &fld : obj.fields) {
            if (fld.first == n) {
                binder[ref].type = fld.second;
                break;
            }
        }
        if (!binder[ref].type) {
            return add_error(binder, ref, "Unknown member '{}'", n);
        }
    } break;
    case BinaryOperator::Subscript: {
        if (binder[IMPL.left].type) {
            auto type = binder.registry[*binder[IMPL.left].type];
            switch (type.typespec.tag()) {
            case TypeKind::Array: {
                auto const &arr = type.typespec.get<TypeKind::Array>();
                binder[ref].type = arr.element_type;
            } break;
            case TypeKind::Pointer: {
                auto const &ptr = type.typespec.get<TypeKind::Pointer>();
                binder[ref].type = ptr.element_type;
            } break;
            case TypeKind::Slice: {
                auto const &slice = type.typespec.get<TypeKind::Slice>();
                binder[ref].type = slice.element_type;
            } break;
            case TypeKind::Primitive:
                if (type.is_raw_pointer()) {
                    binder[ref].type = U8Type;
                }
                // Fall through
            default:
                return add_error(binder, ref, "Can only subscript arrays, slices, or pointers, not {}", type.typespec.tag());
            }
        }
        if (!std::holds_alternative<BoundSubscript>(binder[IMPL.right].impl)) {
            return add_error(binder, ref, "Invalid array index");
        }
    } break;
    default: {
        auto                  left_types = binder.alternatives(IMPL.left, right_type.ref);
        auto                  right_types = binder.alternatives(IMPL.right, left_type.ref);
        BinaryOperatorMapping m { IMPL.op };

        auto test_types = [&](TypeReference lhs, TypeReference rhs) {
            if (auto c = m.compatible(static_cast<PrimitiveType>(lhs), static_cast<PrimitiveType>(rhs)); c) {
                binder[ref].type = static_cast<TypeReference>(*c);
                return true;
            }
            return false;
        };

        auto flat = std::visit(
            overload {
                [&](BoundConstant &l, BoundConstant &r) -> BoundNodeReference {
                    if (auto result = m(l.value, r.value); result) {
                        l.value = *result;
                        binder[IMPL.left].type = l.value.type();
                        binder[IMPL.left].parent = binder[ref].parent;
                        return IMPL.left;
                    }
                    return add_error(binder, ref, "Cannot use operator '{}' with constant LHS '{}' and constant RHS '{}'", m.op, left_type.name, right_type.name);
                },
                [ref](auto &, auto &) -> BoundNodeReference {
                    return ref;
                },
            },
            binder[IMPL.left].impl, binder[IMPL.right].impl);

        if (flat != ref) {
            return flat;
        }

        if (test_types(left_type.ref, right_type.ref)) {
            return ref;
        }
        for (auto rhs : right_types.alternatives) {
            if (test_types(left_type.ref, rhs)) {
                IMPL.right = binder.accept(IMPL.right, rhs);
                return ref;
            }
        }
        for (auto lhs : left_types.alternatives) {
            if (test_types(lhs, right_type.ref)) {
                IMPL.left = binder.accept(IMPL.left, lhs);
                return ref;
            }
        }
        for (auto lhs : left_types.alternatives) {
            for (auto rhs : right_types.alternatives) {
                if (test_types(lhs, rhs)) {
                    IMPL.left = binder.accept(IMPL.left, lhs);
                    IMPL.right = binder.accept(IMPL.right, rhs);
                    return ref;
                }
            }
        }
        add_error(binder, ref, "Cannot use operator '{}' with LHS '{}' and RHS '{}'", to_string(m.op), left_type.name, right_type.name);
    } break;
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundBinaryExpression const &impl)
{
    out << "lhs " << to_string(impl.op) << " rhs";
}

template<>
void dump(std::ostream &out, Binder &binder, BoundBinaryExpression const &impl, int indent)
{
    binder.dump(out, impl.left, "lhs", indent);
    binder.dump(out, impl.right, "rhs", indent);
}

}
