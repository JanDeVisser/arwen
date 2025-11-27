/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <functional>
#include <ostream>
#include <print>
#include <ranges>
#include <variant>

#include <Util/Logging.h>
#include <Util/TokenLocation.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

enum class PseudoType {
    Self,
    Lhs,
    Rhs,
};

struct Operand final {
    std::variant<pType, TypeKind, PseudoType> type;

    Operand(pType t) // NOLINT(*-explicit-constructor)
        : type(std::move(t))
    {
    }

    Operand(TypeKind k) // NOLINT(*-explicit-constructor)
        : type(k)
    {
    }

    Operand(PseudoType pseudo_type) // NOLINT(*-explicit-constructor)
        : type(pseudo_type)
    {
    }

    [[nodiscard]] bool matches(pType const &concrete, pType const &hint = nullptr) const
    {
        auto concrete_value_type = concrete->value_type();
        return std::visit(
            overloads {
                [&concrete_value_type](pType const &t) -> bool {
                    return concrete_value_type == t;
                },
                [&concrete_value_type](TypeKind k) -> bool {
                    return concrete_value_type->is(k);
                },
                [&hint, &concrete_value_type](PseudoType pseudo_type) -> bool {
                    assert(hint != nullptr);
                    return hint == concrete_value_type;
                },
            },
            type);
    }
};

using OpResult = std::variant<pType, PseudoType>;

struct BinaryOperator {
    Operand  lhs;
    Operator op;
    Operand  rhs;
    OpResult result;

    [[nodiscard]] bool matches(pType const &concrete_lhs, pType const &concrete_rhs) const
    {
        return lhs.matches(concrete_lhs, concrete_rhs) && rhs.matches(concrete_rhs, concrete_lhs);
    }
};

static std::vector<BinaryOperator> binary_ops;

struct AssignOperator {
    Operator assign_op;
    Operator bin_op;
};

static std::map<Operator, Operator> assign_ops = {
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignDecrement, Operator::Subtract },
    { Operator::AssignDivide, Operator::Divide },
    { Operator::AssignIncrement, Operator::Add },
    { Operator::AssignModulo, Operator::Modulo },
    { Operator::AssignMultiply, Operator::Multiply },
    { Operator::AssignOr, Operator::BinaryOr },
    { Operator::AssignShiftLeft, Operator::ShiftLeft },
    { Operator::AssignShiftRight, Operator::ShiftRight },
    { Operator::AssignXor, Operator::BinaryXor },
};

struct UnaryOperator {
    Operator op;
    Operand  operand;
    OpResult result;
};

static std::vector<UnaryOperator> unary_ops;

BinaryExpression::BinaryExpression(ASTNode lhs, Operator const op, ASTNode rhs)
    : lhs(std::move(lhs))
    , op(op)
    , rhs(std::move(rhs))
{
}

ASTNode BinaryExpression::normalize(ASTNode const &n)
{
    auto make_expression_list = [&n]() -> ASTNode {
        ASTNodes                     nodes;
        std::function<void(ASTNode)> flatten;
        flatten = [&nodes, &flatten](ASTNode const &n) {
            if (auto const binary_expr = std::get_if<BinaryExpression>(&n->node); binary_expr != nullptr) {
                if (binary_expr->op == Operator::Sequence) {
                    flatten(binary_expr->lhs);
                    nodes.push_back(binary_expr->rhs);
                } else {
                    nodes.push_back(n);
                }
            } else {
                nodes.push_back(n);
            }
        };
        flatten(n);
        auto ret = make_node<ExpressionList>(n, nodes);
        return ret->normalize();
    };

    auto const_evaluate = [&n](ASTNode const &lhs, Operator op, ASTNode const &rhs) -> ASTNode {
        auto const &lhs_const = std::get_if<Constant>(&lhs->node);
        if (lhs_const == nullptr || !lhs_const->bound_value) {
            return n;
        }
        if (op == Operator::Cast) {
            if (auto const rhs_type = std::get_if<TypeSpecification>(&rhs->node); rhs_type != nullptr) {
                if (auto const cast_type = rhs_type->resolve(n); cast_type != nullptr) {
                    if (auto const coerced_maybe = lhs_const->bound_value.value().coerce(cast_type); coerced_maybe) {
                        return make_node<Constant>(n, *coerced_maybe);
                    }
                    n.repo->append(n->location, L"Cannot cast value to `{}`", rhs_type->to_string());
                    return nullptr;
                }
            }
        }
        if (auto const &rhs_const = std::get_if<Constant>(&rhs->node); rhs_const != nullptr && rhs_const->bound_value) {
            auto ret = evaluate(lhs_const->bound_value.value(), op, rhs_const->bound_value.value());
            return make_node<Constant>(n, ret);
        }
        return n;
    };

    switch (op) {
    case Operator::Call: {
        auto arg_list = rhs->normalize();
        if (is<Void>(arg_list)) {
            arg_list = make_node<ExpressionList>(arg_list, ASTNodes {});
        }
        if (!is<ExpressionList>(arg_list)) {
            arg_list = make_node<ExpressionList>(arg_list, ASTNodes { arg_list });
        }
        assert(is<ExpressionList>(arg_list));
        arg_list = arg_list->normalize();
        auto call = make_node<Call>(n, lhs->normalize(), arg_list);
        call = call->normalize();
        return call;
    }
    case Operator::Sequence:
        return make_expression_list();
    default:
        if (assign_ops.contains(op)) {
            auto const bin_expr = make_node<BinaryExpression>(n, lhs->normalize(), assign_ops[op], rhs->normalize());
            return make_node<BinaryExpression>(n, lhs->clone(), Operator::Assign, bin_expr->normalize());
        }
        return const_evaluate(lhs->normalize(), op, rhs->normalize());
    }
}

ASTNode BinaryExpression::stamp(ASTNode const &n)
{
    lhs = lhs->stamp();
    rhs = rhs->stamp();
    return n;
}

pType BinaryExpression::bind(ASTNode const &n)
{
    if (binary_ops.empty()) {
        binary_ops = {
            { TypeKind::IntType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
            { TypeRegistry::string, Operator::Multiply, TypeKind::IntType, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::BinaryAnd, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::BinaryOr, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::BinaryXor, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::ShiftLeft, TypeRegistry::u8, PseudoType::Lhs },
            { TypeKind::IntType, Operator::ShiftRight, TypeRegistry::u8, PseudoType::Lhs },
            { TypeRegistry::boolean, Operator::LogicalAnd, TypeRegistry::boolean, TypeRegistry::boolean },
            { TypeRegistry::boolean, Operator::LogicalOr, TypeRegistry::boolean, TypeRegistry::boolean },
        };
    }

    auto lhs_type = lhs->bind();
    if (lhs->status == ASTNodeImpl::Status::BindErrors || lhs->status == ASTNodeImpl::Status::Ambiguous) {
        return lhs_type;
    }
    if (lhs_type == TypeRegistry::undetermined) {
        return TypeRegistry::undetermined;
    }

    if (op == Operator::MemberAccess) {
        if (lhs_type->kind() != TypeKind::ReferenceType) {
            return n.repo->bind_error(
                n->location,
                L"Left hand side of member access operator must be value reference");
        }
        auto const &ref = std::get<ReferenceType>(lhs_type->description);
        if (ref.referencing->kind() != TypeKind::StructType) {
            return n.repo->bind_error(
                n->location,
                L"Left hand side of member access operator must have struct type");
        }
        if (auto rhs_id = std::get_if<Identifier>(&rhs->node); rhs_id == nullptr) {
            return n.repo->bind_error(
                n->location,
                L"Right hand side of member access operator must be identifier");
        } else {
            auto const &s = std::get<StructType>(ref.referencing->description);
            for (auto const &f : s.fields) {
                if (f.name == rhs_id->identifier) {
                    return TypeRegistry::the().referencing(f.type);
                }
            }
            return n.repo->bind_error(
                n->location,
                L"Unknown field `{}`", rhs_id->identifier);
        }
    }

    auto lhs_value_type = lhs_type->value_type();

    auto rhs_type = rhs->bind();
    if (rhs->status == ASTNodeImpl::Status::BindErrors || rhs->status == ASTNodeImpl::Status::Ambiguous) {
        return lhs_type;
    }
    auto rhs_value_type = rhs_type->value_type();

    if (op == Operator::Assign) {
        if (lhs_type->kind() != TypeKind::ReferenceType) {
            return n.repo->bind_error(n->location, L"Cannot assign to non-references");
        }
        if (lhs_value_type != rhs_value_type) {
            return n.repo->bind_error(
                n->location,
                L"Cannot assign a value of type `{}` to a variable of type `{}`",
                rhs_type->name,
                lhs_type->name);
        }
        return lhs_type;
    }

    if (op == Operator::Call && is<FunctionType>(lhs_value_type) && is<TypeList>(rhs_value_type)) {
        auto const [parameters, result] = get<FunctionType>(lhs_value_type);
        auto const [types] = get<TypeList>(rhs_value_type);
        if (parameters.size() != types.size()) {
            if (auto const ident_node = get_if<Identifier>(lhs); ident_node != nullptr) {
                return n.repo->bind_error(
                    n->location,
                    L"Invalid number of arguments calling function `{}`. Expected {}, got {}.",
                    ident_node->identifier,
                    parameters.size(),
                    types.size());
            }
            return n.repo->bind_error(
                n->location,
                L"Invalid number of arguments calling function. Expected {}, got {}.",
                parameters.size(),
                types.size());
        }
        for (auto const &[param, arg] : std::views::zip(parameters, types)) {
            if (param != arg) {
                return n.repo->bind_error(
                    n->location,
                    L"Invalid argument type. Expected {}, got {}.",
                    param->name,
                    arg->name);
            }
        }
        return result;
    }

    if (op == Operator::Cast) {
        if (auto const &lhs_const = std::get_if<Constant>(&lhs->node); lhs_const != nullptr) {
            if (auto const &rhs_value_type_node = std::get_if<TypeSpecification>(&rhs->node); rhs_value_type_node != nullptr) {
                if (auto type = rhs_value_type_node->resolve(n); type != nullptr) {
                    if (auto const &casted = lhs->coerce(type); casted != nullptr) {
                        return type;
                    }
                }
            }
        }
        return std::visit(
            overloads {
                [&rhs_value_type, &n](IntType const &lhs_int_type, IntType const &rhs_int_type) {
                    if (lhs_int_type.width_bits > rhs_int_type.width_bits) {
                        return n.repo->bind_error(
                            n->location,
                            L"Invalid argument type. Cannot narrow integers");
                    }
                    return rhs_value_type;
                },
                [&rhs_value_type, &n](SliceType const &lhs_slice_type, ZeroTerminatedArray const &rhs_zero_terminated_type) {
                    if (lhs_slice_type.slice_of != TypeRegistry::u32 || rhs_zero_terminated_type.array_of != TypeRegistry::u8) {
                        return n.repo->bind_error(
                            n->location,
                            L"Invalid argument type. Cannot cast slices to zero-terminated arrays except for strings");
                    }
                    return rhs_value_type;
                },
                [this, &n](auto const &, auto const &) {
                    return n.repo->bind_error(
                        n->location,
                        L"Invalid argument type. Can only cast integers");
                } },
            lhs_value_type->description, rhs_value_type->description);
    }

    auto check_operators = [](Operator op, pType const &op_lhs_type, pType const &op_rhs_type) -> pType {
        for (auto const &o : binary_ops) {
            if (op == o.op && o.matches(op_lhs_type, op_rhs_type)) {
                return std::visit(
                    overloads {
                        [](pType const &result_type) -> pType {
                            return result_type;
                        },
                        [&op_lhs_type, &op_rhs_type](PseudoType const &pseudo_type) -> pType {
                            switch (pseudo_type) {
                            case PseudoType::Lhs:
                                return op_lhs_type;
                            case PseudoType::Rhs:
                                return op_rhs_type;
                            default:
                                UNREACHABLE();
                            }
                        } },
                    o.result);
            }
        }
        return nullptr;
    };

    if (auto result = check_operators(op, lhs_value_type, rhs_value_type); result != nullptr) {
        return result;
    }
    if (auto const rhs_coerced_to_lhs = rhs->coerce(lhs_value_type); rhs_coerced_to_lhs != nullptr) {
        if (auto result = check_operators(op, lhs_value_type, rhs_coerced_to_lhs->bound_type); result != nullptr) {
            rhs = rhs_coerced_to_lhs;
            return result;
        }
    }
    if (auto const lhs_coerced_to_rhs = lhs->coerce(rhs_value_type); lhs_coerced_to_rhs != nullptr) {
        if (auto result = check_operators(op, lhs_coerced_to_rhs->bound_type, rhs_value_type); result != nullptr) {
            lhs = lhs_coerced_to_rhs;
            return result;
        }
    }

    return n.repo->bind_error(
        n->location,
        L"Operator `{}` cannot be applied to left hand type `{}` and right hand type `{}`",
        as_wstring(Operator_name(op)),
        lhs_value_type->name,
        rhs_value_type->name);
}

std::wostream &BinaryExpression::header(ASTNode const &, std::wostream &os)
{
    return os << Operator_name(op);
}

void BinaryExpression::dump_node(ASTNode const &, int const indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
}

UnaryExpression::UnaryExpression(Operator const op, ASTNode operand)
    : op(op)
    , operand(std::move(operand))
{
}

ASTNode UnaryExpression::normalize(ASTNode const &n)
{
    auto normalized_operand = operand->normalize();
    if (auto operand_const = std::get_if<Constant>(&operand->node); operand_const != nullptr) {
        auto res = evaluate(op, operand_const->bound_value.value());
        return make_node<Constant>(n, res);
    }
    if (op == Operator::Sizeof) {
        if (auto const &operand_as_type = std::get_if<TypeSpecification>(&normalized_operand->node); operand_as_type != nullptr) {
            if (auto const type_maybe = operand_as_type->resolve(n); type_maybe != nullptr) {
                return make_node<Constant>(n, static_cast<int64_t>(type_maybe->size_of()));
            }
        }
    }
    operand = normalized_operand;
    return n;
}

ASTNode UnaryExpression::stamp(ASTNode const &n)
{
    operand = operand->stamp();
    return n;
}

pType UnaryExpression::bind(ASTNode const &n)
{
    if (unary_ops.empty()) {
        unary_ops = {
            { Operator::BinaryInvert, TypeKind::IntType, PseudoType::Self },
            { Operator::Idempotent, TypeKind::IntType, PseudoType::Self },
            { Operator::Idempotent, TypeKind::FloatType, PseudoType::Self },
            { Operator::Negate, TypeKind::IntType, PseudoType::Self },
            { Operator::Negate, TypeKind::FloatType, PseudoType::Self },
            { Operator::LogicalInvert, TypeRegistry::boolean, TypeRegistry::boolean },
            { Operator::Length, TypeKind::SliceType, TypeRegistry::i64 },
            { Operator::Length, TypeKind::Array, TypeRegistry::i64 },
            { Operator::Length, TypeKind::DynArray, TypeRegistry::i64 },
            { Operator::Length, TypeKind::ZeroTerminatedArray, TypeRegistry::i64 },
        };
    }
    auto operand_type = operand->bind();
    assert(operand_type != nullptr);
    if (operand_type == TypeRegistry::undetermined || is<BindErrors>(operand_type)) {
        return operand_type;
    }
    if (operand_type == TypeRegistry::ambiguous) {
        return n.repo->bind_error(operand->location, std::wstring { L"Type ambiguity" });
    }
    if (op == Operator::Sizeof) {
        return TypeRegistry::i64;
    }
    if (op == Operator::AddressOf) {
        if (!is<ReferenceType>(operand_type)) {
            return n.repo->bind_error(n->location, L"Cannot get address of non-references");
        }
        return TypeRegistry::pointer;
    }
    for (auto const &[oper, operand, result] : unary_ops) {
        if (op == oper && operand.matches(operand_type)) {
            return std::visit(overloads {
                                  [](pType const &result_type) -> pType {
                                      return result_type;
                                  },
                                  [&operand_type](PseudoType const &pseudo_type) -> pType {
                                      switch (pseudo_type) {
                                      case PseudoType::Self:
                                          return operand_type;
                                      default:
                                          UNREACHABLE();
                                      }
                                  } },
                result);
        }
    }
    return n.repo->bind_error(
        n->location,
        L"Unary operator `{}` cannot be applied to type `{}`",
        as_wstring(Operator_name(op)),
        operand_type->name);
}

std::wostream &UnaryExpression::header(ASTNode const &, std::wostream &os)
{
    return os << Operator_name(op);
}

void UnaryExpression::dump_node(ASTNode const &, int const indent)
{
    operand->dump(indent + 4);
}

ExpressionList::ExpressionList(ASTNodes expressions)
    : expressions(std::move(expressions))
{
}

ASTNode ExpressionList::normalize(ASTNode const &n)
{
    normalize_nodes(expressions);
    return n;
}

ASTNode ExpressionList::stamp(ASTNode const &n)
{
    expressions = stamp_nodes(expressions);
    return n;
}

pType ExpressionList::bind(ASTNode const &n)
{
    std::vector<pType> types;
    for (auto const &expr : expressions) {
        auto expr_type = expr->bind();
        if (expr->status != ASTNodeImpl::Status::Bound) {
            return expr_type;
        }
        types.push_back(expr_type);
    }
    return TypeRegistry::the().typelist_of(types);
}

std::wostream &ExpressionList::header(ASTNode const &, std::wostream &os)
{
    return os << L"ExpressionList";
}

void ExpressionList::dump_node(ASTNode const &, int const indent)
{
    for (auto const &stmt : expressions) {
        stmt->dump(indent + 4);
    }
}

}
