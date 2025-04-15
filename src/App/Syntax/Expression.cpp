/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <functional>
#include <memory>
#include <ranges>

#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

struct BinaryOperator {
    pType    lhs;
    Operator op;
    pType    rhs;
    pType    result;
};

static std::vector<BinaryOperator> binary_ops {
    { TypeRegistry::u8, Operator::Add, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::Add, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::Add, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::Add, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::Add, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::Add, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::Add, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::Add, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::f32, Operator::Add, TypeRegistry::f32, TypeRegistry::f32 },
    { TypeRegistry::f64, Operator::Add, TypeRegistry::f64, TypeRegistry::f64 },
    { TypeRegistry::u8, Operator::Subtract, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::Subtract, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::Subtract, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::Subtract, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::Subtract, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::Subtract, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::Subtract, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::Subtract, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::f32, Operator::Subtract, TypeRegistry::f32, TypeRegistry::f32 },
    { TypeRegistry::f64, Operator::Subtract, TypeRegistry::f64, TypeRegistry::f64 },
    { TypeRegistry::u8, Operator::Multiply, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::Multiply, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::Multiply, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::Multiply, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::Multiply, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::Multiply, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::Multiply, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::Multiply, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::f32, Operator::Multiply, TypeRegistry::f32, TypeRegistry::f32 },
    { TypeRegistry::f64, Operator::Multiply, TypeRegistry::f64, TypeRegistry::f64 },
    { TypeRegistry::u8, Operator::Divide, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::Divide, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::Divide, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::Divide, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::Divide, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::Divide, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::Divide, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::Divide, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::f32, Operator::Divide, TypeRegistry::f32, TypeRegistry::f32 },
    { TypeRegistry::f64, Operator::Divide, TypeRegistry::f64, TypeRegistry::f64 },
    { TypeRegistry::u8, Operator::Modulo, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::Modulo, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::Modulo, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::Modulo, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::Modulo, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::Modulo, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::Modulo, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::Modulo, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::f32, Operator::Modulo, TypeRegistry::f32, TypeRegistry::f32 },
    { TypeRegistry::f64, Operator::Modulo, TypeRegistry::f64, TypeRegistry::f64 },
    { TypeRegistry::u8, Operator::Equals, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::Equals, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::Equals, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::Equals, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::Equals, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::Equals, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::Equals, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::Equals, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::Equals, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::Equals, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::NotEqual, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::NotEqual, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::NotEqual, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::NotEqual, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::NotEqual, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::NotEqual, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::NotEqual, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::NotEqual, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::NotEqual, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::NotEqual, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::Less, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::Less, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::Less, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::Less, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::Less, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::Less, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::Less, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::Less, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::Less, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::Less, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::LessEqual, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::LessEqual, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::LessEqual, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::LessEqual, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::LessEqual, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::LessEqual, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::LessEqual, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::LessEqual, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::LessEqual, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::LessEqual, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::Greater, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::Greater, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::Greater, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::Greater, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::Greater, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::Greater, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::Greater, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::Greater, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::Greater, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::Greater, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::GreaterEqual, TypeRegistry::u8, TypeRegistry::boolean },
    { TypeRegistry::i8, Operator::GreaterEqual, TypeRegistry::i8, TypeRegistry::boolean },
    { TypeRegistry::u16, Operator::GreaterEqual, TypeRegistry::u16, TypeRegistry::boolean },
    { TypeRegistry::i16, Operator::GreaterEqual, TypeRegistry::i16, TypeRegistry::boolean },
    { TypeRegistry::u32, Operator::GreaterEqual, TypeRegistry::u32, TypeRegistry::boolean },
    { TypeRegistry::i32, Operator::GreaterEqual, TypeRegistry::i32, TypeRegistry::boolean },
    { TypeRegistry::u64, Operator::GreaterEqual, TypeRegistry::u64, TypeRegistry::boolean },
    { TypeRegistry::i64, Operator::GreaterEqual, TypeRegistry::i64, TypeRegistry::boolean },
    { TypeRegistry::f32, Operator::GreaterEqual, TypeRegistry::f32, TypeRegistry::boolean },
    { TypeRegistry::f64, Operator::GreaterEqual, TypeRegistry::f64, TypeRegistry::boolean },
    { TypeRegistry::u8, Operator::BinaryAnd, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::BinaryAnd, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::BinaryAnd, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::BinaryAnd, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::BinaryAnd, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::BinaryAnd, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::BinaryAnd, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::BinaryOr, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::u8, Operator::BinaryOr, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::BinaryOr, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::BinaryOr, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::BinaryOr, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::BinaryOr, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::BinaryOr, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::BinaryOr, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::BinaryOr, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::i64, Operator::BinaryOr, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::u8, Operator::BinaryXor, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::BinaryXor, TypeRegistry::i8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::BinaryXor, TypeRegistry::u16, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::BinaryXor, TypeRegistry::i16, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::BinaryXor, TypeRegistry::u32, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::BinaryXor, TypeRegistry::i32, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::BinaryXor, TypeRegistry::u64, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::BinaryXor, TypeRegistry::i64, TypeRegistry::i64 },
    { TypeRegistry::u8, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::ShiftLeft, TypeRegistry::u8, TypeRegistry::i64 },
    { TypeRegistry::u8, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::u8 },
    { TypeRegistry::i8, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::i8 },
    { TypeRegistry::u16, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::u16 },
    { TypeRegistry::i16, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::i16 },
    { TypeRegistry::u32, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::u32 },
    { TypeRegistry::i32, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::i32 },
    { TypeRegistry::u64, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::u64 },
    { TypeRegistry::i64, Operator::ShiftRight, TypeRegistry::u8, TypeRegistry::i64 },
    { TypeRegistry::boolean, Operator::LogicalAnd, TypeRegistry::boolean, TypeRegistry::boolean },
    { TypeRegistry::boolean, Operator::LogicalOr, TypeRegistry::boolean, TypeRegistry::boolean },
};

BinaryExpression::BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs)
    : SyntaxNode(SyntaxNodeType::BinaryExpression)
    , lhs(lhs)
    , op(op)
    , rhs(rhs)
{
}

pSyntaxNode BinaryExpression::normalize(Parser &parser)
{
    auto make_expression_list = [this, &parser]() -> pSyntaxNode {
        SyntaxNodes                      nodes;
        std::function<void(pSyntaxNode)> flatten;
        flatten = [&nodes, &flatten](pSyntaxNode n) {
            if (auto binex = std::dynamic_pointer_cast<BinaryExpression>(n); binex) {
                if (binex->op == Operator::Sequence) {
                    nodes.push_back(binex->lhs);
                    flatten(binex->rhs);
                } else {
                    nodes.push_back(binex);
                }
            } else {
                nodes.push_back(n);
            }
        };
        flatten(shared_from_this());
        return make_node<ExpressionList>(nodes, nodes)->normalize(parser);
    };

    auto evaluate = [this](pSyntaxNode const &lhs, Operator op, pSyntaxNode const &rhs) -> pSyntaxNode {
        auto lhs_const = std::dynamic_pointer_cast<ConstantExpression>(lhs);
        auto rhs_const = std::dynamic_pointer_cast<ConstantExpression>(rhs);
        if (lhs_const != nullptr && rhs_const != nullptr) {
            if (auto ret = lhs_const->evaluate_binop(op, rhs_const); ret != nullptr) {
                return ret;
            }
        }
        return make_node<BinaryExpression>(lhs->location + rhs->location, lhs, op, rhs);
    };

    if (op == Operator::Sequence) {
        return make_expression_list();
    }
    return evaluate(lhs->normalize(parser), op, rhs->normalize(parser));
}

pType BinaryExpression::bind(Parser &parser)
{
    auto lhs_type = bind_node(lhs, parser);
    assert(lhs_type != nullptr);
    if (lhs_type == TypeRegistry::undetermined || lhs_type->is<BindErrors>()) {
        return lhs_type;
    }
    if (lhs_type == TypeRegistry::ambiguous) {
        return make_error(lhs->location, L"Type ambiguity");
    }
    auto rhs_type = bind_node(rhs, parser);
    assert(rhs_type != nullptr);
    if (rhs_type == TypeRegistry::undetermined || rhs_type->is<BindErrors>()) {
        return rhs_type;
    }
    if (rhs_type == TypeRegistry::ambiguous) {
        return make_error(rhs->location, L"Type ambiguity");
    }

    if (op == Operator::Call && lhs_type->is<FunctionType>() && rhs_type->is<TypeList>()) {
        auto func = std::get<FunctionType>(lhs_type->description);
        auto list = std::get<TypeList>(rhs_type->description);
        if (func.parameters.size() != list.types.size()) {
            if (auto ident_node = std::dynamic_pointer_cast<Identifier>(lhs); ident_node != nullptr) {
                return make_error(
                    location,
                    L"Invalid number of arguments calling function `{}`. Expected {}, got {}.",
                    ident_node->identifier,
                    func.parameters.size(),
                    list.types.size());
            }
            return make_error(
                location,
                L"Invalid number of arguments calling function. Expected {}, got {}.",
                func.parameters.size(),
                list.types.size());
        }
        for (auto const &[param, arg] : std::views::zip(func.parameters, list.types)) {
            if (param != arg) {
                return make_error(
                    location,
                    L"Invalid argument type. Expected {}, got {}.",
                    param->name,
                    arg->name);
            }
        }
    }

    for (auto const &o : binary_ops) {
        if (o.op == op && o.lhs == lhs_type && o.rhs == rhs_type) {
            return o.result;
        }
    }

    return make_error(
        location,
        L"Operator `{}` cannot be applied to left hand type `{}` and right hand type `{}`",
        as_wstring(Operator_name(op)),
        lhs_type->name,
        rhs_type->name);
}

void BinaryExpression::header()
{
    std::cout << Operator_name(op);
}

void BinaryExpression::dump_node(int indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
}

struct UnaryOperator {
    Operator op;
    pType    operand;
    pType    result;
};

static std::vector<UnaryOperator> unary_ops {
    { Operator::Idempotent, TypeRegistry::u8, TypeRegistry::u8 },
    { Operator::Idempotent, TypeRegistry::i8, TypeRegistry::i8 },
    { Operator::Idempotent, TypeRegistry::u16, TypeRegistry::u16 },
    { Operator::Idempotent, TypeRegistry::i16, TypeRegistry::i16 },
    { Operator::Idempotent, TypeRegistry::u32, TypeRegistry::u32 },
    { Operator::Idempotent, TypeRegistry::i32, TypeRegistry::i32 },
    { Operator::Idempotent, TypeRegistry::u64, TypeRegistry::u64 },
    { Operator::Idempotent, TypeRegistry::i64, TypeRegistry::i64 },
    { Operator::Idempotent, TypeRegistry::f32, TypeRegistry::f32 },
    { Operator::Idempotent, TypeRegistry::f64, TypeRegistry::f64 },
    { Operator::Negate, TypeRegistry::u8, TypeRegistry::u8 },
    { Operator::Negate, TypeRegistry::i8, TypeRegistry::i8 },
    { Operator::Negate, TypeRegistry::u16, TypeRegistry::u16 },
    { Operator::Negate, TypeRegistry::i16, TypeRegistry::i16 },
    { Operator::Negate, TypeRegistry::u32, TypeRegistry::u32 },
    { Operator::Negate, TypeRegistry::i32, TypeRegistry::i32 },
    { Operator::Negate, TypeRegistry::u64, TypeRegistry::u64 },
    { Operator::Negate, TypeRegistry::i64, TypeRegistry::i64 },
    { Operator::Negate, TypeRegistry::f32, TypeRegistry::f32 },
    { Operator::Negate, TypeRegistry::f64, TypeRegistry::f64 },
    { Operator::Invert, TypeRegistry::u8, TypeRegistry::u8 },
    { Operator::Invert, TypeRegistry::i8, TypeRegistry::i8 },
    { Operator::Invert, TypeRegistry::u16, TypeRegistry::u16 },
    { Operator::Invert, TypeRegistry::i16, TypeRegistry::i16 },
    { Operator::Invert, TypeRegistry::u32, TypeRegistry::u32 },
    { Operator::Invert, TypeRegistry::i32, TypeRegistry::i32 },
    { Operator::Invert, TypeRegistry::u64, TypeRegistry::u64 },
    { Operator::Invert, TypeRegistry::i64, TypeRegistry::i64 },
    { Operator::Invert, TypeRegistry::boolean, TypeRegistry::boolean },
};

UnaryExpression::UnaryExpression(Operator op, pSyntaxNode operand)
    : SyntaxNode(SyntaxNodeType::UnaryExpression)
    , op(op)
    , operand(operand)
{
}

pSyntaxNode UnaryExpression::normalize(Parser &parser)
{
    return make_node<UnaryExpression>(location, op, operand->normalize(parser));
}

pType UnaryExpression::bind(Parser &parser)
{
    auto operand_type = bind_node(operand, parser);
    assert(operand_type != nullptr);
    if (operand_type == TypeRegistry::undetermined || operand_type->is<BindErrors>()) {
        return operand_type;
    }
    if (operand_type == TypeRegistry::ambiguous) {
        return make_error(operand->location, std::wstring { L"Type ambiguity" });
    }
    for (auto const &o : unary_ops) {
        if (o.op == op && o.operand == operand_type) {
            return o.result;
        }
    }
    return make_error(
        location,
        L"Unary operator `{}` cannot be applied to type `{}`",
        as_wstring(Operator_name(op)),
        operand_type->name);
}

void UnaryExpression::header()
{
    std::cout << Operator_name(op);
}

void UnaryExpression::dump_node(int indent)
{
    operand->dump_node(indent + 4);
}

ExpressionList::ExpressionList(SyntaxNodes expressions)
    : SyntaxNode(SyntaxNodeType::ExpressionList)
    , expressions(std::move(expressions))
{
}

pSyntaxNode ExpressionList::normalize(Parser &parser)
{
    SyntaxNodes normalized;
    for (auto const &expr : expressions) {
        normalized.emplace_back(expr->normalize(parser));
    }
    return make_node<ExpressionList>(location, normalized);
}

pType ExpressionList::bind(Parser &parser)
{
    std::vector<pType> types;
    for (auto const &expr : expressions) {
        auto expr_type = bind_node(expr, parser);
        if (expr_type == TypeRegistry::ambiguous || expr_type == TypeRegistry::undetermined) {
            return expr_type;
        }
        types.push_back(expr_type);
    }
    return TypeRegistry::the().typelist_of(types);
}

void ExpressionList::dump_node(int indent)
{
    for (auto const &stmt : expressions) {
        stmt->dump(indent + 4);
    }
}

}
