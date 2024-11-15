/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Type/Value.h"
#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <sstream>
#include <string_view>

#include <Lexer/Lexer.h>
#include <Type/Type.h>
#include <Lib.h>
#include <Logging.h>
#include <utility>

namespace Arwen {

#define BinaryOperators(S)                                    \
    S(None, true, TokenKind { Null_Value })                   \
    S(Add, true, TokenKind { Symbol_Value, '+' })             \
    S(Assign, false, TokenKind { Symbol_Value, '=' })         \
    S(BinaryAnd, true, TokenKind { Symbol_Value, '&' })       \
    S(BinaryOr, true, TokenKind { Symbol_Value, '|' })        \
    S(BinaryXor, true, TokenKind { Symbol_Value, '^' })       \
    S(Call, false, TokenKind { Null_Value })                  \
    S(Divide, true, TokenKind { Symbol_Value, '/' })          \
    S(Equal, false, TokenKind { Keyword_Value, "==" })        \
    S(Greater, false, TokenKind { Symbol_Value, '>' })        \
    S(GreaterEqual, false, TokenKind { Keyword_Value, ">=" }) \
    S(Less, false, TokenKind { Symbol_Value, '<' })           \
    S(LessEqual, false, TokenKind { Keyword_Value, "<=" })    \
    S(LogicalAnd, false, TokenKind { Keyword_Value, "&&" })   \
    S(LogicalOr, false, TokenKind { Keyword_Value, "||" })    \
    S(MemberAccess, true, TokenKind { Symbol_Value, '.' })    \
    S(Modulo, true, TokenKind { Symbol_Value, '%' })          \
    S(Multiply, true, TokenKind { Symbol_Value, '*' })        \
    S(NotEqual, false, TokenKind { Keyword_Value, "!=" })     \
    S(Shl, true, TokenKind { Keyword_Value, "<<" })           \
    S(Shr, true, TokenKind { Keyword_Value, ">>" })           \
    S(Subtract, true, TokenKind { Symbol_Value, '-' })        \
    S(Subscript, false, TokenKind { Symbol_Value, '[' })

enum class BinaryOperator {
#undef S
#define S(Op, Assign, ...) Op,
    BinaryOperators(S)
#undef S
};

template<>
inline std::optional<BinaryOperator> decode(std::string_view s, ...)
{
#undef S
#define S(Op, Assign, ...) \
    if (iequals(s, #Op))   \
        return BinaryOperator::Op;
    BinaryOperators(S)
#undef S
        return {};
}

struct BinaryOperatorCompatibility {
    BinaryOperator op;
    BasicType      lhs;
    BasicType      rhs;
    BasicType      result;
};

BinaryOperatorCompatibility constexpr binary_operator_compatibility[] = {
    { BinaryOperator::Add, BasicType::Numeric, BasicType::Numeric, BasicType::Numeric },
    { BinaryOperator::Subtract, BasicType::Numeric, BasicType::Numeric, BasicType::Numeric },
    { BinaryOperator::Multiply, BasicType::Numeric, BasicType::Numeric, BasicType::Numeric },
    { BinaryOperator::Divide, BasicType::Numeric, BasicType::Numeric, BasicType::Numeric },
    { BinaryOperator::Modulo, BasicType::Numeric, BasicType::Numeric, BasicType::Numeric },
    { BinaryOperator::BinaryAnd, BasicType::Int, BasicType::Int, BasicType::Int },
    { BinaryOperator::BinaryOr, BasicType::Int, BasicType::Int, BasicType::Int },
    { BinaryOperator::BinaryXor, BasicType::Int, BasicType::Int, BasicType::Int },
    { BinaryOperator::Shl, BasicType::Int, BasicType::Int, BasicType::Int },
    { BinaryOperator::Shr, BasicType::Int, BasicType::Int, BasicType::Int },
    { BinaryOperator::LogicalAnd, BasicType::Bool, BasicType::Bool, BasicType::Bool },
    { BinaryOperator::LogicalOr, BasicType::Bool, BasicType::Bool, BasicType::Bool },
    { BinaryOperator::Equal, BasicType::Any, BasicType::Self, BasicType::Bool },
    { BinaryOperator::NotEqual, BasicType::Any, BasicType::Self, BasicType::Bool },
    { BinaryOperator::Less, BasicType::Any, BasicType::Self, BasicType::Bool },
    { BinaryOperator::LessEqual, BasicType::Any, BasicType::Self, BasicType::Bool },
    { BinaryOperator::Greater, BasicType::Any, BasicType::Self, BasicType::Bool },
    { BinaryOperator::GreaterEqual, BasicType::Any, BasicType::Self, BasicType::Bool },
};

uint64_t constexpr binary_operator_compatibility_size = sizeof(binary_operator_compatibility) / sizeof(BinaryOperatorCompatibility);

template<>
inline constexpr std::string_view to_string(BinaryOperator const &v)
{
    switch (v) {
#undef S
#define S(Op, Assign, ...)   \
    case BinaryOperator::Op: \
        return #Op;
        BinaryOperators(S)
#undef S
    }
}

struct AddOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.add(v2);
    }
};

struct SubtractOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.subtract(v2);
    }
};

struct MultiplyOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.multiply(v2);
    }
};

struct DivideOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.divide(v2);
    }
};

struct ModuloOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.modulo(v2);
    }
};

struct ShlOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.shl(v2);
    }
};

struct ShrOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.shr(v2);
    }
};

struct BinaryAndOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.shl(v2);
    }
};

struct BinaryOrOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.shl(v2);
    }
};

struct BinaryXorOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.shl(v2);
    }
};

struct LogicalAndOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.logical_and(v2);
    }
};

struct LogicalOrOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.logical_or(v2);
    }
};

struct EqualOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1 == v2;
    }
};

struct NotEqualOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return !(v1 == v2);
    }
};

struct LessOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1 < v2;
    }
};

struct LessEqualOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1 < v2 || v1 == v2;
    }
};

struct GreaterOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v2 < v1;
    }
};

struct GreaterEqualOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v2 < v1 || v1 == v2;
    }
};

struct DummyOp {
    std::optional<Value> operator()(Value const &v)
    {
        return {};
    }
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return {};
    }
};

using AddressOfOp = DummyOp;
using AssignOp = DummyOp;
using DerefOp = DummyOp;
using CallOp = DummyOp;
using MemberAccessOp = DummyOp;
using NoneOp = DummyOp;
using SubscriptOp = DummyOp;

struct BinaryOperatorMapping {
    BinaryOperator op;
    bool           is_assignment_op;
    TokenKind      token;

    explicit BinaryOperatorMapping(BinaryOperator op)
        : op(op)
    {
        switch (op) {
#undef S
#define S(Op, Assign, ...)         \
    case BinaryOperator::Op:       \
        is_assignment_op = Assign; \
        token = __VA_ARGS__;       \
        break;
            BinaryOperators(S)
#undef S
                default : UNREACHABLE();
        }
    }

    explicit BinaryOperatorMapping(TokenKind const &token)
        : token(token)
    {
#undef S
#define S(Op, Assign, ...)         \
    if (token == __VA_ARGS__) {    \
        op = BinaryOperator::Op;   \
        is_assignment_op = Assign; \
        return;                    \
    }
        BinaryOperators(S)
#undef S
            UNREACHABLE();
    }

    std::optional<PrimitiveType> compatible(PrimitiveType const &lhs, PrimitiveType const &rhs)
    {
        for (auto ix = 0; ix < binary_operator_compatibility_size; ++ix) {
            auto const &compatibility = binary_operator_compatibility[ix];
            if (compatibility.op == op) {
                if (is_a(lhs, compatibility.lhs) && is_a(rhs, compatibility.rhs)) {
                    switch (compatibility.result) {
                    case BasicType::Numeric:
                        return lhs == PrimitiveType::Float || rhs == PrimitiveType::Float
                            ? PrimitiveType::Float
                            : lhs;
                    case BasicType::Self:
                        return lhs;
                    default:
                        return static_cast<PrimitiveType>(compatibility.result);
                    }
                }
            }
        }
        return {};
    }

    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        switch (op) {
#undef S
#define S(O, Assign, ...)   \
    case BinaryOperator::O: \
        return O##Op {}(v1, v2);
            BinaryOperators(S)
#undef S
        }
    }
};

#define UnaryOperators(S)                             \
    S(None, TokenKind { Null_Value })                 \
    S(AddressOf, TokenKind { Symbol_Value, '&' })     \
    S(Deref, TokenKind { Symbol_Value, '*' })         \
    S(Idempotent, TokenKind { Symbol_Value, '+' })    \
    S(Invert, TokenKind { Symbol_Value, '~' })        \
    S(LogicalNegate, TokenKind { Symbol_Value, '!' }) \
    S(Negate, TokenKind { Symbol_Value, '-' })

enum class UnaryOperator {
#undef S
#define S(Op, ...) Op,
    UnaryOperators(S)
#undef S
};

template<>
inline std::optional<UnaryOperator> decode(std::string_view s, ...)
{
#undef S
#define S(Op, ...)       \
    if (iequals(s, #Op)) \
        return UnaryOperator::Op;
    UnaryOperators(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(UnaryOperator const &v)
{
    switch (v) {
#undef S
#define S(Op, ...)          \
    case UnaryOperator::Op: \
        return #Op;
        UnaryOperators(S)
#undef S
    }
}

struct UnaryOperatorCompatibility {
    UnaryOperator op;
    BasicType     operand;
    BasicType     result;
};

UnaryOperatorCompatibility constexpr unary_operator_compatibility[] = {
    { UnaryOperator::AddressOf, BasicType::Any, BasicType::Int },
    { UnaryOperator::Deref, BasicType::Aggregate, BasicType::Any },
    { UnaryOperator::Idempotent, BasicType::Numeric, BasicType::Self },
    { UnaryOperator::Invert, BasicType::Int, BasicType::Self },
    { UnaryOperator::LogicalNegate, BasicType::Bool, BasicType::Self },
    { UnaryOperator::Negate, BasicType::Numeric, BasicType::Self },
};

uint64_t constexpr unary_operator_compatibility_size = sizeof(unary_operator_compatibility) / sizeof(UnaryOperatorCompatibility);

struct IdempotentOp {
    std::optional<Value> operator()(Value const &v)
    {
        return v.idempotent();
    }
};

struct NegateOp {
    std::optional<Value> operator()(Value const &v)
    {
        return v.negate();
    }
};

struct LogicalNegateOp {
    std::optional<Value> operator()(Value const &v)
    {
        return v.logical_negate();
    }
};

struct InvertOp {
    std::optional<Value> operator()(Value const &v)
    {
        return v.invert();
    }
};

struct UnaryOperatorMapping {
    UnaryOperator op;
    TokenKind     token;

    explicit UnaryOperatorMapping(UnaryOperator op)
        : op(op)
    {
        switch (op) {
#undef S
#define S(Op, ...)           \
    case UnaryOperator::Op:  \
        token = __VA_ARGS__; \
        break;
            UnaryOperators(S)
#undef S
                default : UNREACHABLE();
        }
    }

    explicit UnaryOperatorMapping(TokenKind const &token)
        : token(token)
    {
#undef S
#define S(Op, ...)              \
    if (token == __VA_ARGS__) { \
        op = UnaryOperator::Op; \
        return;                 \
    }
        UnaryOperators(S)
#undef S
            UNREACHABLE();
    }

    std::optional<PrimitiveType> compatible(PrimitiveType const &operand)
    {
        for (auto ix = 0; ix < unary_operator_compatibility_size; ++ix) {
            auto const &compatibility = unary_operator_compatibility[ix];
            if (compatibility.op == op) {
                if (is_a(operand, compatibility.operand)) {
                    switch (compatibility.result) {
                    case BasicType::Numeric:
                    case BasicType::Self:
                        return operand;
                    default:
                        return static_cast<PrimitiveType>(compatibility.result);
                    }
                }
            }
        }
        return {};
    }

    std::optional<Value> operator()(Value const &v)
    {
        switch (op) {
#undef S
#define S(O, ...)          \
    case UnaryOperator::O: \
        return O##Op {}(v);
            UnaryOperators(S)
#undef S
        }
    }
};

}

template<>
struct std::formatter<Arwen::BinaryOperator, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::BinaryOperator op, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(op);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::UnaryOperator, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::UnaryOperator op, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(op);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
