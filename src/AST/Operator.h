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
#include <Lib.h>
#include <Logging.h>
#include <Type/Type.h>
#include <utility>

namespace Arwen {

#define BinaryOperators(S)                                       \
    S(None, true, TokenKind { KindTag::Null })                   \
    S(Add, true, TokenKind { KindTag::Symbol, '+' })             \
    S(Assign, false, TokenKind { KindTag::Symbol, '=' })         \
    S(BinaryAnd, true, TokenKind { KindTag::Symbol, '&' })       \
    S(BinaryOr, true, TokenKind { KindTag::Symbol, '|' })        \
    S(BinaryXor, true, TokenKind { KindTag::Symbol, '^' })       \
    S(Call, false, TokenKind { KindTag::Null })                  \
    S(Divide, true, TokenKind { KindTag::Symbol, '/' })          \
    S(Equal, false, TokenKind { KindTag::Keyword, "==" })        \
    S(Greater, false, TokenKind { KindTag::Symbol, '>' })        \
    S(GreaterEqual, false, TokenKind { KindTag::Keyword, ">=" }) \
    S(Less, false, TokenKind { KindTag::Symbol, '<' })           \
    S(LessEqual, false, TokenKind { KindTag::Keyword, "<=" })    \
    S(LogicalAnd, false, TokenKind { KindTag::Keyword, "&&" })   \
    S(LogicalOr, false, TokenKind { KindTag::Keyword, "||" })    \
    S(MemberAccess, true, TokenKind { KindTag::Symbol, '.' })    \
    S(Modulo, true, TokenKind { KindTag::Symbol, '%' })          \
    S(Multiply, true, TokenKind { KindTag::Symbol, '*' })        \
    S(NotEqual, false, TokenKind { KindTag::Keyword, "!=" })     \
    S(Shl, true, TokenKind { KindTag::Keyword, "<<" })           \
    S(Shr, true, TokenKind { KindTag::Keyword, ">>" })           \
    S(Subtract, true, TokenKind { KindTag::Symbol, '-' })        \
    S(Subscript, false, TokenKind { KindTag::Symbol, '[' })

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

    std::optional<PrimitiveType> compatible(PrimitiveType const &lhs, PrimitiveType const &rhs) const
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

    std::optional<Value> operator()(Value const &v1, Value const &v2) const
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

#define UnaryOperators(S)                                \
    S(None, TokenKind { KindTag::Null })                 \
    S(AddressOf, TokenKind { KindTag::Symbol, '&' })     \
    S(Deref, TokenKind { KindTag::Symbol, '*' })         \
    S(Idempotent, TokenKind { KindTag::Symbol, '+' })    \
    S(Invert, TokenKind { KindTag::Symbol, '~' })        \
    S(LogicalNegate, TokenKind { KindTag::Symbol, '!' }) \
    S(Negate, TokenKind { KindTag::Symbol, '-' })

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

    std::optional<PrimitiveType> compatible(PrimitiveType const &operand) const
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

    std::optional<Value> operator()(Value const &v) const
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
