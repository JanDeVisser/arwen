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
#include <functional>
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
    { BinaryOperator::BinaryAnd, BasicType::Integer, BasicType::Lhs, BasicType::Lhs },
    { BinaryOperator::BinaryOr, BasicType::Integer, BasicType::Lhs, BasicType::Lhs },
    { BinaryOperator::BinaryXor, BasicType::Integer, BasicType::Lhs, BasicType::Lhs },
    { BinaryOperator::Shl, BasicType::Integer, BasicType::Integer, BasicType::Lhs },
    { BinaryOperator::Shr, BasicType::Integer, BasicType::Integer, BasicType::Lhs },
    { BinaryOperator::LogicalAnd, BasicType::Bool, BasicType::Bool, BasicType::Bool },
    { BinaryOperator::LogicalOr, BasicType::Bool, BasicType::Bool, BasicType::Bool },
    { BinaryOperator::Equal, BasicType::Any, BasicType::Lhs, BasicType::Bool },
    { BinaryOperator::NotEqual, BasicType::Any, BasicType::Lhs, BasicType::Bool },
    { BinaryOperator::Less, BasicType::Any, BasicType::Lhs, BasicType::Bool },
    { BinaryOperator::LessEqual, BasicType::Any, BasicType::Lhs, BasicType::Bool },
    { BinaryOperator::Greater, BasicType::Any, BasicType::Lhs, BasicType::Bool },
    { BinaryOperator::GreaterEqual, BasicType::Any, BasicType::Lhs, BasicType::Bool },
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
        return v1.binary_and(v2);
    }
};

struct BinaryOrOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.binary_or(v2);
    }
};

struct BinaryXorOp {
    std::optional<Value> operator()(Value const &v1, Value const &v2)
    {
        return v1.binary_xor(v2);
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
        auto ret { v1 == v2 };
        return ret;
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
        auto ret { v1 < v2 };
        return ret;
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
        auto lhs_type = TypeRegistry::the()[lhs].decay();
        auto rhs_type = TypeRegistry::the()[rhs].decay();
        for (auto ix = 0; ix < binary_operator_compatibility_size; ++ix) {
            auto const &compatibility = binary_operator_compatibility[ix];
            if (compatibility.op == op) {
                if (is_a(lhs_type.ref, compatibility.lhs)) {
                    if ((compatibility.rhs == BasicType::Lhs)
                            ? is_a(rhs_type.ref, static_cast<BasicType>(lhs_type.ref))
                            : is_a(rhs_type.ref, compatibility.rhs)) {
                        switch (compatibility.result) {
                        case BasicType::Numeric: {
                            if (lhs_type.is_float() || rhs_type.is_float()) {
                                return (lhs == PrimitiveType::Double || rhs == PrimitiveType::Double)
                                    ? PrimitiveType::Double
                                    : PrimitiveType::Float;
                            }
                            if (lhs_type.is_unsigned() && rhs_type.is_unsigned()) {
                                return (lhs >= rhs) ? lhs : rhs;
                            }
                            if (lhs_type.is_signed() && rhs_type.is_signed()) {
                                return (lhs >= rhs) ? lhs : rhs;
                            }
                            auto ret = std::max(lhs, rhs);
                            if (TypeRegistry::the()[ret].is_unsigned() && ret != PrimitiveType::U64) {
                                ret = static_cast<PrimitiveType>(static_cast<uint64_t>(ret) + 1);
                            }
                        }
                        case BasicType::Lhs:
                            return lhs;
                        case BasicType::Rhs:
                            return rhs;
                        default:
                            return static_cast<PrimitiveType>(compatibility.result);
                        }
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

inline std::optional<Value> None(Value const &v)
{
    return {};
}

inline std::optional<Value> AddressOf(Value const &v)
{
    return {};
}

inline std::optional<Value> Deref(Value const &v)
{
    return {};
}

inline std::optional<Value> Idempotent(Value const &v)
{
    return v.idempotent();
}

inline std::optional<Value> Negate(Value const &v)
{
    return v.negate();
}

inline std::optional<Value> LogicalNegate(Value const &v)
{
    return v.logical_negate();
}

inline std::optional<Value> Invert(Value const &v)
{
    return v.invert();
}

inline std::optional<PrimitiveType> compatible(UnaryOperator op, PrimitiveType const &operand)
{
    auto type = TypeRegistry::the()[operand].decay();
    switch (op) {
    case UnaryOperator::AddressOf:
        return PrimitiveType::Ptr;
    case UnaryOperator::Deref:
        return {};
    case UnaryOperator::Idempotent:
        if (type.is_numeric()) {
            return operand;
        }
        return {};
    case UnaryOperator::Invert:
        if (type.is_integer()) {
            return operand;
        }
        return {};
    case UnaryOperator::LogicalNegate:
        if (operand == PrimitiveType::Bool) {
            return operand;
        }
        return {};
    case UnaryOperator::Negate:
        if (!type.is_numeric()) {
            return {};
        }
        if (type.is_float() || type.is_signed() || operand == PrimitiveType::U64) {
            return operand;
        }
        return static_cast<PrimitiveType>(static_cast<uint64_t>(operand) + 1);
    default:
        return {};
    }
}

struct UnaryOperatorMapping {
    UnaryOperator                                      op;
    std::function<std::optional<Value>(Value const &)> handler;
    TokenKind                                          token;

    explicit UnaryOperatorMapping(UnaryOperator op)
        : op(op)
    {
        switch (op) {
#undef S
#define S(Operator, ...)          \
    case UnaryOperator::Operator: \
        handler = Operator;       \
        token = __VA_ARGS__;      \
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
        handler = Op;           \
        op = UnaryOperator::Op; \
        return;                 \
    }
        UnaryOperators(S)
#undef S
            UNREACHABLE();
    }

    std::optional<Value> operator()(Value const &v) const
    {
        return handler(v);
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
