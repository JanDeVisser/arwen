/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <Lexer/Lexer.h>

namespace Arwen {

#define BinaryOperators(S)                                       \
    S(Add, true, TokenKind { KindTag::Symbol, '+' })             \
    S(Assign, false, TokenKind { KindTag::Symbol, '=' })         \
    S(BinaryAnd, true, TokenKind { KindTag::Symbol, '&' })       \
    S(BinaryOr, true, TokenKind { KindTag::Symbol, '|' })        \
    S(BinaryXor, true, TokenKind { KindTag::Symbol, '^' })       \
    S(Divide, true, TokenKind { KindTag::Symbol, '/' })          \
    S(Equal, false, TokenKind { KindTag::Keyword, "==" })        \
    S(Greater, false, TokenKind { KindTag::Symbol, '>' })        \
    S(GreaterEqual, false, TokenKind { KindTag::Keyword, ">=" }) \
    S(LeftShift, true, TokenKind { KindTag::Keyword, "<<" })     \
    S(Less, false, TokenKind { KindTag::Symbol, '<' })           \
    S(LessEqual, false, TokenKind { KindTag::Keyword, "<=" })    \
    S(LogicalAnd, false, TokenKind { KindTag::Keyword, "&&" })   \
    S(LogicalOr, false, TokenKind { KindTag::Keyword, "||" })    \
    S(Modulo, true, TokenKind { KindTag::Symbol, '%' })          \
    S(Multiply, true, TokenKind { KindTag::Symbol, '*' })        \
    S(NotEqual, false, TokenKind { KindTag::Keyword, "!=" })     \
    S(RightShift, true, TokenKind { KindTag::Keyword, ">>" })    \
    S(Subtract, true, TokenKind { KindTag::Symbol, '*' })

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
};

#define UnaryOperators(S)                                \
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
    if (token == __VA_ARGS__) {       \
        op = UnaryOperator::Op; \
        return;                 \
    }
        UnaryOperators(S)
#undef S
            UNREACHABLE();
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
