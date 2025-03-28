/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <optional>
#include <tuple>
#include <variant>

#include <Util/Lexer.h>

namespace Arwen {

using namespace Util;

using Precedence = uint16_t;

#define ArwenKeywords(S)       \
    S(AssignAnd, "&=")         \
    S(AssignDecrement, "-=")   \
    S(AssignDivide, "/=")      \
    S(AssignIncrement, "+=")   \
    S(AssignModulo, "%=")      \
    S(AssignMultiply, "*=")    \
    S(AssignOr, "|=")          \
    S(AssignShiftLeft, "<<=")  \
    S(AssignShiftRight, ">>=") \
    S(AssignXor, "^=")         \
    S(Embed, "@embed")         \
    S(Equals, "==")            \
    S(GreaterEqual, ">=")      \
    S(LessEqual, "<=")         \
    S(LogicalAnd, "&&")        \
    S(LogicalOr, "||")         \
    S(NotEqual, "!=")          \
    S(ShiftLeft, "<<")         \
    S(ShiftRight, ">>")        \
    S(Break, "break")          \
    S(Continue, "continue")    \
    S(Defer, "defer")          \
    S(Func, "func")            \
    S(If, "if")                \
    S(Else, "else")            \
    S(Loop, "loop")            \
    S(Return, "return")        \
    S(While, "while")

enum class ArwenKeyword {
#undef S
#define S(KW, S) KW,
    ArwenKeywords(S)
#undef S
};

enum class Position {
    Prefix,
    Infix,
    Postfix,
};

enum class Associativity {
    Left,
    Right,
};

#define Operators(S)    \
    S(Add)              \
    S(Assign)           \
    S(AssignAnd)        \
    S(AssignDecrement)  \
    S(AssignDivide)     \
    S(AssignIncrement)  \
    S(AssignModulo)     \
    S(AssignMultiply)   \
    S(AssignOr)         \
    S(AssignShiftLeft)  \
    S(AssignShiftRight) \
    S(AssignXor)        \
    S(BinaryAnd)        \
    S(BinaryOr)         \
    S(BinaryXor)        \
    S(Call)             \
    S(Divide)           \
    S(Equals)           \
    S(Greater)          \
    S(GreaterEqual)     \
    S(Idempotent)       \
    S(Invert)           \
    S(Less)             \
    S(LessEqual)        \
    S(LogicalAnd)       \
    S(LogicalOr)        \
    S(Modulo)           \
    S(Multiply)         \
    S(Negate)           \
    S(NotEqual)         \
    S(Sequence)         \
    S(ShiftLeft)        \
    S(ShiftRight)       \
    S(Subtract)

enum class Operator : int {
#undef S
#define S(O) O,
    Operators(S)
#undef S
};

using OperatorSymbol = std::variant<wchar_t, ArwenKeyword>;

struct OperatorDef {
    Operator       op;
    OperatorSymbol sym;
    Precedence     precedence;
    Position       position { Position::Infix };
    Associativity  associativity { Associativity::Left };
};

extern char const *ArwenKeyword_name(ArwenKeyword kw);
extern char const *Operator_name(Operator op);

}

namespace Util {

using namespace Arwen;

using ArwenKeywordMatch = KeywordMatch<ArwenKeyword>;

template<>
[[nodiscard]] inline std::optional<ArwenKeywordMatch> match_keyword(std::string const &str)
{
#undef S
#define S(KW, STR)                                                                                             \
    if (std::string_view(STR).starts_with(str)) {                                                              \
        return ArwenKeywordMatch {                                                                                    \
            ArwenKeyword::KW,                                                                                  \
            (str == STR) ? ArwenKeywordMatch::MatchType::FullMatch : ArwenKeywordMatch::MatchType::PrefixMatch \
        };                                                                                                     \
    }
    ArwenKeywords(S)
#undef S
        return {};
}

}
