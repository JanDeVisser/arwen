/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <optional>

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
    S(Break, "break")          \
    S(Cast, "::")              \
    S(Const, "const")          \
    S(Continue, "continue")    \
    S(Defer, "defer")          \
    S(Else, "else")            \
    S(Embed, "@embed")         \
    S(Enum, "enum")            \
    S(Equals, "==")            \
    S(Error, "error")          \
    S(ExternLink, "->")        \
    S(False, "false")          \
    S(For, "for")              \
    S(Func, "func")            \
    S(GreaterEqual, ">=")      \
    S(If, "if")                \
    S(Import, "import")        \
    S(Include, "@include")     \
    S(LessEqual, "<=")         \
    S(LogicalAnd, "&&")        \
    S(LogicalOr, "||")         \
    S(Loop, "loop")            \
    S(NotEqual, "!=")          \
    S(Null, "null")            \
    S(Public, "public")        \
    S(Range, "..")             \
    S(Return, "return")        \
    S(ShiftLeft, "<<")         \
    S(ShiftRight, ">>")        \
    S(Sizeof, "#::")           \
    S(Struct, "struct")        \
    S(True, "true")            \
    S(While, "while")          \
    S(Yield, "yield")

enum class ArwenKeyword {
#undef S
#define S(KW, S) KW,
    ArwenKeywords(S)
#undef S
};

struct ArwenError {
    TokenLocation location;
    std::wstring  message;
};

extern char const *ArwenKeyword_name(ArwenKeyword kw);

}

namespace Util {

using namespace Arwen;

using ArwenKeywordMatch = KeywordMatch<ArwenKeyword>;

template<>
[[nodiscard]] inline std::optional<ArwenKeywordMatch> match_keyword(std::string const &str)
{
#undef S
#define S(KW, STR)                                                                                                                      \
    {                                                                                                                                   \
        std::string_view kw_str { STR };                                                                                                \
        if (kw_str.starts_with(str)) {                                                                                                  \
            return ArwenKeywordMatch {                                                                                                  \
                ArwenKeyword::KW,                                                                                                       \
                (str.length() == kw_str.length()) ? ArwenKeywordMatch::MatchType::FullMatch : ArwenKeywordMatch::MatchType::PrefixMatch \
            };                                                                                                                          \
        }                                                                                                                               \
    }
    ArwenKeywords(S)
#undef S
        return {};
}

}
