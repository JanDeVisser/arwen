/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <Grammar/Grammar.h>

namespace Arwen {

#define GrammarParserErrors(S) \
    S(MalformedAction)         \
    S(MalformedActionData)     \
    S(MalformedConfigSection)  \
    S(MalformedGrammar)        \
    S(MalformedProduction)     \
    S(SyntaxError)

enum class GrammarParserError {
#undef S
#define S(E) E,
    GrammarParserErrors(S)
#undef S
};

template<>
inline std::optional<GrammarParserError> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return GrammarParserError::S;
    GrammarParserErrors(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(GrammarParserError const &v)
{
    switch (v) {
#undef S
#define S(S)                    \
    case GrammarParserError::S: \
        return #S;
        GrammarParserErrors(S)
#undef S
    }
}

struct GrammarParser {
    Lexer lexer;

    explicit GrammarParser(std::string_view source);
    Error<GrammarParserError>         grammar_config(Grammar &grammar);
    Result<Value, GrammarParserError> parse_value();
    Error<GrammarParserError>         parse_actions(Grammar &grammar, Sequence &seq);
    Error<GrammarParserError>         parse_non_terminal(Grammar &grammar);
    Error<GrammarParserError>         parse(Grammar &grammar);
};

void tests_GrammarParser();

}