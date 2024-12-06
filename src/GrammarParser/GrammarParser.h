/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <format>
#include <optional>
#include <sstream>
#include <string_view>
#include <utility>

#include <Grammar/Grammar.h>
#include <Lexer/Lexer.h>
#include <Lib.h>
#include <Result.h>
#include <Type/Value.h>

namespace Arwen {

#define GrammarParserErrors(S) \
    S(ExpectedConfigSection)   \
    S(ExpectedAction)          \
    S(ExpectedNonTerminal)     \
    S(MalformedAction)         \
    S(MalformedActionData)     \
    S(MalformedConfigSection)  \
    S(MalformedGrammar)        \
    S(MalformedProduction)     \
    S(SyntaxError) \
    S(UnexpectedKeyword) \
    S(UnexpectedSymbol)

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

template<>
struct std::formatter<Arwen::GrammarParserError, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::GrammarParserError e, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(e);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
