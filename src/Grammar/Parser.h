/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <format>
#include <optional>
#include <print>
#include <ranges>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

#include <Lib.h>
#include <Result.h>
#include <Lexer/Lexer.h>
#include <Grammar/Grammar.h>
#include <ScopeGuard.h>

namespace Arwen {

#define ParserErrors(S)  \
    S(CantResolveAction) \
    S(MalformedGrammar)  \
    S(NoEntryPoint)      \
    S(SyntaxError)

enum class ParserError {
#undef S
#define S(E) E,
    ParserErrors(S)
#undef S
};

template<>
inline std::optional<ParserError> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return ParserError::S;
    ParserErrors(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(ParserError const &v)
{
    switch (v) {
#undef S
#define S(S)             \
    case ParserError::S: \
        return #S;
        ParserErrors(S)
#undef S
    }
}

template<typename T>
struct Parser {
    Grammar            &grammar;
    std::vector<Symbol> prod_stack {};
    Token               last_token {};
    bool                log { false };
    T                   impl;

    explicit Parser(Grammar &grammar)
        : grammar(grammar)
        , impl(T {})
    {
    }

    Error<ParserError> parse(std::string_view source, std::string_view buffer = "")
    {
        prod_stack.clear();
        if (!grammar.entry_point) {
            return ParserError::NoEntryPoint;
        }
        prod_stack.emplace_back(*grammar.entry_point);
        Lexer lexer { grammar.lexer, source, buffer };
        impl.startup(buffer);
        ScopeGuard sg {
            [this]() {
                impl.cleanup();
            }
        };
        impl.log = log;
        for (auto token = lexer.next(); token; token = lexer.next()) {
            last_token = *token;
            if (log) {
                std::println("{}", *token);
            }
            bool consumed { false };
            bool done { false };
            while (!done) {
                // if (log) {
                //     std::print("  ");
                //     for (auto &s : prod_stack) {
                //         std::print("{} ", s);
                //     }
                //     std::println("");
                // }
                if (!prod_stack.empty()) {
                    auto s = prod_stack.back();
                    prod_stack.pop_back();
                    switch (s.type()) {
                    case SymbolType::NonTerminal: {
                        auto nt = s.non_terminal();
                        if (consumed) {
                            prod_stack.push_back(s);
                            done = true;
                            break;
                        }
                        if (auto it = grammar.rules.find(nt); it != grammar.rules.end()) {
                            auto &rule = it->second;
                            if (auto itt = rule.parse_table.find(Symbol { token->kind }); itt != rule.parse_table.end()) {
                                Sequence &seq = rule.sequences[itt->second];
                                for (const auto & symbol : std::ranges::reverse_view(seq.symbols)) {
                                    prod_stack.push_back(symbol);
                                }
                            } else if (token->tag() != KindTag::Eof) {
                                if (log) {
                                    std::println("Token: {} rule: {} firsts: {} follows: {}", token->kind, rule, rule.firsts, rule.follows);
                                }
                                std::println("Unexpected token '{}'", *token);
                                rule.dump_parse_table();
                                return ParserError::SyntaxError;
                            }
                        } else {
                            return ParserError::MalformedGrammar;
                        }
                    } break;
                    case SymbolType::Terminal: {
                        auto &t = s.terminal();
                        if (consumed) {
                            prod_stack.push_back(s);
                            done = true;
                            break;
                        }
                        if (t != token->kind) {
                            std::println("Expected '{}', got '{}'", t, token->kind);
                            return ParserError::SyntaxError;
                        }
                        consumed = true;
                    } break;
                    case SymbolType::Action: {
                        auto &action = s.action();
                        if (log) {
                            std::println("Executing action {}", action.full_name);
                        }
                        TRY_FORWARD(ParserError::CantResolveAction, action.call(*this));
                    } break;
                    default:
                        break;
                    }
                } else {
                    if (token->tag() == KindTag::Eof) {
                        return {};
                    }
                    std::println("Production stack underflow");
                    return ParserError::SyntaxError;
                }
            }
        }
        return {};
    }
};

}

template<>
struct std::formatter<Arwen::ParserError, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::ParserError e, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(e);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
