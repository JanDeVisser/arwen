/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

 #include <array>
 #include <format>
 #include <optional>
 #include <print>
 #include <string>
 #include <string_view>

#include <Grammar/Grammar.h>
#include <Lexer/Lexer.h>
#include <GrammarParser/GrammarParser.h>
#include <Lib.h>
#include <Result.h>
#include <Type/Value.h>

namespace Arwen {

GrammarParser::GrammarParser(std::string_view source)
{
    Config config;
    config.Whitespace.on = true;
    config.Keywords.on = true;
    config.QString.on = true;
    config.Comment.on = true;
    config.Identifier.on = true;
    config.Number.signed_numbers = false;
    config.Comment.eol_marker.push_back("//");
    config.Keywords.addAll(std::array<std::string_view, 5> {
        "#binary",
        "#float",
        "#hex",
        "#ident",
        "#int",
    });
    lexer = Lexer { config, source, "Grammar" },
    lexer.ignored.insert(KindTag::Whitespace);
    lexer.ignored.insert(KindTag::Newline);
    lexer.ignored.insert(KindTag::Comment);
}

Error<GrammarParserError> GrammarParser::grammar_config(Grammar &grammar)
{
    TRY_FORWARD(GrammarParserError::ExpectedConfigSection, lexer.expect_symbol('%'));
    for (auto token_maybe = lexer.peek_next(); token_maybe; token_maybe = lexer.peek_next()) {
        auto t = *token_maybe;
        switch (t.tag()) {
        case KindTag::Identifier: {
            auto name = t.text;
            lexer.advance();
            TRY_FORWARD(GrammarParserError::MalformedConfigSection, lexer.expect_symbol(':'));
            if (auto v = lexer.peek_next(); v) {
                std::string_view value;
                switch (v->tag()) {
                case KindTag::Identifier:
                case KindTag::Number:
                case KindTag::Keyword:
                    value = v->text;
                    break;
                case KindTag::String:
                    value = trim(v->text.substr(1, v->text.length() - 2));
                    break;
                default:
                    return GrammarParserError::MalformedConfigSection;
                }
                TRY_FORWARD(GrammarParserError::MalformedConfigSection, grammar.configure(name, value));
                lexer.advance();
            } else {
                return GrammarParserError::MalformedConfigSection;
            }
        } break;
        case KindTag::Symbol:
            if (t.kind.symbol() != '%') {
                return GrammarParserError::MalformedConfigSection;
            }
            lexer.advance();
            return {};
        default:
            return GrammarParserError::MalformedConfigSection;
        }
    }
    return GrammarParserError::MalformedConfigSection;
}

Result<Value, GrammarParserError> GrammarParser::parse_value()
{
    if (auto t = lexer.peek_next(); t) {
        lexer.advance();
        std::string      value_string;
        std::string_view value;
        switch (t->tag()) {
        case KindTag::Identifier:
            value = t->text;
            break;
        case KindTag::String:
            value = trim(t->text.substr(1, t->text.length() - 2));
            break;
        case KindTag::Number:
            value_string = std::format("{}:{}", to_string(t->kind.number_type()), t->text);
            value = value_string;
        default:
            return GrammarParserError::MalformedActionData;
        }
        if (auto decoded = decode<Value>(value); decoded) {
            return *decoded;
        } else {
            return GrammarParserError::MalformedActionData;
        }
    }
    return GrammarParserError::MalformedActionData;
}

Error<GrammarParserError> GrammarParser::parse_actions(Grammar &grammar, Sequence &seq)
{
    TRY_FORWARD(GrammarParserError::ExpectedAction, lexer.expect_symbol('['));
    for (auto t = lexer.peek_next(); t; t = lexer.peek_next()) {
        switch (t->tag()) {
        case KindTag::Symbol: {
            switch (t->kind.symbol()) {
            case ']':
                lexer.advance();
                return {};
            default:
                return GrammarParserError::MalformedAction;
            }
        }
        case KindTag::Identifier: {
            auto name = t->text;
            lexer.advance();
            Value data {};
            if (lexer.accept_symbol(':')) {
                data = TRY_EVAL(parse_value());
            }
            seq.symbols.emplace_back(GrammarAction { name, data });
        } break;
        default:
            return GrammarParserError::MalformedAction;
        }
    }
    return GrammarParserError::MalformedAction;
}

Error<GrammarParserError> GrammarParser::parse_non_terminal(Grammar &grammar)
{
    auto name = TRY_EVAL_FORWARD(GrammarParserError::ExpectedNonTerminal, lexer.expect_identifier());
    TRY_FORWARD(GrammarParserError::MalformedProduction, lexer.expect_symbol(':'));
    TRY_FORWARD(GrammarParserError::MalformedProduction, lexer.expect_symbol('='));
    Rule     rule { grammar, name.text };
    Sequence seq { grammar };
    auto done {false};
    for (auto t = lexer.peek_next(); t && !done; t = lexer.peek_next()) {
        switch (t->tag()) {
        case KindTag::Symbol:
            switch (t->kind.symbol()) {
            case '[':
                TRY(parse_actions(grammar, seq));
                break;
            case ';':
                done = true;
                break;
            case '|':
                rule.sequences.push_back(seq);
                seq.symbols.clear();
                lexer.advance();
                break;
            default:
                seq.symbols.emplace_back(TokenKind { KindTag::Symbol, t->kind.symbol() } );
                lexer.advance();
                break;
            }
            break;
        case KindTag::Identifier:
            seq.symbols.emplace_back(t->text);
            lexer.advance();
            break;
        case KindTag::Keyword: {
            auto kw = t->kind.keyword();
            if (kw == "#ident") {
                seq.symbols.emplace_back(TokenKind { KindTag::Identifier });
            } else if (kw == "#int") {
                seq.symbols.emplace_back(TokenKind { KindTag::Number, NumberType::Int });
            } else if (kw == "#hex") {
                seq.symbols.emplace_back(TokenKind { KindTag::Number, NumberType::Hex });
            } else if (kw == "#binary") {
                seq.symbols.emplace_back(TokenKind { KindTag::Number, NumberType::Binary });
            } else if (kw == "#float") {
                seq.symbols.emplace_back(TokenKind { KindTag::Number, NumberType::Float });
            } else {
                return GrammarParserError::MalformedProduction;
            }
            lexer.advance();
        } break;
        case KindTag::String:
            switch (t->kind.quote()) {
            case '\'':
                switch (t->text[1]) {
                case '"':
                case '\'':
                    seq.symbols.emplace_back(TokenKind { KindTag::String, t->text[1] });
                    break;
                default:
                    seq.symbols.emplace_back(TokenKind { KindTag::Symbol, t->text[1] });
                    break;
                }
                lexer.advance();
                break;
            case '"': {
                auto kw = t->text.substr(1, t->text.length() - 2);
                grammar.lexer.Keywords.add(kw);
                seq.symbols.emplace_back(TokenKind { KindTag::Keyword, kw });
                lexer.advance();
            } break;
            default:
                return GrammarParserError::MalformedProduction;
            }
            break;
        case KindTag::Eof:
            break;
        default:
            std::print("Unexpected token '{}'", *t);
            return GrammarParserError::MalformedProduction;
        }
    }

    rule.sequences.push_back(seq);
    grammar.rules.emplace(rule.non_terminal, rule);
    if (grammar.rules.size() == 1) {
        grammar.entry_point = rule.non_terminal;
    }
    lexer.advance();
    return {};
}

Error<GrammarParserError> GrammarParser::parse(Grammar &grammar)
{
    for (auto t = lexer.peek_next(); t; t = lexer.peek_next()) {
        switch (t->tag()) {
        case KindTag::Symbol:
            switch (t->kind.symbol()) {
            case '%':
                TRY(grammar_config(grammar));
                break;
            default:
            return GrammarParserError::UnexpectedSymbol;
            }
            break;
        case KindTag::Identifier:
            TRY(parse_non_terminal(grammar));
            break;
        case KindTag::Eof:
            if (auto res = grammar.build_parse_table(); res.is_error()) {
                std::println("{}", res.error());
                return GrammarParserError::MalformedGrammar;
            }
            return {};
        case KindTag::Keyword:
            return GrammarParserError::UnexpectedKeyword;
        default:
            std::println("{}", t->kind);
            return GrammarParserError::SyntaxError;
        }
    }
    return {};
}

void test_Grammar_Parser()
{
    auto g = R"(
program := [a] declarations [b] ;
declarations := declaration declarations | ;
declaration := "A" | "B" ;
)";

    GrammarParser gp { g };
    Grammar grammar {};
    gp.parse(grammar).must();
    grammar.dump();
}

void tests_GrammarParser()
{
    test_Grammar_Parser();
}

}
