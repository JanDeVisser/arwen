//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <Lexer/Lexer.h>
#include <Unescape.h>

namespace Arwen {

Lexer::Lexer(Config &config, std::string_view source)
    : config(config)
    , source(source)
{
    if (config.Whitespace.on) {
        if (config.Whitespace.ignore_nl) {
            ignored.insert(KindTag::Newline);
        }
        if (config.Whitespace.ignore_ws) {
            ignored.insert(KindTag::Whitespace);
        }
    }
    if (config.Comment.ignore) {
        ignored.insert(KindTag::Comment);
    }
}

std::string_view Lexer::unescape(std::string_view s)
{
    if (auto unesc = Arwen::unescape(s); unesc.has_value() && unesc.value()) {
        unescaped_strings.push_back(*unesc.value());
        return unescaped_strings[this->unescaped_strings.size() - 1];
    }
    return s;
}

Token Lexer::buildToken(uint64_t len, Arwen::TokenKind kind)
{
    current = Token { kind, unescape((source.length() == 0 || len == 0) ? "" : source.substr(0, len)) };
    current->location = location;
    current->raw_text = (source.length() == 0 or len == 0) ? "" : source.substr(0, len);
    return *current;
}

void Lexer::advance()
{
    if (!current) {
        UNREACHABLE();
    }
    Token token = *current;
    location.pos += token.raw_text.length();
    source = source.substr(token.raw_text.length());
    current = {};
}

std::optional<Token> Lexer::next()
{
    if (auto t = peek_next(); t) {
        advance();
        return t;
    }
    return {};
}

std::optional<Token> Lexer::peek_next()
{
    while (true) {
        auto t = peek();
        if (!t) {
            return {};
        }
        if (!ignored.contains(t->kind.tag())) {
            return t;
        }
        advance();
    }
}

std::optional<Token> Lexer::peek()
{
    if (current) {
        return current;
    }
    if (exhausted) {
        return {};
    }
    if (source.empty()) {
        exhausted = true;
        return buildToken(0, TokenKind { KindTag::Eof });
    }
    if (source[0] == '\n') {
        auto ret = buildToken(1, TokenKind { KindTag::Newline });
        location.line += 1;
        location.col = 0;
        if (config.Whitespace.on) {
            return ret;
        }
    }
    if (config.Comment.on) {
        if (auto t = config.Comment.scan(*this); t) {
            return t;
        }
    }
    if (isspace(source[0])) {
        size_t p = 1;
        while (p < source.length() && isspace(source[p])) {
            p += 1;
        }
        auto ret = buildToken(1, TokenKind { KindTag::Whitespace });
        if (config.Whitespace.on) {
            return ret;
        }
    }
    if (config.Number.on) {
        if (auto t = config.Number.scan(*this); t) {
            return t;
        }
    }
    if (config.QString.on) {
        if (auto q = config.QString.quotes.find(source[0]); q != std::string_view::npos) {
            char   quote = config.QString.quotes[q];
            size_t p = 1;
            while (p < source.length() && source[p] != quote) {
                p += (source[p] == '\\') ? 2 : 1;
            }
            if (p < source.length()) {
                p += 1;
            }
            return buildToken(p, TokenKind { KindTag::String, quote });
        }
    }
    if (isalpha(source[0]) || source[0] == '_') {
        size_t p = 1;
        while (p < source.length() && (isalnum(source[p]) || source[p] == '_')) {
            p += 1;
        }
        if (config.Keywords.on) {
            switch (config.Keywords.match(source.substr(0, p))) {
            case Config::Keywords::MatchResult::ExactMatch:
            case Config::Keywords::MatchResult::PrefixAndExact:
                return buildToken(p, TokenKind { KindTag::Keyword, source.substr(0, p) } );
            default:
                break;
            }
        }
        if (config.Identifier.on) {
            return buildToken(p, TokenKind { KindTag::Identifier });
        }
    }
    if (config.Keywords.on) {
        size_t matched = 0;
        for (auto l = 1; l < source.length(); ++l) {
            switch (config.Keywords.match(source.substr(0, l))) {
            case Config::Keywords::MatchResult::ExactMatch:
                return buildToken(l, TokenKind { KindTag::Keyword, source.substr(0, l) } );
            case Config::Keywords::MatchResult::NoMatch:
                goto default_return;
            case Config::Keywords::MatchResult::Prefix:
                break;
            case Config::Keywords::MatchResult::PrefixAndExact:
                matched = l;
                break;
            case Config::Keywords::MatchResult::MatchLost:
                return buildToken(matched, TokenKind { KindTag::Keyword, source.substr(0, matched) });
            }
        }
    }
default_return:
    return buildToken(1, TokenKind { KindTag::Symbol, source[0] });
}

bool Lexer::accept_keyword(std::string_view keyword)
{
    if (auto t = peek_next(); t) {
        if (t->is(KindTag::Keyword) && t->kind.keyword() == keyword) {
            advance();
            return true;
        }
    }
    return false;
}

Result<Token, Lexer::Error> Lexer::expect_identifier()
{
    if (auto t = peek_next(); t) {
        if (t->is(KindTag::Identifier)) {
            advance();
            return *t;
        }
    }
    return Error::ExpectedIdentifier;
}

std::optional<Token> Lexer::accept_identifier()
{
    if (auto t = peek_next(); t) {
        if (t->is(KindTag::Identifier)) {
            advance();
            return *t;
        }
    }
    return {};
}

Error<Lexer::Error> Lexer::expect_symbol(char symbol)
{
    if (auto t = peek_next(); t) {
        if (t->is(KindTag::Symbol) && t->kind.symbol() == symbol) {
            advance();
            return {};
        }
    }
    return Error::ExpectedIdentifier;
}

bool Lexer::accept_symbol(char symbol)
{
    if (auto t = peek_next(); t) {
        if (t->is(KindTag::Symbol) && t->kind.symbol() == symbol) {
            advance();
            return true;
        }
    }
    return false;
}

}
