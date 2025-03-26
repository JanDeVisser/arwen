/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Util/Logging.h"
#include <cctype>
#include <deque>
#include <format>
#include <string>
#include <string_view>

#include <Util/Result.h>
#include <Util/StringUtil.h>
#include <Util/Token.h>
#include <Util/Utf8.h>
#include <variant>

namespace Util {

struct LexerErrorMessage {
    TokenLocation location;
    std::string   message;

    [[nodiscard]] std::string const &to_string() const
    {
        return message;
    }
};

struct SkipToken { };

template<typename Buffer>
struct NoKeywords {
    using Keyword = NoKeywordCode;
    using Token = GenericToken<Keyword>;
    using PeekResult = std::variant<Token, Buffer, SkipToken>;

    struct MatchResult {
        PeekResult result;
        size_t     matched;
    };

    std::optional<MatchResult> pre_match(Buffer const &, size_t)
    {
        return {};
    }

    PeekResult post_match(Buffer const &, Token const &)
    {
        return {};
    }

    std::optional<Keyword> match(std::string const &)
    {
        return {};
    }

    std::optional<std::tuple<Keyword, size_t>> match(Buffer const &, size_t)
    {
        return {};
    }
};

enum class MatchType {
    PrefixMatch,
    FullMatch,
};

template<typename Keyword>
std::optional<std::tuple<Keyword, MatchType>> match_keyword(std::string const &str)
{
    return {};
}

template<typename Buffer, typename KW>
struct EnumKeywords {
    using Keyword = KW;
    using Token = GenericToken<Keyword>;
    using PeekResult = std::variant<Token, Buffer, SkipToken>;

    struct MatchResult {
        PeekResult result;
        size_t     matched;
    };

    std::optional<MatchResult> pre_match(Buffer const &, size_t)
    {
        return {};
    }

    PeekResult post_match(Buffer const &, Token const &)
    {
        return {};
    }

    std::optional<Keyword> match(std::string const &str)
    {
        if (auto m = match_keyword<Keyword>(str); m && std::get<MatchType>(*m) == MatchType::FullMatch) {
            return std::get<Keyword>(*m);
        }
        return {};
    }

    std::optional<std::tuple<Keyword, size_t>> match(Buffer const &buffer, size_t index)
    {
        std::string scanned;
        for (auto ix = index; ix < buffer.length(); ++ix) {
            scanned += buffer[ix];
            if (auto m = match_keyword<Keyword>(scanned)) {
                if (std::get<MatchType>(*m) == MatchType::FullMatch) {
                    return std::tuple { std::get<Keyword>(*m), scanned.length() };
                }
            } else {
                return {};
            }
        }
        return {};
    }

    std::string_view get_scope(Token const &)
    {
        return "identifier";
    }
};

template<bool Whitespace = false, bool Comments = false, bool BackquotedStrings = false>
struct LexerConfig {
    bool whitespace { Whitespace };
    bool comments { Comments };
    bool backquotedStrings { BackquotedStrings };
};

template<typename Buffer, typename Matcher = NoKeywords<Buffer>, typename Char = wchar_t, bool Whitespace = false, bool Comments = false, bool BackquotedStrings = false>
class Lexer {
public:
    using LexerError = Error<LexerErrorMessage>;
    using Keyword = typename Matcher::Keyword;
    using Token = GenericToken<Keyword>;
    using LexerResult = Result<Token, LexerErrorMessage>;
    using PeekResult = typename Matcher::PeekResult;
    using MatchResult = typename Matcher::MatchResult;

    Lexer() = default;

    void push_source(Buffer source)
    {
        m_sources.emplace_back(this, std::move(source));
        if constexpr (!BackquotedStrings) {
            m_sources.back().quote_chars = "\"'";
        }
    }

    std::basic_string_view<Char> text(Token const &token) const
    {
        return m_sources.back().substr(token.location.index, token.location.length);
    }

    std::string text_utf8(Token const &token) const
    {
        return as_utf8(text(token));
    }

    Token const &peek()
    {
        if (m_current.has_value()) {
            trace(LEXER, "lexer.peek() -> {} [cached]", *m_current);
            return m_current.value();
        }
        if (!pushed_back.empty()) {
            m_current = pushed_back.back();
            return m_current.value();
        }
        while (!exhausted()) {
            TokenKind k;
            while (true) {
                PeekResult res = m_sources.back().peek_next();
                if (res.index() == 2) { // Skip it
                    continue;
                } else if (res.index() == 1) { // Push a new source buffer
                    push_source(std::get<Buffer>(res));
                    continue;
                } else {
                    m_current = std::get<Token>(res);
                    trace(LEXER, "lexer.peek() -> {} [source.peek_next()]", *m_current);
                    k = m_current->kind;
                    if constexpr (!Whitespace) {
                        if (k == TokenKind::Whitespace || k == TokenKind::Tab || k == TokenKind::EndOfLine) {
                            lex();
                            trace(LEXER, "skip it");
                            continue;
                        }
                    }
                    if constexpr (!Comments) {
                        if (k == TokenKind::Comment) {
                            lex();
                            trace(LEXER, "skip it");
                            continue;
                        }
                    }
                }
                break;
            }
            if (k != TokenKind::EndOfFile) {
                break;
            }
            m_sources.pop_back();
        }
        trace(LEXER, "lexer.peek() -> {} [caching it]", *m_current);
        return m_current.value();
    }

    Token lex()
    {
        auto ret = peek();
        if (!pushed_back.empty()) {
            pushed_back.pop_back();
        } else if (!m_sources.empty()) {
            m_sources.back().lex();
        }
        m_current.reset();
        return ret;
    }

    LexerResult expect(TokenKind kind)
    {
        if (auto ret = peek(); !ret.matches(kind)) {
            return LexerErrorMessage { location(),
                std::format("Expected '{}'", TokenKind_name(kind)) };
        }
        return lex();
    }

    bool accept(TokenKind kind)
    {
        if (auto ret = peek(); ret.matches(kind)) {
            lex();
            return true;
        }
        return false;
    }

    LexerError expect_keyword(Keyword code)
    {
        if (auto ret = peek(); !ret.matches_keyword(code)) {
            return LexerErrorMessage {
                location(),
                std::format("Expected keyword"), // FIXME need KW code -> text mechanism
            };
        }
        lex();
        return {};
    }

    bool accept_keyword(Keyword code)
    {
        if (auto ret = peek(); ret.matches_keyword(code)) {
            lex();
            return true;
        }
        return false;
    }

    LexerError expect_symbol(int symbol)
    {
        if (auto ret = peek(); !ret.matches_symbol(symbol)) {
            return LexerErrorMessage { location(), std::format("Expected '{}' but got '{}'", static_cast<char>(symbol), text_utf8(ret)) };
        }
        lex();
        return {};
    }

    bool accept_symbol(int symbol)
    {
        if (auto ret = peek(); ret.matches_symbol(symbol)) {
            lex();
            return true;
        }
        return false;
    }

    LexerResult expect_identifier()
    {
        if (auto ret = peek(); !ret.is_identifier()) {
            return LexerErrorMessage { location(), "Expected identifier" };
        }
        return lex();
    }

    bool next_matches(TokenKind kind)
    {
        auto n = peek();
        return n.matches(kind);
    }

    bool next_matches(int symbol)
    {
        auto n = peek();
        return n.matches_symbol(symbol);
    }

    [[nodiscard]] bool exhausted() const
    {
        return m_sources.empty();
    }

    void push_back(Token token)
    {
        pushed_back.emplace_back(std::move(token));
        m_current.reset();
    }

private:
    std::optional<Token> m_current {};

    [[nodiscard]] TokenLocation const &location() const
    {
        assert(!m_sources.empty());
        return m_sources.back().location();
    }

    class Source {
    public:
        char const *quote_chars;

        [[nodiscard]] TokenLocation const &location() const
        {
            return m_location;
        }

        Source(Lexer *lexer, Buffer const &src)
            : quote_chars("\"'`")
            , m_buffer(src)
            , m_lexer(lexer)
        {
        }

        PeekResult peek_next()
        {
            if (m_current.has_value()) {
                return *m_current;
            }

            auto res = peek();
            return std::visit(overloads {
                                  [this](Token const &token) -> PeekResult {
                                      m_current = token;
                                      return PeekResult { *m_current };
                                  },
                                  [this, &res](Buffer const &) -> PeekResult {
                                      return res;
                                  },
                                  [this, &res](SkipToken const &) -> PeekResult {
                                      return res;
                                  },
                              },
                res);
        }

        PeekResult peek()
        {
            if (m_index >= m_buffer.length()) {
                return Token::end_of_file();
            }
            if (auto res = matcher.pre_match(m_buffer, m_index); res) {
                m_index += res->matched;
                return res->result;
            }
            auto match_token = [this]() -> Token {
                m_scanned.clear();
                auto cur = m_buffer[m_index];
                if (m_in_comment) {
                    if (cur == '\n') {
                        ++m_index;
                        return Token::end_of_line();
                    }
                    return block_comment();
                }
                if (cur == '/') {
                    switch (m_buffer[m_index + 1]) {
                    case '/': {
                        m_index += 2;
                        for (; m_index < m_buffer.length() && m_buffer[m_index] != '\n'; ++m_index)
                            ;
                        return Token::comment(CommentType::Line);
                    }
                    case '*': {
                        return block_comment();
                    }
                    default:
                        break;
                    }
                }
                if (isdigit(cur)) {
                    return scan_number();
                }
                if (strchr(quote_chars, cur)) {
                    ++m_index;
                    while (m_index < m_buffer.length() && m_buffer[m_index] != cur) {
                        m_index += (m_buffer[m_index] == '\\') ? 2 : 1;
                    }
                    ++m_index;
                    return { Token::string(static_cast<QuoteType>(cur), m_index < m_buffer.length()) };
                }
                switch (cur) {
                case '\n':
                    ++m_index;
                    return Token::end_of_line();
                case '\t':
                    ++m_index;
                    return Token::tab();
                case ' ':
                    while (m_index < m_buffer.length() && m_buffer[m_index] == ' ') {
                        ++m_index;
                    }
                    return Token::whitespace();
                default:
                    break;
                }
                if (isalpha(cur) || cur == '_') {
                    for (; isalnum(m_buffer[m_index]) || m_buffer[m_index] == '_'; ++m_index) {
                        m_scanned += m_buffer[m_index];
                    }
                    if (auto kw = matcher.match(m_scanned); kw) {
                        return Token::keyword(*kw);
                    }
                    return Token::identifier();
                }

                if (auto kw = matcher.match(m_buffer, m_index); kw) {
                    m_index += std::get<size_t>(*kw);
                    return Token::keyword(std::get<Keyword>(*kw));
                }
                ++m_index;
                return Token::symbol(static_cast<int>(cur));
            };
            auto token = match_token();
            m_location.length = m_index - m_location.index;
            token.location = m_location;
            m_location.index = m_index;
            if (m_current->kind == TokenKind::EndOfLine) {
                ++m_location.line;
                m_location.column = 0;
            } else {
                m_location.column += m_location.length;
            }
            m_location.length = 0;
            return matcher.post_match(m_buffer, match_token());
        }

        void lex()
        {
            if (!m_current) {
                return;
            }
            m_current.reset();
        }

        Token block_comment()
        {
            for (; m_index < m_buffer.length() && m_buffer[m_index] != '\n' && (m_buffer[m_index - 1] != '*' || m_buffer[m_index] != '/'); ++m_index)
                ;
            if (m_index >= m_buffer.length()) {
                return Token::comment(CommentType::Block, false);
            }
            if (m_buffer[m_index] == '\n') {
                m_in_comment = true;
                return Token::comment(CommentType::Block, false);
            }
            m_in_comment = false;
            ++m_index;
            return Token::comment(CommentType::Block, true);
        }

        Token scan_number()
        {
            auto type = NumberType::Integer;
            auto cur = m_buffer[m_index];
            int  ix = m_index;
            int (*predicate)(int) = isdigit;
            if (m_index < m_buffer.length() - 1 && cur == '0') {
                if (m_buffer[m_index + 1] == 'x' || m_buffer[m_index + 1] == 'X') {
                    if (m_index == m_buffer.length() - 2 || !isxdigit(m_buffer[m_index + 2])) {
                        m_index += 1;
                        return Token::number(NumberType::Integer);
                    }
                    type = NumberType::HexNumber;
                    predicate = isxdigit;
                    ix = m_index + 2;
                } else if (m_buffer[m_index + 1] == 'b' || m_buffer[m_index + 1] == 'B') {
                    if (m_index == m_buffer.length() - 2 || !isbdigit(m_buffer[m_index + 2])) {
                        m_index += 1;
                        return Token::number(NumberType::Integer);
                    }
                    type = NumberType::BinaryNumber;
                    predicate = isbdigit;
                    ix = m_index + 2;
                }
            }
            while (ix < m_buffer.length()) {
                Char const ch = m_buffer[ix];
                if (!predicate(ch) && ((ch != '.') || (type == NumberType::Decimal))) {
                    // FIXME lex '1..10' as '1', '..', '10'. It will now lex as '1.', '.', '10'
                    break;
                }
                if (ch == '.') {
                    if (type != NumberType::Integer) {
                        break;
                    }
                    type = NumberType::Decimal;
                }
                ++ix;
            }
            m_index = ix;
            return Token::number(type);
        }

        Char operator*() const
        {
            return m_buffer[m_index];
        }

        Char operator[](size_t ix) const
        {
            return m_buffer[ix];
        }

        [[nodiscard]] std::basic_string_view<Char> substr(size_t pos, size_t len = std::basic_string_view<Char>::npos) const
        {
            return m_buffer.substr(pos, len);
        }

        [[nodiscard]] size_t length() const
        {
            return m_buffer.length();
        }

        [[nodiscard]] bool exhausted() const
        {
            return m_index >= length();
        }

        Source &operator++()
        {
            ++m_index;
            return *this;
        }

        Source &operator++(int)
        {
            ++m_index;
            return *this;
        }

    private:
        Buffer               m_buffer;
        size_t               m_index { 0 };
        Lexer               *m_lexer;
        TokenLocation        m_location {};
        std::optional<Token> m_current {};
        bool                 m_in_comment { false };
        std::string          m_scanned;
        Matcher              matcher {};
    };

    std::deque<Token>   pushed_back;
    std::vector<Source> m_sources {};
};

}
