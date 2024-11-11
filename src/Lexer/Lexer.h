/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <array>
#include <cctype>
#include <charconv>
#include <cstdint>
#include <functional>
#include <map>

#include <Lib.h>
#include <Result.h>

namespace Arwen {

struct Location {
    std::string_view buffer = {};
    uint64_t         pos = 0;
    uint64_t         line = 0;
    uint64_t         col = 0;

    [[nodiscard]] bool operator==(Location const &other) const
    {
        if (buffer != other.buffer) {
            return false;
        }
        return pos == other.pos;
    }

    [[nodiscard]] bool operator<(Location const &other) const
    {
        if (buffer != other.buffer) {
            return buffer < other.buffer;
        }
        return pos < other.pos;
    }
};

#define KindTags(S)  \
    S(Null, 0)       \
    S(Comment, 1)    \
    S(Eof, 2)        \
    S(Identifier, 3) \
    S(Keyword, 4)    \
    S(Newline, 5)    \
    S(Number, 6)     \
    S(String, 7)     \
    S(Symbol, 8)     \
    S(Whitespace, 9)

enum class KindTag {
#undef S
#define S(Tag, Index) \
    Tag = Index,
    KindTags(S)
#undef S
};

template<>
inline std::optional<KindTag> decode(std::string_view s, ...)
{
#undef S
#define S(Tag, Index)     \
    if (iequals(s, #Tag)) \
        return KindTag::Tag;
    KindTags(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(KindTag const &v)
{
    switch (v) {
#undef S
#define S(Tag, Index)  \
    case KindTag::Tag: \
        return #Tag;
        KindTags(S)
#undef S
    }
}

#define NumberTypes(S) \
    S(Binary)          \
    S(Float)           \
    S(Hex)             \
    S(Int)

enum class NumberType {
#undef S
#define S(Type) \
    Type,
    NumberTypes(S)
#undef S
};

template<>
inline std::optional<NumberType> decode(std::string_view s, ...)
{
#undef S
#define S(Type)            \
    if (iequals(s, #Type)) \
        return NumberType::Type;
    NumberTypes(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(NumberType const &v)
{
    switch (v) {
#undef S
#define S(Type)            \
    case NumberType::Type: \
        return #Type;
        NumberTypes(S)
#undef S
    }
}

struct TokenKind {
    TokenKind()
        : m_tag(KindTag::Null)
    {
    }

    explicit TokenKind(KindTag tag)
        : m_tag(tag)
    {
        if (tag == KindTag::Comment || tag == KindTag::Keyword || tag == KindTag::Number || tag == KindTag::String || tag == KindTag::Symbol) {
            fatal("Token kind tag '{}' needs a payload", tag);
        }
    }

    TokenKind(KindTag tag, std::string_view str)
        : m_tag(tag)
    {
        if (tag != KindTag::Comment && tag != KindTag::Keyword) {
            fatal("Token kind tag '{}' cannot have a string payload", tag);
        }
        payload.sv = str;
    }

    TokenKind(KindTag tag, char ch)
        : m_tag(tag)
    {
        if (tag != KindTag::String && tag != KindTag::Symbol) {
            fatal("Token kind tag '{}' cannot have a char payload", tag);
        }
        payload.symbol = ch;
    }

    explicit TokenKind(NumberType number_type)
        : m_tag(KindTag::Number)
    {
        payload.number_type = number_type;
    }

    [[nodiscard]] KindTag          tag() const;
    [[nodiscard]] char             symbol() const;
    [[nodiscard]] char             quote() const;
    [[nodiscard]] std::string_view keyword() const;
    [[nodiscard]] std::string_view comment_marker() const;
    [[nodiscard]] NumberType       number_type() const;
    [[nodiscard]] bool             operator<(TokenKind const &rhs) const;
    [[nodiscard]] bool             operator==(TokenKind const &rhs) const;

private:
    KindTag m_tag = KindTag::Null;
    union {
        bool             dummy { true };
        std::string_view sv;
        char             symbol;
        NumberType       number_type;
    } payload;
};

struct Token {
    Location         location {};
    std::string_view raw_text {};
    std::string_view text {};
    TokenKind        kind {};

    constexpr Token() = default;
    Token(TokenKind kind, std::string_view text);
    [[nodiscard]] KindTag tag() const;
    [[nodiscard]] bool    is(KindTag k) const;
    [[nodiscard]] bool    operator==(Token const &rhs) const;
    [[nodiscard]] bool    operator<(Token const &rhs) const;

    enum class ConversionError {
        ConversionError,
    };

    template<typename T>
    Result<T, ConversionError> as() const
    {
        UNREACHABLE();
    }

    template<>
    Result<std::string_view, ConversionError> as() const
    {
        return text;
    }

    template<std::integral T>
    Result<T, ConversionError> as() const
    {
        T ret;
        auto [ptr, ec] = std::from_chars(text.data(), text.data() + text.length(), ret);
        if (ec != std::errc()) {
            return ConversionError::ConversionError;
        }
        return ret;
    }

    template<std::floating_point T>
    Result<T, ConversionError> as() const
    {
        char  *end;
        double flt = std::strtod(text.data(), &end);
        if (errno == ERANGE || (flt == 0 && end == text.data())) {
            return ConversionError::ConversionError;
        }
        return static_cast<T>(flt);
    }
};

#define Scanners(S) \
    S(Comment)      \
    S(Identifier)   \
    S(Keywords)     \
    S(Number)       \
    S(QString)      \
    S(Whitespace)

enum class Scanner {
#undef S
#define S(S) S,
    Scanners(S)
#undef S
};

template<>
inline std::optional<Scanner> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return Scanner::S;
    Scanners(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(Scanner const &v)
{
    switch (v) {
#undef S
#define S(S)         \
    case Scanner::S: \
        return #S;
        Scanners(S)
#undef S
    }
}

struct Config {
    struct Comment {
        struct BlockMarker {
            std::string_view start = {};
            std::string_view end = {};
        };

        bool                     on = false;
        bool                     ignore = false;
        bool                     hashpling = true;
        std::vector<BlockMarker> block_marker = {};
        std::vector<std::string> eol_marker = {};

        std::optional<Token> scan(struct Lexer &lexer) const;
        void                 configure(std::string_view const &key, std::optional<std::string_view> const &value);
    };

    struct Identifier {
        bool on = false;

        void configure(std::string_view, std::optional<std::string_view>)
        {
        }
    };

    struct Keywords {
        bool                  on = true;
        std::set<std::string> keywords = {};

        enum MatchResult {
            NoMatch,
            ExactMatch,
            Prefix,
            PrefixAndExact,
            MatchLost,
        };

        [[nodiscard]] bool        has(std::string_view const &kw) const;
        void                      add(std::string_view const &kw);
        [[nodiscard]] MatchResult match(std::string_view const &text) const;
        void                      configure(std::string_view const &key, std::optional<std::string_view> const &value);

        void addAll(auto const &kw_s)
        {
            for (auto kw : kw_s) {
                this->add(kw);
            }
        }
    };

    struct Number {
        bool on = false;
        bool signed_numbers = false;
        bool decimal = false;
        bool binary = false;
        bool hex = false;

        std::optional<Token> scan(Lexer &lexer) const;
        void                 configure(std::string_view const &key, std::optional<std::string_view> const &value);
    };

    struct QString {
        bool        on = false;
        std::string quotes = "\"'`";

        void configure(std::string_view const &key, std::optional<std::string_view> const &value);
    };

    struct Whitespace {
        bool on = false;
        bool ignore_ws = false;
        bool ignore_nl = false;

        void configure(std::string_view key, std::optional<std::string_view> value);
    };

    void configure(std::string_view const &scanner, std::optional<std::string_view> scanner_config);

    Comment    Comment = {};
    Identifier Identifier = {};
    Keywords   Keywords = {};
    Number     Number = {};
    QString    QString = {};
    Whitespace Whitespace = {};
};

class Lexer {
public:
    enum Error {
        ExpectedIdentifier,
    };

    Config            config = {};
    std::set<KindTag> ignored = {};

    std::string_view         source = {};
    Location                 location = {};
    std::optional<Token>     current = {};
    bool                     exhausted = false;
    std::vector<std::string> unescaped_strings = {};

    Lexer() = default;
    Lexer(Config &config, std::string_view source, std::string_view buffer = "");
    Token                       buildToken(uint64_t len, TokenKind kind);
    void                        advance();
    std::optional<Token>        next();
    std::optional<Token>        peek_next();
    std::optional<Token>        peek();
    bool                        accept_keyword(std::string_view keyword);
    Result<Token, Lexer::Error> expect_identifier();
    std::optional<Token>        accept_identifier();
    Arwen::Error<Lexer::Error>  expect_symbol(char symbol);
    bool                        accept_symbol(char symbol);

private:
    std::string_view unescape(std::string_view s);
};

template<>
inline std::optional<Token> decode(std::string_view s, ...)
{
    va_list args;
    va_start(args, s);
    Lexer const *lexer = va_arg(args, Lexer const *);
    auto         v = s;
    KindTag      k = KindTag::Identifier;
    if (auto colon = s.find(':'); colon != std::string_view::npos) {
        v = s.substr(colon);
        if (auto k_opt = decode<KindTag>(s.substr(0, colon)); k_opt) {
            k = *k_opt;
        }
    }
    switch (k) {
    case KindTag::Null:
    case KindTag::Eof:
    case KindTag::Identifier:
    case KindTag::Newline:
    case KindTag::Whitespace:
        return Token { TokenKind { k }, v };
    case KindTag::Comment: {
        std::string_view marker = "//";
        if (auto semicolon = v.find(';'); semicolon != std::string_view::npos) {
            marker = v.substr(0, semicolon);
            v = s.substr(semicolon);
        }
        return Token { TokenKind { k, marker }, v };
    }
    case KindTag::Keyword: {
        if (lexer->config.Keywords.has(v)) {
            return Token { TokenKind { k, v }, v };
        }
        return Token { TokenKind { k }, v };
    }
    case KindTag::Number: {
        NumberType number_type = NumberType::Int;
        if (auto semicolon = v.find(';'); semicolon != std::string_view::npos) {
            if (auto number_type_opt = decode<NumberType>(v.substr(0, semicolon)); number_type_opt) {
                number_type = *number_type_opt;
            }
            v = s.substr(semicolon);
        }
        return Token { TokenKind { number_type }, v };
    }
    case KindTag::String:
        return Token { TokenKind { k, !v.empty() ? v[0] : '"' }, v };
    case KindTag::Symbol:
        return Token { TokenKind { k, v[0] }, v };
    }
}

}

template<>
struct std::formatter<Arwen::Location, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Location location, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << location.buffer << ':' << location.line + 1 << ':' << location.col + 1 << ':';
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::KindTag, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::KindTag k, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(k);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::NumberType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::NumberType number_type, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(number_type);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::TokenKind, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::TokenKind k, FmtContext &ctx) const
    {
        std::ostringstream out;
        auto               kind_tag = k.tag();
        switch (kind_tag) {
        case Arwen::KindTag::Keyword:
            out << '"' << k.keyword() << '"';
            break;
        case Arwen::KindTag::String:
            out << "#QString(" << k.quote() << ")";
            break;
        case Arwen::KindTag::Symbol:
            out << '\'' << k.symbol() << '\'';
            break;
        case Arwen::KindTag::Number:
            out << '#' << Arwen::to_string(k.number_type());
            break;
        default:
            out << '#' << Arwen::to_string(kind_tag);
            break;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Token, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Token token, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("{} [{}]", token.text, token.kind);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
