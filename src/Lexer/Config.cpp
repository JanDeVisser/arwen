/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Lexer/Lexer.h>

namespace Arwen {

void Config::Comment::configure(std::string_view const &key, std::optional<std::string_view> const &value)
{
    if (iequals(key, "marker")) {
        if (!value) {
            fatal("Malformed lexer configuration string: 'marker' without value");
        }
        auto v = trim(*value);
        if (auto space = v.find(' '); space != std::string_view::npos) {
            this->block_marker.push_back(BlockMarker {
                .start = v.substr(0, space),
                .end = trim(v.substr(space + 1)),
            });
        } else {
            this->eol_marker.emplace_back(v);
        }
    }
}

std::optional<Token> Config::Comment::scan(Lexer &lexer) const
{
    if (lexer.location.pos == 0 && this->hashpling && lexer.source.starts_with("#!")) {
        uint64_t len = lexer.source.length();
        if (auto p = lexer.source.find('\n'); p != std::string_view::npos) {
            len = p;
        }
        Token ret = lexer.buildToken(len, TokenKind { KindTag::Comment, "#!" });
        lexer.location.line = 1;
        lexer.location.col = 0;
        return ret;
    }
    for (auto &marker : this->eol_marker) {
        if (lexer.source.starts_with(marker)) {
            uint64_t len = lexer.source.length();
            if (auto p = lexer.source.find('\n'); p != std::string_view::npos) {
                len = p;
            }
            Token ret = lexer.buildToken(len, TokenKind { KindTag::Comment, lexer.source.substr(0, len) });
            lexer.location.line += 1;
            lexer.location.col = 0;
            return ret;
        }
    }
    for (auto &marker : this->block_marker) {
        if (lexer.source.starts_with(marker.start)) {
            uint64_t len = lexer.source.length();
            if (auto p = lexer.source.find(marker.end); p != std::string_view::npos) {
                len = p;
            }
            Token    ret = lexer.buildToken(len, TokenKind { KindTag::Comment, lexer.source.substr(0, len) });
            uint64_t newlines = 0;
            uint64_t last = len;
            for (auto ix = 0; ix < len; ++ix) {
                if (lexer.source[ix] == '\n') {
                    ++newlines;
                    last = ix;
                }
            }
            if (newlines > 0) {
                lexer.location.line += newlines;
                if (newlines == 0) {
                    lexer.location.col += len;
                } else {
                    lexer.location.col = len - last;
                }
            }
            return ret;
        }
    }
    return {};
}

[[nodiscard]] bool Config::Keywords::has(std::string_view const &kw) const
{
    return this->keywords.contains(std::string { kw });
}

void Config::Keywords::add(std::string_view const &kw)
{
    this->keywords.emplace(kw);
}

[[nodiscard]] Config::Keywords::MatchResult Config::Keywords::match(std::string_view const &text) const
{
    uint32_t prefix_matches = 0;
    uint32_t lost_matches = 0;
    bool     matched = false;
    for (auto kw : this->keywords) {
        if (text == kw) {
            matched = true;
        } else if (text.length() < kw.length()) {
            if (kw.substr(0, text.length()) == text) {
                prefix_matches += 1;
            }
        } else {
            if (text.substr(0, kw.length()) == kw) {
                lost_matches += 1;
            }
        }
    }
    if (!matched && prefix_matches == 0 && lost_matches == 0) {
        return NoMatch;
    }
    if (!matched && prefix_matches > 0) {
        return Prefix;
    }
    if (matched) {
        if (prefix_matches == 0) {
            return ExactMatch;
        }
        return PrefixAndExact;
    }
    assert(!matched && prefix_matches == 0 && lost_matches != 0);
    return MatchLost;
}

void Config::Keywords::configure(std::string_view const &key, std::optional<std::string_view> const &value)
{
    if (iequals(key, "kw") && value) {
        if (!value) {
            fatal("Malformed lexer configuration string: 'kw' without value");
        }
        this->keywords.emplace(trim(*value));
    }
}

std::optional<Token> Config::Number::scan(Lexer &lexer) const
{
    NumberType t = NumberType::Int;
    uint64_t   p = 0;
    if (!lexer.source.empty() && (lexer.source[0] == '+' || lexer.source[0] == '-')) {
        if (!this->signed_numbers) {
            return {};
        }
        p = 1;
    }
    bool digit_found = false;
    if (p < lexer.source.length() && lexer.source[p] == '0') {
        p += 1;
        if (p < lexer.source.length() && (lexer.source[p] == 'x' || lexer.source[p] == 'X')) {
            if (!this->hex) {
                return lexer.buildToken(p, TokenKind { KindTag::Number, NumberType::Int });
            }
            p += 1;
            t = NumberType::Hex;
        } else if (p < lexer.source.length() && (lexer.source[p] == 'b' || lexer.source[p] == 'B')) {
            if (!this->binary) {
                return lexer.buildToken(p, TokenKind { KindTag::Number, NumberType::Int });
            }
            p += 1;
            t = NumberType::Binary;
        } else {
            digit_found = true;
        }
    }
    bool done = false;
    while (p < lexer.source.length() && !done) {
        auto ch = lexer.source[p];
        switch (t) {
        case NumberType::Binary:
            if (ch != '0' && ch != '1') {
                done = true;
            }
            break;
        case NumberType::Hex:
            if (!isxdigit(ch)) {
                done = true;
            }
            break;
        case NumberType::Int:
            if (!isdigit(ch)) {
                done = true;
            }
            break;
        default:
            UNREACHABLE();
        }
        if (!done) {
            digit_found = true;
            p += 1;
        }
    }
    if (this->decimal && p < lexer.source.length() && t == NumberType::Int && lexer.source[p] == '.') {
        p += 1;
        t = NumberType::Float;
        while (p < lexer.source.length() && isdigit(lexer.source[p])) {
            digit_found = true;
            p += 1;
        }
    }
    if (digit_found) {
        return lexer.buildToken(p, TokenKind { KindTag::Number, t });
    }
    return {};
}

void Config::Number::configure(std::string_view const &key, std::optional<std::string_view> const &value)
{
    if (iequals(key, "signed") || !iequals(key, "signed_numbers")) {
        signed_numbers = true;
    }
    if (iequals(key, "decimal") || !iequals(key, "float")) {
        decimal = true;
    }
    if (iequals(key, "binary") || !iequals(key, "base2")) {
        binary = true;
    }
    if (iequals(key, "hex") || !iequals(key, "base16")) {
        hex = true;
    }
}

void Config::QString::configure(std::string_view const &key, std::optional<std::string_view> const &value)
{
    if (iequals(key, "quotes")) {
        if (!value) {
            fatal("Malformed lexer configuration string: 'quotes' without value");
        }
        quotes = *value;
    }
}

void Config::Whitespace::configure(std::string_view key, std::optional<std::string_view> value)
{
    if (iequals(key, "ignore_ws")) {
        this->ignore_ws = true;
    }
    if (iequals(key, "ignore_nl")) {
        this->ignore_nl = true;
    }
    if (iequals(key, "ignoreall")) {
        this->ignore_nl = true;
        this->ignore_ws = true;
    }
}

void Config::configure(std::string_view const &scanner, std::optional<std::string_view> scanner_config)
{
    auto s_opt = decode<Scanner>(scanner);
    if (!s_opt) {
        if (!scanner_config) {
            fatal("Malformed lexer configuration string: Unknown scanner '{}'", scanner);
        }
    }

    auto s = *s_opt;
    switch (s) {
#undef S
#define S(S)               \
    case Scanner::S:       \
        this->S.on = true; \
        break;
        Scanners(S)
#undef S
    }
    if (!scanner_config || scanner_config->empty()) {
        return;
    }
    auto c = *scanner_config;
    while (!c.empty()) {
        auto eq_ix = c.length();
        if (auto ix = c.find_first_of("=;"); ix != std::string_view::npos) {
            eq_ix = ix;
        }
        auto was_eq = eq_ix < c.length() && c[eq_ix] == '=';
        auto key = trim(c.substr(0, eq_ix));
        c = c.substr((eq_ix < c.length()) ? eq_ix + 1 : c.length());
        std::optional<std::string_view> value = {};
        if (was_eq) {
            auto semi_ix = c.length();
            if (auto ix = c.find(';'); ix != std::string_view::npos) {
                semi_ix = ix;
            }
            auto trimmed = trim(c.substr(0, semi_ix - 1));
            c = c.substr((semi_ix < c.length()) ? semi_ix + 1 : c.length());
            value = trimmed;
        }

        switch (s) {
#undef S
#define S(S)                           \
    case Scanner::S:                   \
        this->S.configure(key, value); \
        break;
            Scanners(S)
#undef S
        }
    }
}

}
