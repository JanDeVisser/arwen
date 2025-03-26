/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <memory>
#include <string_view>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Result.h>
#include <Util/Token.h>
#include <Util/Utf8.h>
#include <App/Operator.h>
#include <App/SyntaxNode.h>

namespace Arwen {

using namespace Util;

template<typename Buffer>
struct ArwenMatcher : EnumKeywords<Buffer, ArwenKeyword> {
    using Token = GenericToken<ArwenKeyword>;
    using Keyword = typename Token::Keyword;
    using PeekResult = std::variant<Token, Buffer, SkipToken>;

    struct MatchResult {
        PeekResult result;
        size_t     matched;
    };

    enum class State {
        NoState,
        Embed,
        WantPath,
        GotPath,
    };

    State  state { State::NoState };
    std::string path;

    PeekResult post_match(Buffer const &buffer, Token const &token)
    {
        switch (state) {
        case State::NoState:
            if (!token.matches_keyword(ArwenKeyword::Embed)) {
                return token;
            }
            state = State::Embed;
            return SkipToken {};
        case State::Embed:
            if (!token.matches_symbol('(')) {
                state = State::NoState;
                return token; // FIXME trigger error
            }
            state = State::WantPath;
            return SkipToken {};
        case State::WantPath:
            if (!token.matches(TokenKind::QuotedString)) {
                state = State::NoState;
                return token; // FIXME trigger error
            }
            path = MUST_EVAL(to_utf8(buffer.substr(token.location.index, token.location.length)));
            std::cerr << "Embedding '" << path << "' " << token.location.index << ", " << token.location.length << std::endl;
            state = State::GotPath;
            return SkipToken {};
        case State::GotPath:
            if (!token.matches_symbol(')')) {
                state = State::NoState;
                return token; // FIXME trigger error
            }
            state = State::NoState;
            if (auto embed_maybe = read_file_by_name<wchar_t>(path); embed_maybe.has_value()) {
                return embed_maybe.value();
            } else {
                std::cerr << "Could not open embed file '" << path << "': " << embed_maybe.error().to_string() << std::endl;
                assert(false);
            }
        default:
            UNREACHABLE();
        }
    }
};

struct Parser {
    using ArwenLexer = Lexer<std::wstring, ArwenMatcher<std::wstring>>;
    using Token = ArwenLexer::Token;

    static std::vector<OperatorDef> operators;
    std::wstring_view               text;
    ArwenLexer                      lexer {};
    std::shared_ptr<Module>         module;
    Label                           pending_label {};

    Parser() = default;

    pSyntaxNode                parse_module(std::string_view name, std::wstring text);
    Token                      parse_statements(SyntaxNodes &statements);
    pSyntaxNode                parse_statement();
    std::wstring_view          text_of(Token const &token);
    pSyntaxNode                parse_top_expression();
    pSyntaxNode                parse_primary();
    pSyntaxNode                parse_expr(pSyntaxNode lhs, Precedence min_prec);
    std::optional<OperatorDef> check_binop(Precedence min_prec);
    std::optional<OperatorDef> check_prefix_op();
    pSyntaxNode                parse_break_continue();
    pSyntaxNode                parse_if();
    pSyntaxNode                parse_loop();
    pSyntaxNode                parse_while();
};

}
