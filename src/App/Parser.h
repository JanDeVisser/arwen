/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <format>
#include <memory>
#include <string>
#include <string_view>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Result.h>
#include <Util/Utf8.h>

namespace Arwen {

using namespace Util;

struct ArwenError {
    TokenLocation location;
    std::wstring  message;
};

struct Parser {
    using ArwenLexerTypes = LexerTypes<std::wstring_view, wchar_t, ArwenKeyword>;
    using ArwenLexer = Lexer<ArwenLexerTypes, ArwenLexerTypes::CScannerPack>;
    using Token = ArwenLexer::Token;
    using LexerError = ArwenLexer::LexerError;
    using LexerResult = ArwenLexer::LexerResult;

    enum class ParseLevel {
        Module,
        Function,
        Block,
    };

    static std::vector<OperatorDef> operators;
    std::wstring_view               text;
    ArwenLexer                      lexer {};
    std::shared_ptr<Module>         module;
    ParseLevel                      level { ParseLevel::Module };
    std::optional<Token>            pending_id {};
    std::vector<ArwenError>         errors;

    Parser() = default;

    pSyntaxNode                parse_file(std::wstring const &text);
    pSyntaxNode                parse_module(std::string_view name, std::wstring const &text);
    Token                      parse_statements(SyntaxNodes &statements);
    pSyntaxNode                parse_statement();
    pSyntaxNode                parse_module_level_statement();
    std::wstring_view          text_of(Token const &token) const;
    std::wstring_view          text_of(LexerErrorMessage const &error) const;
    std::wstring_view          text_of(LexerError const &error) const;
    std::wstring_view          text_of(LexerResult const &res) const;
    std::wstring_view          text_of(TokenLocation const &location) const;
    pSyntaxNode                parse_top_expression();
    pSyntaxNode                parse_primary();
    pSyntaxNode                parse_expr(pSyntaxNode lhs, Precedence min_prec);
    std::optional<OperatorDef> check_binop(Precedence min_prec);
    std::optional<OperatorDef> check_prefix_op();
    pSyntaxNode                parse_break_continue();
    pSyntaxNode                parse_defer();
    pSyntaxNode                parse_embed();
    pSyntaxNode                parse_func();
    pSyntaxNode                parse_if();
    pSyntaxNode                parse_include();
    pSyntaxNode                parse_loop();
    pSyntaxNode                parse_return();
    pSyntaxNode                parse_var_decl();
    pSyntaxNode                parse_while();
    pSyntaxNode                parse_yield();

    void append(LexerErrorMessage const &lexer_error);
    void append(LexerErrorMessage const &lexer_error, char const *message);
    void append(LexerErrorMessage const &lexer_error, wchar_t const *message);
    void append(Token const &token, char const *message);
    void append(Token const &token, wchar_t const *message);
    void append(TokenLocation location, std::wstring message);
    void append(TokenLocation location, std::string const &message);
    void append(TokenLocation location, wchar_t const *message);
    void append(TokenLocation location, char const *message);

    template<typename... Args>
    void append(Token const &token, std::format_string<Args...> const message, Args &&...args)
    {
        append(token.location, std::vformat(message.get(), std::make_format_args(args...)));
    }

    template<typename... Args>
    void append(Token const &token, std::wformat_string<Args...> const message, Args &&...args)
    {
        append(token.location, std::vformat(message.get(), std::make_wformat_args(args...)));
    }

    template<typename... Args>
    void append(TokenLocation location, std::format_string<Args...> const message, Args &&...args)
    {
        append(std::move(location), std::vformat(message.get(), std::make_format_args(args...)));
    }

    template<typename... Args>
    void append(TokenLocation location, std::wformat_string<Args...> const message, Args &&...args)
    {
        append(std::move(location), std::vformat(message.get(), std::make_wformat_args(args...)));
    }

    template<typename... Args>
    void append(LexerErrorMessage const &lexer_error, std::format_string<Args...> const message, Args &&...args)
    {
        append(lexer_error.location, std::vformat(message.get(), std::make_format_args(args...)));
    }

    template<typename... Args>
    void append(LexerErrorMessage const &lexer_error, std::wformat_string<Args...> const message, Args &&...args)
    {
        append(lexer_error.location, std::vformat(message.get(), std::make_wformat_args(args...)));
    }
};

}
