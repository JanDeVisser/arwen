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

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Result.h>
#include <Util/TokenLocation.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

using namespace Util;

struct Parser {

    struct ArwenInsertBlock {
        constexpr static wchar_t const *begin = L"@insert";
        constexpr static wchar_t const *end = L"@end";
    };

    using ArwenLexerTypes = LexerTypes<std::wstring_view, wchar_t, ArwenKeyword>;
    using ArwenLexer = Lexer<ArwenLexerTypes, ArwenLexerTypes::ScannerPack<ArwenLexerTypes::CScannerPack, ArwenLexerTypes::RawScanner<ArwenInsertBlock>>>;
    using Token = ArwenLexer::Token;
    using LexerError = ArwenLexer::LexerError;
    using LexerResult = ArwenLexer::LexerResult;
    using OperatorSymbol = std::variant<wchar_t, ArwenKeyword>;

    struct OperatorDef {
        Operator       op;
        OperatorSymbol sym;
        Precedence     precedence;
        Position       position { Position::Infix };
        Associativity  associativity { Associativity::Left };
    };

    enum class ParseLevel {
        Module,
        Function,
        Block,
    };

    static std::vector<OperatorDef> operators;
    std::wstring_view               text;
    ArwenLexer                      lexer {};
    ParseLevel                      level { ParseLevel::Module };
    std::vector<ArwenError>         errors;
    std::vector<pSyntaxNode>        unbound_nodes;
    pNamespace                      root;
    std::vector<pNamespace>         namespaces;
    std::shared_ptr<Program>        program;
    int                             pass { 0 };
    int                             unbound { 0 };

    Parser();

    pSyntaxNode                parse_file(std::wstring const &text, pNamespace ns);
    pModule                    parse_module(std::string_view name, std::wstring text);
    pSyntaxNode                parse_script(std::wstring text);
    Token                      parse_statements(SyntaxNodes &statements);
    pSyntaxNode                parse_statement();
    pSyntaxNode                parse_module_level_statement();
    std::wstring_view          text_at(size_t start, std::optional<size_t> end) const;
    std::wstring_view          text_of(Token const &token) const;
    std::wstring_view          text_of(LexerErrorMessage const &error) const;
    std::wstring_view          text_of(LexerError const &error) const;
    std::wstring_view          text_of(LexerResult const &res) const;
    std::wstring_view          text_of(TokenLocation const &location) const;
    pSyntaxNode                parse_primary();
    pSyntaxNode                parse_expression(Precedence min_prec = 0);
    bool                       check_op();
    std::optional<OperatorDef> check_binop();
    std::optional<OperatorDef> check_prefix_op();
    std::optional<OperatorDef> check_postfix_op();
    pTypeSpecification         parse_type();
    pSyntaxNode                parse_break_continue();
    pSyntaxNode                parse_defer();
    pSyntaxNode                parse_embed();
    pSyntaxNode                parse_enum();
    pSyntaxNode                parse_for();
    pSyntaxNode                parse_func();
    pSyntaxNode                parse_if();
    pSyntaxNode                parse_import();
    pSyntaxNode                parse_include();
    pSyntaxNode                parse_loop();
    pSyntaxNode                parse_public();
    pSyntaxNode                parse_return_error();
    pSyntaxNode                parse_struct();
    pSyntaxNode                parse_var_decl();
    pSyntaxNode                parse_while();
    pSyntaxNode                parse_yield();

    pType                            type_of(std::wstring const &name) const;
    bool                             has_function(std::wstring const &name, pType const &type) const;
    pFunctionDefinition              find_function(std::wstring const &name, pType const &type) const;
    pFunctionDefinition              find_function_by_arg_list(std::wstring const &name, pType const &type) const;
    std::vector<pFunctionDefinition> find_overloads(std::wstring const &name) const;
    void                             register_variable(std::wstring name, pSyntaxNode node);
    void                             register_function(std::wstring name, pFunctionDefinition node);
    void                             unregister_function(std::wstring name, pFunctionDefinition node);
    pType                            find_type(std::wstring const &name) const;
    void                             register_type(std::wstring name, pType type);
    void                             push_namespace(pNamespace const &ns);
    pNamespace const                &push_new_namespace();
    void                             pop_namespace();

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

    pType bind_error(TokenLocation location, std::wstring msg);

    template<typename... Args>
    pType bind_error(TokenLocation location, std::format_string<Args...> const message, Args &&...args)
    {
        return bind_error(std::move(location), as_wstring(std::vformat(message.get(), std::make_format_args(args...))));
    }

    template<typename... Args>
    pType bind_error(TokenLocation location, std::wformat_string<Args...> const message, Args &&...args)
    {
        return bind_error(std::move(location), std::vformat(message.get(), std::make_wformat_args(args...)));
    }
};

}
