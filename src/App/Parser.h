/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <memory>
#include <string_view>
#include <vector>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Token.h>

#include <App/SyntaxNode.h>

namespace Arwen {

using namespace Util;

struct Parser {
    using ArwenLexer = Lexer<std::wstring_view, EnumKeywords<std::wstring_view, NoKeywordCategory, ArwenKeyword>>;
    using Token = ArwenLexer::Token;

    static std::vector<OperatorDef> operators;
    std::wstring_view               text;
    ArwenLexer                      lexer {};
    std::shared_ptr<Module>         module;
    Label                           pending_label {};

    Parser() = default;

    pSyntaxNode                parse_module(std::string_view name, std::wstring_view text);
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
