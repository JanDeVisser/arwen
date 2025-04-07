/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Result.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <string>
#include <sys/socket.h>

namespace Arwen {

using namespace Util;

std::vector<OperatorDef> Parser::operators {
    { Operator::Add, '+', 11 },
    { Operator::Assign, '=', 1, Position::Infix, Associativity::Right },
    { Operator::AssignAnd, ArwenKeyword::AssignAnd, 1, Position::Infix, Associativity::Right },
    { Operator::AssignDecrement, ArwenKeyword::AssignDecrement, 1, Position::Infix, Associativity::Right },
    { Operator::AssignDivide, ArwenKeyword::AssignDivide, 1, Position::Infix, Associativity::Right },
    { Operator::AssignIncrement, ArwenKeyword::AssignIncrement, 1, Position::Infix, Associativity::Right },
    { Operator::AssignModulo, ArwenKeyword::AssignModulo, 1, Position::Infix, Associativity::Right },
    { Operator::AssignMultiply, ArwenKeyword::AssignMultiply, 1, Position::Infix, Associativity::Right },
    { Operator::AssignOr, ArwenKeyword::AssignOr, 1, Position::Infix, Associativity::Right },
    { Operator::AssignShiftLeft, ArwenKeyword::AssignShiftLeft, 1, Position::Infix, Associativity::Right },
    { Operator::AssignShiftRight, ArwenKeyword::AssignShiftRight, 1, Position::Infix, Associativity::Right },
    { Operator::AssignXor, ArwenKeyword::AssignXor, 1, Position::Infix, Associativity::Right },
    { Operator::Call, '(', 15 },
    { Operator::Divide, '/', 12 },
    { Operator::Equals, ArwenKeyword::Equals, 8 },
    { Operator::Greater, '>', 8 },
    { Operator::GreaterEqual, ArwenKeyword::GreaterEqual, 8 },
    { Operator::Idempotent, '+', 14, Position::Prefix, Associativity::Right },
    { Operator::Invert, '!', 14, Position::Prefix, Associativity::Right },
    { Operator::Less, '<', 8 },
    { Operator::LessEqual, ArwenKeyword::LessEqual, 8 },
    { Operator::Modulo, '%', 12 },
    { Operator::Multiply, '*', 12 },
    { Operator::Negate, '-', 14, Position::Prefix, Associativity::Right },
    { Operator::NotEqual, ArwenKeyword::NotEqual, 8 },
    { Operator::Range, ArwenKeyword::Range, 2 },
    { Operator::Sequence, ',', 0 },
    { Operator::ShiftLeft, ArwenKeyword::ShiftLeft, 10 },
    { Operator::ShiftRight, ArwenKeyword::ShiftRight, 10 },
    { Operator::Subtract, '-', 11 },
};

pSyntaxNode Parser::parse_file(std::wstring const &text)
{
    this->text = text;
    lexer.push_source(text);

    SyntaxNodes statements;
    if (auto t = parse_statements(statements); !t.matches(TokenKind::EndOfFile)) {
        append(t, "Expected end of file");
        return nullptr;
    }
    switch (statements.size()) {
    case 0:
        return nullptr;
    case 1:
        return statements[0];
    default:
        return make_node<Block>(statements[0]->location + statements.back()->location, statements);
    }
}

pSyntaxNode Parser::parse_module(std::string_view name, std::wstring const &text)
{
    this->text = text;
    lexer.push_source(text);

    SyntaxNodes statements;
    if (auto t = parse_statements(statements); !t.matches(TokenKind::EndOfFile)) {
        append(t, "Expected end of file");
        return nullptr;
    }
    if (!statements.empty()) {
        return make_node<Module>(statements[0]->location + statements.back()->location, name, text, statements);
    }
    return nullptr;
}

Parser::Token Parser::parse_statements(SyntaxNodes &statements)
{
    while (true) {
        auto const t = lexer.peek();
        if (t.matches(TokenKind::EndOfFile) || t.matches_symbol('}')) {
            lexer.lex();
            return t;
        }
        if (auto stmt = (level == ParseLevel::Module) ? parse_module_level_statement() : parse_statement(); stmt != nullptr) {
            statements.push_back(stmt);
        }
    }
}

pSyntaxNode Parser::parse_module_level_statement()
{
    auto const t = lexer.peek();
    // std::cerr << TokenKind_name(t.kind) << " " << std::endl;
    switch (t.kind) {
    case TokenKind::EndOfFile:
        append(t, "Unexpected end of file");
        return nullptr;
    case TokenKind::Identifier:
        lexer.lex();
        if (auto err = lexer.expect_symbol(':'); err.is_error()) {
            append(err.error(), "Expected variable declaration");
            break;
        }
        return parse_statement();
    case TokenKind::Keyword: {
        switch (t.keyword()) {
        case ArwenKeyword::Const:
            lexer.lex();
            return parse_module_level_statement();
        case ArwenKeyword::Func:
            return parse_func();
        case ArwenKeyword::Include:
            return parse_include();
        default:
            break;
        }
    } break;
    default:
        break;
    }
    lexer.lex();
    append(t, L"Unexpected token '{}'", text_of(t));
    return nullptr;
}

pSyntaxNode Parser::parse_statement()
{
    auto t = lexer.peek();
    switch (t.kind) {
    case TokenKind::EndOfFile:
        append(t, "Unexpected end of file");
        return nullptr;
    case TokenKind::Identifier:
        if (lexer.has_lookback(1) && lexer.lookback(0).matches_symbol(':') && lexer.lookback(1).matches(TokenKind::Identifier)) {
            // This is the type of a variable decl:
            return parse_var_decl();
        }
        lexer.lex();
        if (lexer.peek().matches_symbol(':')) {
            lexer.lex();
            return parse_statement();
        }
        lexer.push_back(t);
        // Fall through
    case TokenKind::Number:
    case TokenKind::QuotedString:
        return parse_top_expression();
    case TokenKind::Keyword: {
        switch (t.keyword()) {
        case ArwenKeyword::Break:
        case ArwenKeyword::Continue:
            return parse_break_continue();
        case ArwenKeyword::Const:
            lexer.lex();
            return parse_statement();
        case ArwenKeyword::Defer:
            return parse_defer();
        case ArwenKeyword::Embed:
            return parse_embed();
        case ArwenKeyword::Error:
            return parse_return_error();
        case ArwenKeyword::For:
            return parse_for();
        case ArwenKeyword::Func:
            return parse_func();
        case ArwenKeyword::If:
            return parse_if();
        case ArwenKeyword::Include:
            return parse_include();
        case ArwenKeyword::Return:
            return parse_return_error();
        case ArwenKeyword::Loop:
            return parse_loop();
        case ArwenKeyword::While:
            return parse_while();
        case ArwenKeyword::Yield:
            return parse_yield();
        default:
            append(t, "Unexpected keyword '{}'", as_utf8(text_of(t)));
            lexer.lex();
            return nullptr;
        }
    } break;
    case TokenKind::Symbol:
        switch (t.symbol_code()) {
        case ';':
            return make_node<Dummy>(lexer.lex().location);
        case '{': {
            lexer.lex();
            SyntaxNodes block;
            auto        old_level = level;
            level = ParseLevel::Block;
            Defer defer { [this, old_level]() { level = old_level; } };
            if (auto const end_token = parse_statements(block); !end_token.matches_symbol('}')) {
                append(t, "Unexpected end of block");
                return nullptr;
            } else {
                if (block.empty()) {
                    return make_node<Void>(t.location + end_token.location);
                }
                return make_node<Block>(t.location + end_token.location, block);
            }
        }
        case '=':
            if (lexer.has_lookback(1)
                && lexer.lookback(0).matches_symbol(':')
                && lexer.lookback(1).matches(TokenKind::Identifier)) {
                // This is the '=' of a variable decl with implied type:
                return parse_var_decl();
            }
            // Fall through
        default:
            if (auto expr = parse_top_expression(); expr) {
                return expr;
            }
            append(t, "Unexpected symbol '{:c}'", static_cast<char>(t.symbol_code()));
            lexer.lex();
            return nullptr;
        }
    default:
        lexer.lex();
        append(t, L"Unexpected token '{}'", text_of(t));
        return nullptr;
    }
}

std::wstring_view Parser::text_of(Token const &token) const
{
    return text_of(token.location);
}

std::wstring_view Parser::text_of(LexerErrorMessage const &error) const
{
    return text_of(error.location);
}

std::wstring_view Parser::text_of(LexerError const &error) const
{
    return text_of(error.error().location);
}

std::wstring_view Parser::text_of(LexerResult const &res) const
{
    if (res.is_error()) {
        return text_of(res.error().location);
    }
    return text_of(res.value().location);
}

std::wstring_view Parser::text_of(TokenLocation const &location) const
{
    if (location.index < text.length()) {
        return text.substr(location.index, location.length);
    }
    return L"";
}

pSyntaxNode Parser::parse_top_expression()
{
    auto lhs = parse_primary();
    return (lhs) ? parse_expr(lhs, 0) : nullptr;
}

pSyntaxNode Parser::parse_primary()
{
    auto       &token = lexer.peek();
    pSyntaxNode ret { nullptr };
    switch (token.kind) {
    case TokenKind::Number: {
        ret = make_node<Number>(token.location, text_of(token), token.number_type());
        lexer.lex();
        break;
    }
    case TokenKind::QuotedString: {
        ret = make_node<QuotedString>(token.location, text_of(token), token.quoted_string().quote_type);
        lexer.lex();
        break;
    }
    case TokenKind::Identifier: {
        ret = make_node<Identifier>(token.location, text_of(token));
        lexer.lex();
        break;
    }
    case TokenKind::Keyword:
        if (token.matches_keyword(ArwenKeyword::Embed)) {
            return parse_embed();
        }
        if (token.matches_keyword(ArwenKeyword::Include)) {
            return parse_include();
        }
        append(token, "Unexpected keyword '{}'", ArwenKeyword_name(token.keyword()));
        return nullptr;
    case TokenKind::Symbol: {
        if (token.symbol_code() == L'(') {
            lexer.lex();
            ret = parse_top_expression();
            if (auto t = lexer.peek(); !t.matches_symbol(')')) {
                append(t, "Expected ')'");
                return nullptr;
            }
            lexer.lex();
            break;
        }
        if (auto const op_maybe = check_prefix_op(); op_maybe) {
            auto &op = *op_maybe;
            auto  op_token = lexer.lex();
            auto  operand = parse_primary();
            if (!operand) {
                append(token, "Expected operand following prefix operator '{}'", Operator_name(op.op));
                return nullptr;
            }
            ret = make_node<UnaryExpression>(op_token.location, op.op, operand);
        }
    } // Fall through
    default:
        append(token, L"Unexpected token `{}`", text_of(token));
        ret = nullptr;
    }
    if (ret == nullptr) {
        append(token, "Expected primary expression");
    }
    return ret;
}

pSyntaxNode Parser::parse_expr(pSyntaxNode lhs, Precedence min_prec)
{
    for (auto op_maybe = check_binop(min_prec); op_maybe; op_maybe = check_binop(min_prec)) {
        auto const &curr_op = *op_maybe;
        if (curr_op.op == Operator::Call) {
            // Don't lex the '(' so parse_primary will return a
            // single expression, probably a binop with op = ','.
            auto param_list = parse_primary();
            return make_node<BinaryExpression>(lhs->location + param_list->location, lhs, curr_op.op, param_list);
        }
        auto token = lexer.lex();
        auto rhs = parse_primary();
        if (!rhs) {
            append(token, "Expected right hand side operand following infix operator `{}`", Operator_name(curr_op.op));
            return nullptr;
        }
        for (auto next_op_maybe = check_binop(curr_op.precedence); next_op_maybe; next_op_maybe = check_binop(curr_op.precedence)) {
            auto const &next_op = *next_op_maybe;
            auto        next_prec = curr_op.precedence + ((next_op.precedence > curr_op.precedence) ? 1 : 0);
            token = lexer.peek();
            rhs = parse_expr(rhs, next_prec);
            if (!rhs) {
                append(token, "Expected right hand side operand following infix operator `{}`", Operator_name(next_op.op));
                return nullptr;
            }
        }
        lhs = make_node<BinaryExpression>(lhs->location + rhs->location, lhs, curr_op.op, rhs);
    }
    return lhs;
}

std::optional<OperatorDef> Parser::check_binop(Precedence min_prec)
{
    auto const &token = lexer.peek();
    if (token.kind != TokenKind::Symbol && token.kind != TokenKind::Keyword) {
        return {};
    }
    for (auto const &def : operators) {
        if (def.position != Position::Infix) {
            continue;
        }
        if (!std::visit(overloads {
                            [&token](wchar_t sym) { return token.matches_symbol(sym); },
                            [&token](ArwenKeyword sym) { return token.matches_keyword(sym); } },
                def.sym)) {
            continue;
        }
        if (def.precedence < min_prec) {
            continue;
        }
        if (def.associativity == Associativity::Right || def.precedence > min_prec) {
            return def;
        }
        return def;
    }
    return {};
}

std::optional<OperatorDef> Parser::check_prefix_op()
{
    auto const &token = lexer.peek();
    if (token.kind != TokenKind::Symbol && token.kind != TokenKind::Keyword) {
        return {};
    }
    for (auto const &def : operators) {
        if (def.position != Position::Prefix) {
            continue;
        }
        if (!std::visit(overloads {
                            [&token](wchar_t sym) { return token.matches_symbol(sym); },
                            [&token](ArwenKeyword sym) { return token.matches_keyword(sym); } },
                def.sym)) {
            continue;
        }
        return def;
    }
    return {};
}

pTypeSpecification Parser::parse_type()
{
    TypeFlag flags { TypeFlag::None };
    auto     t = lexer.peek();
    auto     start_location = t.location;
    if (lexer.accept_symbol('[')) {
        if (auto err = lexer.expect_symbol(']'); err.is_error()) {
            append(t, "Expected `]' to close slice type specification");
            return nullptr;
        }
        flags |= TypeFlag::Slice;
    }
    auto name = lexer.expect_identifier();
    if (name.is_error()) {
        append(name.error(), "Expected type name");
        return nullptr;
    }
    auto               end_location = name.value().location;
    TypeSpecifications arguments;
    if (lexer.accept_symbol('<')) {
        while (true) {
            auto arg = parse_type();
            if (arg == nullptr) {
                append(lexer.peek(), "Expected type specification");
                return nullptr;
            }
            end_location = arg->location;
            arguments.push_back(arg);
            auto t = lexer.peek();
            if (t.matches_symbol('>')) {
                lexer.lex();
                end_location = t.location;
                break;
            }
            if (auto err = lexer.expect_symbol(','); err.is_error()) {
                append(err.error(), "Expected `,` or `>`");
                return nullptr;
            }
            t = lexer.peek();
            if (t.matches_symbol('>')) {
                lexer.lex();
                end_location = t.location;
                break;
            }
        }
    }
    pTypeSpecification error_type { nullptr };
    if (lexer.accept_symbol('/')) {
        error_type = parse_type();
        if (error_type == nullptr) {
            append(lexer.peek(), "Expected error type");
            return nullptr;
        }
        end_location = error_type->location;
    }
    bool optional { false };
    t = lexer.peek();
    if (t.matches_symbol('?')) {
        lexer.lex();
        flags |= TypeFlag::Optional;
        end_location = t.location;
    }
    return make_node<TypeSpecification>(
        start_location + end_location, std::wstring { text_of(name) }, arguments, error_type, flags);
}

pSyntaxNode Parser::parse_break_continue()
{
    auto kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Break) || kw.matches_keyword(ArwenKeyword::Continue));
    Label label {};
    if (lexer.accept_symbol(':')) {
        auto lbl = lexer.peek();
        if (!lbl.matches(TokenKind::Identifier)) {
            append(lbl, "Expected label name after `:`");
            return nullptr;
        }
        label = text_of(lbl);
    }
    if (kw.matches_keyword(ArwenKeyword::Break)) {
        return make_node<Break>(kw.location, label);
    }
    return make_node<Continue>(kw.location, label);
}

pSyntaxNode Parser::parse_embed()
{
    auto kw = lexer.lex();
    auto token = lexer.peek();
    if (auto res = lexer.expect_symbol('('); res.is_error()) {
        append(res.error());
        return nullptr;
    }
    lexer.lex();
    auto file_name = lexer.expect(TokenKind::QuotedString);
    if (file_name.is_error()) {
        append(file_name.error());
        return nullptr;
    }
    auto fname = text_of(file_name.value());
    fname = fname.substr(0, fname.length() - 1).substr(1);
    auto close_paren = lexer.peek();
    if (auto res = lexer.expect_symbol(')'); res.is_error()) {
        append(res.error());
        return nullptr;
    }
    return make_node<Embed>(kw.location + close_paren.location, fname);
}

pSyntaxNode Parser::parse_defer()
{
    auto kw = lexer.lex();
    if (auto stmt = parse_statement(); stmt == nullptr) {
        append(kw, "Could not parse defer statement");
        return nullptr;
    } else {
        return make_node<DeferStatement>(kw.location + stmt->location, stmt);
    }
}

pSyntaxNode Parser::parse_for()
{
    Label         label;
    TokenLocation location;
    if (lexer.has_lookback(1)
        && lexer.lookback(0).matches_symbol(':')
        && lexer.lookback(1).matches(TokenKind::Identifier)) {
        label = text_of(lexer.lookback(1));
        location = lexer.lookback(1).location;
    }
    auto for_token = lexer.lex();
    assert(for_token.matches_keyword(ArwenKeyword::For));
    if (!label.has_value()) {
        location = for_token.location;
    }

    auto var_name = lexer.peek();
    if (auto res = lexer.expect_identifier(); res.is_error()) {
        append(res.error(), "Expected `for` range variable name");
        return nullptr;
    }
    auto token = lexer.peek();
    if (token.matches(TokenKind::Identifier) && text_of(token) == L"in") {
        lexer.lex();
    }
    token = lexer.peek();
    auto condition = parse_top_expression();
    if (condition == nullptr) {
        append(token, "Error parsing `for` range");
        return nullptr;
    }
    token = lexer.peek();
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        append(token, "Error parsing `for` block");
        return nullptr;
    }
    auto ret = make_node<ForStatement>(location + stmt->location, std::wstring { text_of(var_name) }, condition, stmt);
    return ret;
}

pSyntaxNode Parser::parse_func()
{
    auto func = lexer.lex();
    level = ParseLevel::Function;
    Defer        defer { [this]() { level = ParseLevel::Module; } };
    std::wstring name;
    if (auto res = lexer.expect_identifier(); res.is_error()) {
        append(res.error(), "Expected function name");
        return nullptr;
    } else {
        name = text_of(res.value());
    }
    if (auto res = lexer.expect_symbol('('); res.is_error()) {
        append(res.error(), "Expected '(' in function definition");
    }
    std::vector<pParameter> params;
    while (true) {
        if (lexer.accept_symbol(')')) {
            break;
        }
        std::wstring  param_name;
        TokenLocation start;
        if (auto res = lexer.expect_identifier(); res.is_error()) {
            append(res.error(), "Expected parameter name");
            return nullptr;
        } else {
            param_name = text_of(res.value());
            start = res.value().location;
        }
        if (auto res = lexer.expect_symbol(':'); res.is_error()) {
            append(res.error(), "Expected ':' in function parameter declaration");
        }
        auto          param_type = parse_type();
        TokenLocation end;
        if (param_type == nullptr) {
            append(lexer.peek(), "Expected parameter type");
            return nullptr;
        }
        params.emplace_back(make_node<Parameter>(start + param_type->location, param_name, param_type));
        if (lexer.accept_symbol(')')) {
            break;
        }
        if (auto res = lexer.expect_symbol(','); res.is_error()) {
            append(res.error(), "Expected ',' in function signature");
        }
    }
    auto          return_type = parse_type();
    TokenLocation return_type_loc;
    if (return_type == nullptr) {
        append(lexer.peek(), "Expected return type");
        return nullptr;
    }
    auto decl = make_node<FunctionDeclaration>(func.location + return_type->location, name, params, return_type);
    if (lexer.accept_keyword(ArwenKeyword::ExternLink)) {
        if (auto res = lexer.expect_identifier(); res.is_error()) {
            append(res.error(), "Expected extern function name");
            return nullptr;
        } else {
            auto name = text_of(res.value());
            if (name.length() <= 2) {
                append(res.value(), "Invalid extern function name");
            }
            name = name.substr(0, name.size() - 1).substr(1);
            return make_node<FunctionDefinition>(
                decl->location + res.value().location,
                decl, make_node<ExternLink>(res.value().location, std::wstring { name }));
        }
    }
    if (auto impl = parse_statement(); impl != nullptr) {
        return make_node<FunctionDefinition>(decl->location + impl->location, decl, impl);
    }
    return nullptr;
}

pSyntaxNode Parser::parse_if()
{
    auto if_token = lexer.lex();
    assert(if_token.matches_keyword(ArwenKeyword::If));
    auto condition = parse_top_expression();
    if (condition == nullptr) {
        append(if_token, "Error parsing `if` condition");
        return nullptr;
    }
    auto if_branch = parse_statement();
    if (if_branch == nullptr) {
        append(if_token, "Error parsing `if` branch");
        return nullptr;
    }
    pSyntaxNode else_branch { nullptr };
    auto        else_kw = lexer.peek();
    if (lexer.accept_keyword(ArwenKeyword::Else)) {
        else_branch = parse_statement();
        if (else_branch == nullptr) {
            append(else_kw, "Error parsing `else` branch");
            return nullptr;
        }
    }
    return make_node<IfStatement>(
        if_token.location + (else_branch != nullptr ? else_branch->location : if_branch->location),
        condition, if_branch, else_branch);
}

pSyntaxNode Parser::parse_include()
{
    auto kw = lexer.lex();
    if (auto res = lexer.expect_symbol('('); res.is_error()) {
        append(res.error(), "Malformed '@include' statement: expected '('");
        return nullptr;
    }
    auto file_name = lexer.expect(TokenKind::QuotedString);
    if (file_name.is_error()) {
        append(file_name.error(), "Malformed '@include' statement: no file name");
        return nullptr;
    }
    auto fname = text_of(file_name.value());
    fname = fname.substr(0, fname.length() - 1).substr(1);
    auto close_paren = lexer.peek();
    if (auto res = lexer.expect_symbol(')'); res.is_error()) {
        append(res.error(), L"Malformed '@include' statement: expected ')', got '{}'", text_of(res.error().location));
        return nullptr;
    }
    return make_node<Include>(kw.location + close_paren.location, fname);
}

pSyntaxNode Parser::parse_loop()
{
    Label         label;
    TokenLocation location;
    if (lexer.has_lookback(1)
        && lexer.lookback(0).matches_symbol(':')
        && lexer.lookback(1).matches(TokenKind::Identifier)) {
        label = text_of(lexer.lookback(1));
        location = lexer.lookback(1).location;
    }
    auto loop_token = lexer.lex();
    assert(loop_token.matches_keyword(ArwenKeyword::Loop));
    if (!label.has_value()) {
        location = loop_token.location;
    }
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        append(loop_token, "Error parsing `loop` block");
        return nullptr;
    }
    auto ret = make_node<LoopStatement>(location + stmt->location, label, stmt);
    return ret;
}

pSyntaxNode Parser::parse_return_error()
{
    auto kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Return) || kw.matches_keyword(ArwenKeyword::Error));
    auto expr = parse_top_expression();
    if (expr == nullptr) {
        append(kw.location, "Error parsing return expression");
        return nullptr;
    }
    if (kw.matches_keyword(ArwenKeyword::Return)) {
        return make_node<Return>(kw.location + expr->location, expr);
    }
    return make_node<Error>(kw.location + expr->location, expr);
}

pSyntaxNode Parser::parse_var_decl()
{
    assert(lexer.has_lookback(1)
        && lexer.lookback(0).matches_symbol(':')
        && lexer.lookback(1).matches(TokenKind::Identifier));
    bool               is_const = lexer.has_lookback(2) && lexer.lookback(2).matches_keyword(ArwenKeyword::Const);
    auto               name = lexer.lookback(1);
    Token              token = lexer.peek();
    pTypeSpecification type_name { nullptr };
    auto               location = lexer.lookback(is_const ? 2 : 1).location;
    auto               end_location = token.location;
    if (token.matches(TokenKind::Identifier)) {
        type_name = parse_type();
        if (type_name == nullptr) {
            append(lexer.peek(), "Expected variable type specification");
            return nullptr;
        }
        end_location = type_name->location;
    }
    token = lexer.peek();
    pSyntaxNode initializer = nullptr;
    if (token.matches_symbol('=')) {
        lexer.lex();
        initializer = parse_top_expression();
        if (initializer == nullptr) {
            append(token.location, "Error parsing initialization expression");
            return nullptr;
        }
        end_location = initializer->location;
    } else if (!type_name) {
        append(token, "Expected variable initialization expression");
        return nullptr;
    } else {
        end_location = token.location;
    }
    auto ret = make_node<VariableDeclaration>(
        location + end_location,
        std::wstring { text_of(name) },
        type_name,
        initializer,
        is_const);
    return ret;
}

pSyntaxNode Parser::parse_while()
{
    Label         label;
    TokenLocation location;
    if (lexer.has_lookback(1)
        && lexer.lookback(0).matches_symbol(':')
        && lexer.lookback(1).matches(TokenKind::Identifier)) {
        label = text_of(lexer.lookback(1));
        location = lexer.lookback(1).location;
    }
    auto while_token = lexer.lex();
    assert(while_token.matches_keyword(ArwenKeyword::While));
    if (!label.has_value()) {
        location = while_token.location;
    }
    auto condition = parse_top_expression();
    if (condition == nullptr) {
        append(while_token, "Error parsing `while` condition");
        return nullptr;
    }
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        append(while_token, "Error parsing `while` block");
        return nullptr;
    }
    auto ret = make_node<WhileStatement>(location + stmt->location, label, condition, stmt);
    return ret;
}

pSyntaxNode Parser::parse_yield()
{
    auto kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Yield));
    Label label {};
    if (lexer.accept_symbol(':')) {
        if (auto res = lexer.expect_identifier(); res.is_error()) {
            append(res.value(), "Expected label name after `:`");
            return nullptr;
        } else {
            label = text_of(res.value());
        }
    }
    if (auto stmt = parse_statement(); stmt == nullptr) {
        append(kw, "Could not parse yield expression");
        return nullptr;
    } else {
        return make_node<Yield>(kw.location, label, stmt);
    }
}

void Parser::append(LexerErrorMessage const &lexer_error)
{
    append(lexer_error.location, MUST_EVAL(to_wstring(lexer_error.message)));
}

void Parser::append(LexerErrorMessage const &lexer_error, char const *message)
{
    append(lexer_error.location, message);
}

void Parser::append(LexerErrorMessage const &lexer_error, wchar_t const *message)
{
    append(lexer_error.location, message);
}

void Parser::append(Token const &token, wchar_t const *message)
{
    append(token.location, message);
}

void Parser::append(Token const &token, char const *message)
{
    append(token.location, message);
}

void Parser::append(TokenLocation location, std::wstring message)
{
    std::wcerr << location.line + 1 << ":" << location.column + 1 << " " << message << std::endl;
    errors.emplace_back(std::move(location), std::move(message));
}

void Parser::append(TokenLocation location, std::string const &message)
{
    append(std::move(location), MUST_EVAL(to_wstring(message)));
}

void Parser::append(TokenLocation location, wchar_t const *message)
{
    append(std::move(location), std::wstring { message });
}

void Parser::append(TokenLocation location, char const *message)
{
    append(std::move(location), MUST_EVAL(to_wstring(message)));
}

}
