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
        if (auto stmt = parse_statement(); stmt != nullptr) {
            statements.push_back(stmt);
        }
    }
}

pSyntaxNode Parser::parse_statement()
{
    auto const t = lexer.peek();
    switch (t.kind) {
    case TokenKind::EndOfFile:
        append(t, "Unexpected end of file");
        return nullptr;
    case TokenKind::Identifier:
        if (pending_id) {
            // This is the type of a variable decl:
            return parse_var_decl();
        }
        lexer.lex();
        if (lexer.peek().matches_symbol(':')) {
            pending_id = t;
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
        case ArwenKeyword::Embed:
            return parse_embed();
        case ArwenKeyword::Func:
            return parse_func();
        case ArwenKeyword::If:
            return parse_if();
        case ArwenKeyword::Include:
            return parse_include();
        case ArwenKeyword::Return:
            return parse_return();
        case ArwenKeyword::Loop:
            return parse_loop();
        case ArwenKeyword::While:
            return parse_while();
        default:
            append(t, "Unexpected keyword '{}'", as_utf8(text_of(t)));
            lexer.lex();
            return nullptr;
        }
    } break;
    case TokenKind::Symbol:
        switch (t.symbol_code()) {
        case ';':
            pending_id.reset();
            lexer.lex();
            return make_node<Dummy>(lexer.lex().location);
        case '{': {
            pending_id.reset();
            lexer.lex();
            SyntaxNodes block;
            auto        old_level = level;
            level = ParseLevel::Block;
            Defer defer { [this, old_level]() { level = old_level; } };
            if (auto const t = parse_statements(block); !t.matches_symbol('}')) {
                append(t, "Unexpected end of block");
                return nullptr;
            }
            return make_node<Block>(block[0]->location + block.back()->location, block);
        }
        case '=':
            if (pending_id) {
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
    return text.substr(location.index, location.length);
}

pSyntaxNode Parser::parse_top_expression()
{
    pending_id.reset();
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

pSyntaxNode Parser::parse_break_continue()
{
    pending_id.reset();
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
    pending_id.reset();
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

pSyntaxNode Parser::parse_func()
{
    pending_id.reset();
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
        std::wstring  param_type;
        TokenLocation end;
        if (auto res = lexer.expect_identifier(); res.is_error()) {
            append(res.error(), "Expected parameter type");
            return nullptr;
        } else {
            param_type = text_of(res.value());
            end = res.value().location;
        }
        params.emplace_back(make_node<Parameter>(start + end, param_name, param_type));
        if (lexer.accept_symbol(')')) {
            break;
        }
        if (auto res = lexer.expect_symbol(','); res.is_error()) {
            append(res.error(), "Expected ',' in function signature");
        }
        if (lexer.accept_symbol(')')) {
            break;
        }
    }
    std::wstring  return_type;
    TokenLocation return_type_loc;
    if (auto res = lexer.expect_identifier(); res.is_error()) {
        append(res.error(), "Expected return type");
        return nullptr;
    } else {
        return_type = text_of(res.value());
        return_type_loc = res.value().location;
    }
    auto decl = make_node<FunctionDeclaration>(func.location + return_type_loc, name, params, return_type);
    if (auto impl = parse_statement(); impl != nullptr) {
        return make_node<FunctionDefinition>(decl->location + impl->location, decl, impl);
    }
    return nullptr;
}

pSyntaxNode Parser::parse_if()
{
    pending_id.reset();
    auto const &if_token = lexer.lex();
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
    return make_node<IfStatement>(if_token.location + (else_branch != nullptr ? else_branch->location : if_branch->location), condition, if_branch, else_branch);
}

pSyntaxNode Parser::parse_include()
{
    pending_id.reset();
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
    auto loop_token = lexer.lex();
    assert(loop_token.matches_keyword(ArwenKeyword::Loop));
    TokenLocation location = loop_token.location;
    Label         label;
    if (pending_id) {
        label = text_of(*pending_id);
        location = pending_id->location;
        pending_id.reset();
    }
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        append(loop_token, "Error parsing `loop` block");
        return nullptr;
    }
    auto ret = make_node<LoopStatement>(location + stmt->location, label, stmt);
    return ret;
}

pSyntaxNode Parser::parse_return()
{
    pending_id.reset();
    auto const &kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Return));
    auto expr = parse_top_expression();
    if (expr == nullptr) {
        append(kw.location, "Error parsing return expression");
        return nullptr;
    }
    return make_node<Return>(kw.location + expr->location, expr);
}

pSyntaxNode Parser::parse_var_decl()
{
    assert(pending_id.has_value());
    auto                        name = pending_id.value();
    Token                       token = lexer.peek();
    std::optional<std::wstring> type_name {};
    auto                        end_location = token.location;
    if (token.matches(TokenKind::Identifier)) {
        type_name = text_of(token);
        end_location = token.location;
        lexer.lex();
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
        name.location + end_location,
        std::wstring { text_of(name) },
        type_name,
        initializer);
    pending_id.reset();
    return ret;
}

pSyntaxNode Parser::parse_while()
{
    auto while_token = lexer.lex();
    assert(while_token.matches_keyword(ArwenKeyword::While));
    TokenLocation location = while_token.location;
    Label         label;
    if (pending_id) {
        label = text_of(*pending_id);
        location = pending_id->location;
        pending_id.reset();
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
    errors.emplace_back(std::move(location), std::move(message));
}

void Parser::append(TokenLocation location, std::string const &message)
{
    errors.emplace_back(std::move(location), MUST_EVAL(to_wstring(message)));
}

void Parser::append(TokenLocation location, wchar_t const *message)
{
    errors.emplace_back(std::move(location), std::wstring { message });
}

void Parser::append(TokenLocation location, char const *message)
{
    errors.emplace_back(std::move(location), MUST_EVAL(to_wstring(message)));
}

}
