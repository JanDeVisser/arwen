/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Logging.h>

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

pSyntaxNode Parser::parse_module(std::string_view name, std::wstring const &text)
{
    this->text = text;
    lexer.push_source(text);

    SyntaxNodes statements;
    if (auto t = parse_statements(statements); !t.matches(TokenKind::EndOfFile)) {
        std::cerr << "Expected end of file" << std::endl;
        return nullptr;
    }
    return make_node<Module>(name, text, statements);
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
        std::cerr << "Unexpected end of file" << std::endl;
        return nullptr;
    case TokenKind::Identifier:
        lexer.lex();
        if (lexer.peek().matches_symbol(':')) {
            pending_label = text_of(t);
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
        case ArwenKeyword::If:
            return parse_if();
        case ArwenKeyword::Loop:
            return parse_loop();
        case ArwenKeyword::While:
            return parse_while();
        default:
            std::wcerr << "Unexpected keyword '" << text_of(t) << "'." << std::endl;
            lexer.lex();
            return nullptr;
        }
    } break;
    case TokenKind::Symbol:
        switch (t.symbol_code()) {
        case ';':
            pending_label.reset();
            return make_node<Dummy>();
        case '{': {
            pending_label.reset();
            lexer.lex();
            SyntaxNodes block;
            if (auto const t = parse_statements(block); !t.matches_symbol('}')) {
                std::cerr << "Unexpected end of block" << std::endl;
                return nullptr;
            }
            return make_node<Block>(block);
        }
        default:
            if (auto expr = parse_top_expression(); expr) {
                return expr;
            }
            std::wcerr << "Unexpected symbol '" << t.symbol_code() << "'" << std::endl;
            lexer.lex();
            return nullptr;
        }
    default:
        lexer.lex();
        return nullptr;
    }
}

std::wstring_view Parser::text_of(Token const &token)
{
    return text.substr(token.location.index, token.location.length);
}

pSyntaxNode Parser::parse_top_expression()
{
    pending_label.reset();
    auto lhs = parse_primary();
    return (lhs) ? parse_expr(lhs, 0) : nullptr;
}

pSyntaxNode Parser::parse_primary()
{
    auto       &token = lexer.peek();
    pSyntaxNode ret { nullptr };
    switch (token.kind) {
    case TokenKind::Number: {
        ret = make_node<Number>(text_of(token), token.number_type());
        lexer.lex();
        break;
    }
    case TokenKind::QuotedString: {
        ret = make_node<QuotedString>(text_of(token), token.quoted_string().quote_type);
        lexer.lex();
        break;
    }
    case TokenKind::Identifier: {
        ret = make_node<Identifier>(text_of(token));
        lexer.lex();
        break;
    }
    case TokenKind::Keyword:
        std::cerr << "Unexpected keyword `" << ArwenKeyword_name(token.keyword()) << "`\n";
        return nullptr;
    case TokenKind::Symbol: {
        if (token.symbol_code() == L'(') {
            lexer.lex();
            ret = parse_top_expression();
            if (!lexer.peek().matches_symbol(')')) {
                std::cerr << "Expected ')'" << std::endl;
                return nullptr;
            }
            lexer.lex();
            break;
        }
        if (auto const op_maybe = check_prefix_op(); op_maybe) {
            auto &op = *op_maybe;
            lexer.lex();
            auto operand = parse_primary();
            if (!operand) {
                std::cerr << "Expected operand following prefix operator " << Operator_name(op.op) << std::endl;
                return nullptr;
            }
            ret = make_node<UnaryExpression>(op.op, operand);
        }
    } // Fall through
    default:
        std::wcerr << "Unexpected token `" << text_of(token) << "`\n";
        ret = nullptr;
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
            return make_node<BinaryExpression>(lhs, curr_op.op, param_list);
        }
        lexer.lex();
        auto rhs = parse_primary();
        if (!rhs) {
            std::cerr << "Expected right hand side operand following infix operator " << Operator_name(curr_op.op) << std::endl;
            return nullptr;
        }
        for (auto next_op_maybe = check_binop(curr_op.precedence); next_op_maybe; next_op_maybe = check_binop(curr_op.precedence)) {
            auto const &next_op = *next_op_maybe;
            auto        next_prec = curr_op.precedence + ((next_op.precedence > curr_op.precedence) ? 1 : 0);
            rhs = parse_expr(rhs, next_prec);
            if (!rhs) {
                std::cerr << "Expected right hand side operand following infix operator " << Operator_name(next_op.op) << std::endl;
                return nullptr;
            }
        }
        if (curr_op.op == Operator::Call) {
        }
        lhs = make_node<BinaryExpression>(lhs, curr_op.op, rhs);
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
    auto kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Break) || kw.matches_keyword(ArwenKeyword::Continue));
    Label label {};
    if (lexer.accept_symbol(':')) {
        auto lbl = lexer.peek();
        if (!lbl.matches(TokenKind::Identifier)) {
            std::cerr << "Expected label name after `:`" << std::endl;
            return nullptr;
        }
        label = text_of(lbl);
    }
    if (kw.matches_keyword(ArwenKeyword::Break)) {
        return make_node<Break>(label);
    }
    return make_node<Continue>(label);
}

pSyntaxNode Parser::parse_if()
{
    auto const &if_token = lexer.lex();
    assert(if_token.matches_keyword(ArwenKeyword::If));
    pending_label.reset();
    auto condition = parse_top_expression();
    if (condition == nullptr) {
        std::cerr << "Error parsing `if` condition" << std::endl;
        return nullptr;
    }
    auto if_branch = parse_statement();
    if (if_branch == nullptr) {
        std::cerr << "Error parsing `if` branch" << std::endl;
        return nullptr;
    }
    pSyntaxNode else_branch { nullptr };
    if (lexer.accept_keyword(ArwenKeyword::Else)) {
        else_branch = parse_statement();
        if (else_branch == nullptr) {
            std::cerr << "Error parsing `else` branch" << std::endl;
            return nullptr;
        }
    }
    return make_node<IfStatement>(condition, if_branch, else_branch);
}

pSyntaxNode Parser::parse_loop()
{
    auto label = pending_label;
    pending_label.reset();
    auto const &loop_token = lexer.lex();
    assert(loop_token.matches_keyword(ArwenKeyword::Loop));
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        std::cerr << "Error parsing `loop` block" << std::endl;
        return nullptr;
    }
    auto ret = make_node<LoopStatement>(label, stmt);
    return ret;
}

pSyntaxNode Parser::parse_while()
{
    auto label = pending_label;
    pending_label.reset();
    auto const &while_token = lexer.lex();
    assert(while_token.matches_keyword(ArwenKeyword::While));
    auto condition = parse_top_expression();
    if (condition == nullptr) {
        std::cerr << "Error parsing `while` condition" << std::endl;
        return nullptr;
    }
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        std::cerr << "Error parsing `while` block" << std::endl;
        return nullptr;
    }
    auto ret = make_node<WhileStatement>(label, condition, stmt);
    return ret;
}

}
