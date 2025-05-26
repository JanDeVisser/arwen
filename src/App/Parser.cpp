/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Type.h"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>

#include <Util/Defer.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Result.h>
#include <Util/StringUtil.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <string_view>

namespace Arwen {

using namespace Util;

std::vector<Parser::OperatorDef> Parser::operators {
    { Operator::Add, '+', 11 },
    { Operator::AddressOf, '@', 14, Position::Prefix, Associativity::Right },
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
    { Operator::BinaryInvert, '~', 14, Position::Prefix, Associativity::Right },
    { Operator::Call, '(', 15 },
    { Operator::Call, ')', 15, Position::Closing },
    { Operator::Cast, ArwenKeyword::Cast, 14 },
    { Operator::Divide, '/', 12 },
    { Operator::Equals, ArwenKeyword::Equals, 8 },
    { Operator::Greater, '>', 8 },
    { Operator::GreaterEqual, ArwenKeyword::GreaterEqual, 8 },
    { Operator::Idempotent, '+', 14, Position::Prefix, Associativity::Right },
    { Operator::Length, '#', 9, Position::Prefix, Associativity::Right },
    { Operator::Less, '<', 8 },
    { Operator::LessEqual, ArwenKeyword::LessEqual, 8 },
    { Operator::LogicalInvert, '!', 14, Position::Prefix, Associativity::Right },
    { Operator::MemberAccess, '.', 15 },
    { Operator::Modulo, '%', 12 },
    { Operator::Multiply, '*', 12 },
    { Operator::Negate, '-', 14, Position::Prefix, Associativity::Right },
    { Operator::NotEqual, ArwenKeyword::NotEqual, 8 },
    { Operator::Range, ArwenKeyword::Range, 2 },
    { Operator::Sequence, ',', 1 },
    { Operator::ShiftLeft, ArwenKeyword::ShiftLeft, 10 },
    { Operator::ShiftRight, ArwenKeyword::ShiftRight, 10 },
    { Operator::Sizeof, ArwenKeyword::Sizeof, 9, Position::Prefix, Associativity::Right },
    { Operator::Subscript, '[', 15, Position::Postfix },
    { Operator::Subscript, ']', 15, Position::Closing },
    { Operator::Subtract, '-', 11 },
};

struct BindingPower {
    int left;
    int right;
};

static BindingPower binding_power(Parser::OperatorDef op)
{
    switch (op.position) {
    case Position::Infix: {
        switch (op.associativity) {
        case Associativity::Left:
            return { op.precedence * 2 - 1, op.precedence * 2 };
        case Associativity::Right:
            return { op.precedence * 2, op.precedence * 2 - 1 };
        }
    }
    case Position::Prefix: {
        return { -1, op.precedence * 2 - 1 };
    }
    case Position::Postfix: {
        return { op.precedence * 2 - 1, -1 };
    }
    case Position::Closing: {
        return { -1, -1 };
    }
    }
}

Parser::Parser()
    : root(std::make_shared<Namespace>(nullptr))
{
    push_namespace(root);
    for (auto const &t : TypeRegistry::the().types) {
        root->register_type(t->name, t);
    }
}

pSyntaxNode Parser::parse_file(std::wstring const &text, pNamespace ns)
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
        return make_node<Block>(
            statements[0]->location + statements.back()->location,
            statements,
            std::make_shared<Namespace>(ns));
    }
}

pModule Parser::parse_module(std::string_view name, std::wstring text)
{
    this->text = text;
    lexer.push_source(text);

    push_new_namespace();
    Defer       pop_ns { [this]() { pop_namespace(); } };
    SyntaxNodes statements;
    if (auto t = parse_statements(statements); !t.matches(TokenKind::EndOfFile)) {
        append(t, "Expected end of file");
        return nullptr;
    }
    if (!statements.empty()) {
        return make_node<Module>(
            statements[0]->location + statements.back()->location,
            as_wstring(name),
            std::move(text),
            statements,
            namespaces.back());
    }
    return nullptr;
}

pSyntaxNode Parser::parse_script(std::wstring text)
{
    this->text = text;
    lexer.push_source(text);

    push_new_namespace();
    Defer       pop_ns { [this]() { pop_namespace(); } };
    SyntaxNodes statements;
    level = ParseLevel::Block;
    if (auto t = parse_statements(statements); !t.matches(TokenKind::EndOfFile)) {
        append(t, "Expected end of file");
        return nullptr;
    }
    if (!statements.empty()) {
        return make_node<Block>(
            statements[0]->location + statements.back()->location,
            statements,
            namespaces.back());
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
    auto t = lexer.peek();
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
        case ArwenKeyword::Enum:
            return parse_enum();
        case ArwenKeyword::Func:
            return parse_func();
        case ArwenKeyword::Import:
            return parse_import();
        case ArwenKeyword::Include:
            return parse_include();
        case ArwenKeyword::Public:
            return parse_public();
        case ArwenKeyword::Struct:
            return parse_struct();
        default:
            break;
        }
    } break;
    default:
        break;
    }
    lexer.lex();
    append(t, L"Unexpected token `{}`", text_of(t));
    return nullptr;
}

pSyntaxNode Parser::parse_statement()
{
    auto t = lexer.peek();
    // trace("parse_statement() t = {} [{}]", as_utf8(text_of(t)), TokenKind_name(t.kind));
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
        return parse_expression();
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
        case ArwenKeyword::Enum:
            return parse_enum();
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
        case ArwenKeyword::Loop:
            return parse_loop();
        case ArwenKeyword::Return:
            return parse_return_error();
        case ArwenKeyword::Struct:
            return parse_struct();
        case ArwenKeyword::While:
            return parse_while();
        case ArwenKeyword::Yield:
            return parse_yield();
        default:
            append(t, "Unexpected keyword `{}` parsing statement", as_utf8(text_of(t)));
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
            push_new_namespace();
            Defer pop_ns { [this]() { pop_namespace(); } };
            if (auto const end_token = parse_statements(block); !end_token.matches_symbol('}')) {
                append(t, "Unexpected end of block");
                return nullptr;
            } else {
                if (block.empty()) {
                    return make_node<Void>(t.location + end_token.location);
                }
                return make_node<Block>(t.location + end_token.location, block, namespaces.back());
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
            if (auto expr = parse_expression(); expr) {
                return expr;
            }
            append(t, "Unexpected symbol `{:c}`", static_cast<char>(t.symbol_code()));
            lexer.lex();
            return nullptr;
        }
    case TokenKind::Raw: {
        auto raw = t.raw_text();
        assert(raw.marker == ArwenInsertBlock::begin);
        lexer.lex();
        if (raw.end) {
            return make_node<Insert>(t.location, text_at(raw.start, *raw.end));
        } else {
            append(t.location, "Unclosed `@insert` block");
            return nullptr;
        }
    }
    default:
        lexer.lex();
        append(t, L"Unexpected token `{}`", text_of(t));
        return nullptr;
    }
}

std::wstring_view Parser::text_at(size_t start, std::optional<size_t> end) const
{
    if (start < text.length()) {
        if (end) {
            return text.substr(start, *end - start);
        } else {
            return text.substr(start);
        }
    }
    return L"";
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

pSyntaxNode Parser::parse_primary()
{
    auto token = lexer.peek();
    // trace("parse_primary() t = {} [{}]", as_utf8(text_of(token)), TokenKind_name(token.kind));
    pSyntaxNode ret { nullptr };
    switch (token.kind) {
    case TokenKind::Number: {
        ret = make_node<Number>(token.location, text_of(token), token.number_type());
        lexer.lex();
        break;
    }
    case TokenKind::QuotedString: {
        lexer.lex();
        if (token.quoted_string().quote_type == QuoteType::SingleQuote && token.location.length != 1) {
            append(token, "Single quoted string should contain exactly one character");
            return nullptr;
        }
        ret = make_node<QuotedString>(token.location, text_of(token), token.quoted_string().quote_type);
        break;
    }
    case TokenKind::Identifier: {
        lexer.lex();
        auto bm = lexer.bookmark();
        if (lexer.accept_symbol('<')) {
            TypeSpecifications specs;
            while (true) {
                auto spec = parse_type();
                if (spec == nullptr) {
                    break;
                }
                specs.push_back(spec);
                if (lexer.accept_symbol('>')) {
                    return make_node<StampedIdentifier>(token.location + lexer.location(), text_of(token), specs);
                }
                if (!lexer.accept_symbol(',')) {
                    break;
                }
            }
        }
        lexer.push_back(bm);
        ret = make_node<Identifier>(token.location, text_of(token));
        break;
    }
    case TokenKind::Keyword:
        if (token.matches_keyword(ArwenKeyword::Embed)) {
            return parse_embed();
        }
        if (token.matches_keyword(ArwenKeyword::Include)) {
            return parse_include();
        }
        if (token.matches_keyword(ArwenKeyword::False)) {
            return make_node<BoolConstant>(token.location, false);
        }
        if (token.matches_keyword(ArwenKeyword::True)) {
            return make_node<BoolConstant>(token.location, true);
        }
        if (auto const op_maybe = check_prefix_op(); op_maybe) {
            auto &op = *op_maybe;
            auto  bp = binding_power(op);
            auto  op_token = lexer.lex();
            auto  operand = (op.op == Operator::Sizeof) ? parse_type() : parse_expression(bp.right);
            if (!operand) {
                append(token, "Expected operand following prefix operator '{}'", Operator_name(op.op));
                return nullptr;
            }
            ret = make_node<UnaryExpression>(op_token.location + operand->location, op.op, operand);
            break;
        }
        append(token, "Unexpected keyword '{}' parsing primary expression", ArwenKeyword_name(token.keyword()));
        return nullptr;
    case TokenKind::Symbol: {
        if (token.symbol_code() == L'(') {
            lexer.lex();
            if (lexer.accept_symbol(')')) {
                return make_node<Void>(token.location);
            }
            ret = parse_expression();
            if (auto err = lexer.expect_symbol(')'); err.is_error()) {
                append(err.error(), "Expected ')'");
                return nullptr;
            }
            break;
        }
        if (auto const op_maybe = check_prefix_op(); op_maybe) {
            auto &op = *op_maybe;
            auto  bp = binding_power(op);
            auto  op_token = lexer.lex();
            auto  operand = parse_expression(bp.right);
            if (!operand) {
                append(token, "Expected operand following prefix operator '{}'", Operator_name(op.op));
                return nullptr;
            }
            ret = make_node<UnaryExpression>(op_token.location + operand->location, op.op, operand);
            break;
        }
    } // Fall through
    default:
        append(token, L"Unexpected token {} `{}`", as_wstring(TokenKind_name(token.kind)), text_of(token));
        ret = nullptr;
    }
    if (ret == nullptr) {
        append(token, "Expected primary expression");
    }
    return ret;
}

// Shamelessly stolen from here:
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
pSyntaxNode Parser::parse_expression(Precedence min_prec)
{
    auto lhs = parse_primary();
    // trace("parse_expression({}) lhs = {}", min_prec, SyntaxNodeType_name(lhs->type));
    if (lhs == nullptr) {
        return nullptr;
    }
    while (!lexer.next_matches(TokenKind::EndOfFile) && check_op()) {
        if (auto op_maybe = check_postfix_op(); op_maybe) {
            auto op = op_maybe.value();
            auto bp = binding_power(op);
            // trace("parse_expression({}) postfix op = {} ({})", min_prec, Operator_name(op.op), bp.left);
            if (bp.left < min_prec) {
                break;
            }
            if (op.op == Operator::Subscript) {
                lexer.lex();
                auto rhs = parse_expression();
                if (rhs == nullptr) {
                    append(lexer.peek().location, "Expected subscript expression");
                    return nullptr;
                }
                if (auto err = lexer.expect_symbol(']'); err.is_error()) {
                    append(err.error(), "Expected ']'");
                    return nullptr;
                }
                lhs = make_node<BinaryExpression>(lhs->location + rhs->location, lhs, op_maybe->op, rhs);
            } else {
                lhs = make_node<UnaryExpression>(lhs->location + lexer.peek().location, op_maybe->op, lhs);
                lexer.lex();
            }
            continue;
        }
        if (auto op_maybe = check_binop(); op_maybe) {
            auto op = op_maybe.value();
            auto bp = binding_power(op);
            // trace("parse_expression({}) infix op = {} ({})", min_prec, Operator_name(op.op), bp.left);
            if (bp.left < min_prec) {
                break;
            }
            if (op.op == Operator::Call) {
                // Don't lex the '(' so parse_primary will return a
                // single expression, probably a binop with op = ','.
                auto param_list = parse_primary();
                if (param_list == nullptr) {
                    append(lhs->location, "Could not parse function call argument list");
                    return nullptr;
                }
                // trace("parse_expression() param_list = {}", SyntaxNodeType_name(param_list->type));
                return make_node<BinaryExpression>(lhs->location + param_list->location, lhs, Operator::Call, param_list);
            }
            auto token = lexer.lex();
            auto rhs = (op.op == Operator::Cast) ? parse_type() : parse_expression(bp.right);
            if (rhs == nullptr) {
                return nullptr;
            }
            // trace("parse_expression({}) rhs = {}", min_prec, SyntaxNodeType_name(rhs->type));
            lhs = make_node<BinaryExpression>(lhs->location + rhs->location, lhs, op.op, rhs);
            continue;
        }
        break;
    }
    return lhs;
}

bool Parser::check_op()
{
    auto const &token = lexer.peek();
    if (!token.matches(TokenKind::Symbol) && !token.matches(TokenKind::Keyword)) {
        return false;
    }
    return std::any_of(
        operators.begin(),
        operators.end(),
        [&token](auto const &def) -> bool {
            return std::visit(overloads {
                                  [&token](wchar_t sym) { return token.matches_symbol(sym); },
                                  [&token](ArwenKeyword sym) { return token.matches_keyword(sym); } },
                def.sym);
        });
}

std::optional<Parser::OperatorDef> Parser::check_binop()
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
        return def;
    }
    return {};
}

std::optional<Parser::OperatorDef> Parser::check_prefix_op()
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

std::optional<Parser::OperatorDef> Parser::check_postfix_op()
{
    auto const &token = lexer.peek();
    if (token.kind != TokenKind::Symbol && token.kind != TokenKind::Keyword) {
        return {};
    }
    for (auto const &def : operators) {
        if (def.position != Position::Postfix) {
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
    auto t = lexer.peek();
    if (lexer.accept_symbol('&')) {
        if (auto type = parse_type(); type != nullptr) {
            return make_node<TypeSpecification>(t.location + type->location, ReferenceDescriptionNode { type });
        }
        return nullptr;
    }
    if (lexer.accept_symbol('[')) {
        if (lexer.accept_symbol(']')) {
            if (auto type = parse_type(); type != nullptr) {
                return make_node<TypeSpecification>(t.location + type->location, SliceDescriptionNode { type });
            }
            return nullptr;
        }
        if (lexer.accept_symbol('0')) {
            if (auto err = lexer.expect_symbol(']'); err.is_error()) {
                append(err.error(), "Expected `]` to close `[0`");
                return nullptr;
            }
            if (auto type = parse_type(); type != nullptr) {
                return make_node<TypeSpecification>(t.location + type->location, ZeroTerminatedArrayDescriptionNode { type });
            }
            return nullptr;
        }
        if (lexer.accept_symbol('*')) {
            if (auto err = lexer.expect_symbol(']'); err.is_error()) {
                append(err.error(), "Expected `]` to close `[*`");
                return nullptr;
            }
            if (auto type = parse_type(); type != nullptr) {
                return make_node<TypeSpecification>(t.location + type->location, ZeroTerminatedArrayDescriptionNode { type });
            }
            return nullptr;
        }
        if (auto res = lexer.expect(TokenKind::Number); res.is_error()) {
            append(res.error(), "Expected array size, `0`, or `]`");
            return nullptr;
        } else if (res.value().number_type() == NumberType::Decimal) {
            append(res.error(), "Array size must be integer");
            return nullptr;
        } else {
            if (auto err = lexer.expect_symbol(']'); err.is_error()) {
                append(err.error(), "Expected `]` to close array descriptor");
                return nullptr;
            }
            auto size = string_to_integer<size_t>(text_of(res.value()));
            assert(size.has_value());
            if (auto type = parse_type(); type != nullptr) {
                return make_node<TypeSpecification>(t.location + type->location, ArrayDescriptionNode { type, size.value() });
            }
            return nullptr;
        }
    }

    auto name = lexer.accept_identifier();
    if (!name) {
        append(lexer.peek(), "Expected type name");
        return nullptr;
    }
    TypeSpecifications arguments;
    if (lexer.accept_symbol('<')) {
        while (true) {
            if (lexer.accept_symbol('>')) {
                break;
            }
            auto arg = parse_type();
            if (arg == nullptr) {
                append(lexer.peek(), "Expected template type specification");
                return nullptr;
            }
            arguments.push_back(arg);
            auto t = lexer.peek();
            if (lexer.accept_symbol('>')) {
                break;
            }
            if (auto err = lexer.expect_symbol(','); err.is_error()) {
                append(err.error(), "Expected `,` or `>`");
                return nullptr;
            }
        }
    }
    auto type = make_node<TypeSpecification>(
        name->location + lexer.last_location,
        TypeNameNode { std::wstring { text_of(name.value()) }, arguments });
    if (lexer.accept_symbol('?')) {
        type = make_node<TypeSpecification>(
            name->location + lexer.last_location,
            OptionalDescriptionNode { type });
    }
    if (lexer.accept_symbol('/')) {
        if (auto error_type = parse_type(); error_type != nullptr) {
            return make_node<TypeSpecification>(
                name->location + lexer.last_location,
                ErrorDescriptionNode { type, error_type });
        }
        return nullptr;
    }
    return type;
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
    auto file_name = lexer.expect(TokenKind::QuotedString);
    if (file_name.is_error()) {
        append(file_name.error());
        return nullptr;
    }
    auto fname = text_of(file_name.value());
    fname = fname.substr(0, fname.length() - 1).substr(1);
    if (auto res = lexer.expect_symbol(')'); res.is_error()) {
        append(lexer.location(), "Expected `)`");
        return nullptr;
    }
    return make_node<Embed>(kw.location + lexer.location(), fname);
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

pSyntaxNode Parser::parse_enum()
{
    auto enum_token = lexer.lex();
    assert(enum_token.matches_keyword(ArwenKeyword::Enum));

    auto name = lexer.expect_identifier();
    if (name.is_error()) {
        append(lexer.last_location, "Expected enum name");
        return nullptr;
    }
    pTypeSpecification underlying { nullptr };
    if (lexer.accept_symbol(':')) {
        if (underlying = parse_type(); underlying == nullptr) {
            append(lexer.last_location, "Expected underlying type after `:`");
            return nullptr;
        }
    }
    if (auto res = lexer.expect_symbol('{'); res.is_error()) {
        append(res.error().location, res.error().message);
        return nullptr;
    }
    EnumValues values;
    while (!lexer.accept_symbol('}')) {
        auto label = lexer.expect_identifier();
        if (label.is_error()) {
            append(label.error().location, label.error().message);
            return nullptr;
        }
        pTypeSpecification payload { nullptr };
        if (lexer.accept_symbol('(')) {
            payload = parse_type();
            if (payload == nullptr) {
                append(lexer.last_location, "Expected enum value payload type");
                return nullptr;
            }
            if (auto err = lexer.expect_symbol(')'); err.is_error()) {
                append(lexer.last_location, "Expected `)` to close enum value payload type");
                return nullptr;
            }
        }
        pSyntaxNode value_node { nullptr };
        if (lexer.accept_symbol('=')) {
            auto value = lexer.peek();
            if (!value.matches(TokenKind::Number) || value.number_type() == NumberType::Decimal) {
                append(value.location, "Expected enum value"); // Make better
                return nullptr;
            }
            lexer.lex();
            value_node = make_node<Number>(value.location, text_of(value), value.number_type());
        }
        values.emplace_back(make_node<EnumValue>(
            label.value().location + lexer.last_location,
            std::wstring { text_of(label) },
            value_node,
            payload));
        if (!lexer.accept_symbol(',') && !lexer.next_matches('}')) {
            append(lexer.last_location, "Expected `,` or `}`");
            return nullptr;
        }
    }
    return make_node<Enum>(
        enum_token.location + lexer.last_location,
        std::wstring { text_of(name) },
        underlying,
        values);
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
    push_new_namespace();
    Defer pop_for_ns { [this] { pop_namespace(); } };
    auto  range = parse_expression();
    if (range == nullptr) {
        append(token, "Error parsing `for` range");
        return nullptr;
    }
    token = lexer.peek();
    auto stmt = parse_statement();
    if (stmt == nullptr) {
        append(token, "Error parsing `for` block");
        return nullptr;
    }
    return make_node<ForStatement>(location + stmt->location, std::wstring { text_of(var_name) }, range, stmt, namespaces.back());
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

    std::vector<pIdentifier> generics;
    if (lexer.accept_symbol('<')) {
        while (true) {
            if (lexer.accept_symbol('>')) {
                break;
            }
            std::wstring  generic_name;
            TokenLocation start;
            if (auto res = lexer.expect_identifier(); res.is_error()) {
                append(res.error(), "Expected generic name");
                return nullptr;
            } else {
                generics.emplace_back(make_node<Identifier>(res.value().location, text_of(res.value())));
            }
            if (lexer.accept_symbol('>')) {
                break;
            }
            if (auto res = lexer.expect_symbol(','); res.is_error()) {
                append(res.error(), "Expected ',' in function signature generic list");
            }
        }
    }

    if (auto res = lexer.expect_symbol('('); res.is_error()) {
        append(res.error(), "Expected '(' in function definition");
    }
    push_new_namespace();
    Defer                   pop_def_ns { [this]() { pop_namespace(); } };
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
    auto decl = make_node<FunctionDeclaration>(
        func.location + return_type->location,
        name,
        generics,
        params,
        return_type);
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
                decl->name,
                decl,
                make_node<ExternLink>(res.value().location, std::wstring { name }),
                nullptr);
        }
    }
    if (auto impl = parse_statement(); impl != nullptr) {
        return make_node<FunctionDefinition>(
            decl->location + impl->location,
            decl->name,
            decl,
            impl,
            namespaces.back());
    }
    return nullptr;
}

pSyntaxNode Parser::parse_if()
{
    auto if_token = lexer.lex();
    assert(if_token.matches_keyword(ArwenKeyword::If));
    auto condition = parse_expression();
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

pSyntaxNode Parser::parse_import()
{
    auto import_token = lexer.lex();
    assert(import_token.matches_keyword(ArwenKeyword::Import));
    std::wstring  path;
    TokenLocation end_location = import_token.location;
    do {
        auto ident_maybe = lexer.expect_identifier();
        if (ident_maybe.is_error()) {
            append(ident_maybe.error(), "Expected import path component");
        }
        path += text_of(ident_maybe);
        end_location = ident_maybe.value().location;
        if (!lexer.accept_symbol('.')) {
            break;
        }
        path += '.';
    } while (true);
    return make_node<Import>(import_token.location + end_location, path);
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

pSyntaxNode Parser::parse_public()
{
    auto t = lexer.peek();
    assert(t.matches_keyword(ArwenKeyword::Public));
    lexer.lex();
    auto decl = parse_module_level_statement();
    if (decl == nullptr) {
        return nullptr;
    }
    std::wstring name;
    switch (decl->type) {
    case SyntaxNodeType::Enum:
        name = std::dynamic_pointer_cast<Struct>(decl)->name;
        break;
    case SyntaxNodeType::FunctionDefinition:
        name = std::dynamic_pointer_cast<FunctionDefinition>(decl)->name;
        break;
    case SyntaxNodeType::PublicDeclaration:
        append(decl->location, L"Double public declaration");
        return nullptr;
    case SyntaxNodeType::Struct:
        name = std::dynamic_pointer_cast<Struct>(decl)->name;
        break;
    case SyntaxNodeType::VariableDeclaration:
        name = std::dynamic_pointer_cast<VariableDeclaration>(decl)->name;
        break;
    default:
        append(decl->location, "Cannot declare statement of type `{}` public", SyntaxNodeType_name(decl->type));
        return nullptr;
    }
    return make_node<PublicDeclaration>(t.location + decl->location, name, decl);
}

pSyntaxNode Parser::parse_return_error()
{
    auto kw = lexer.lex();
    assert(kw.matches_keyword(ArwenKeyword::Return) || kw.matches_keyword(ArwenKeyword::Error));
    auto expr = parse_expression();
    if (expr == nullptr) {
        append(kw.location, "Error parsing return expression");
        return nullptr;
    }
    if (kw.matches_keyword(ArwenKeyword::Return)) {
        return make_node<Return>(kw.location + expr->location, expr);
    }
    return make_node<Error>(kw.location + expr->location, expr);
}

pSyntaxNode Parser::parse_struct()
{
    auto struct_token = lexer.lex();
    assert(struct_token.matches_keyword(ArwenKeyword::Struct));

    auto name = lexer.expect_identifier();
    if (name.is_error()) {
        append(lexer.last_location, "Expected struct name");
        return nullptr;
    }
    if (auto res = lexer.expect_symbol('{'); res.is_error()) {
        append(res.error().location, res.error().message);
        return nullptr;
    }
    StructMembers members;
    while (!lexer.accept_symbol('}')) {
        auto label = lexer.expect_identifier();
        if (label.is_error()) {
            append(label.error().location, label.error().message);
            return nullptr;
        }
        if (auto err = lexer.expect_symbol(':'); err.is_error()) {
            append(err.error().location, "Expected `:`");
            return nullptr;
        }
        auto type = parse_type();
        if (type == nullptr) {
            append(lexer.last_location, "Expected struct member type");
            return nullptr;
        }
        members.emplace_back(make_node<StructMember>(
            label.value().location + lexer.last_location,
            std::wstring { text_of(label) },
            type));
        if (!lexer.accept_symbol(',') && !lexer.next_matches('}')) {
            append(lexer.last_location, "Expected `,` or `}`");
            return nullptr;
        }
    }
    return make_node<Struct>(
        struct_token.location + lexer.last_location,
        std::wstring { text_of(name) },
        members);
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
        initializer = parse_expression();
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
    auto condition = parse_expression();
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

pType Parser::type_of(std::wstring const &name) const
{
    return namespaces.back()->type_of(name);
}

bool Parser::has_function(std::wstring const &name, pType const &type) const
{
    return namespaces.back()->find_function(name, type) != nullptr;
}

pFunctionDefinition Parser::find_function(std::wstring const &name, pType const &type) const
{
    assert(type->is<FunctionType>());
    return namespaces.back()->find_function(name, type);
}

pFunctionDefinition Parser::find_function_by_arg_list(std::wstring const &name, pType const &type) const
{
    assert(type->is<TypeList>());
    return namespaces.back()->find_function_by_arg_list(name, type);
}

std::vector<pFunctionDefinition> Parser::find_overloads(std::wstring const &name, TypeSpecifications const &type_args) const
{
    return namespaces.back()->find_overloads(name, type_args);
}

void Parser::register_variable(std::wstring name, pSyntaxNode node)
{
    assert(!namespaces.empty());
    namespaces.back()->register_variable(std::move(name), std::move(node));
}

void Parser::register_function(std::wstring name, pFunctionDefinition fnc)
{
    assert(!namespaces.empty());
    namespaces.back()->register_function(std::move(name), std::move(fnc));
}

void Parser::unregister_function(std::wstring name, pFunctionDefinition fnc)
{
    assert(!namespaces.empty());
    namespaces.back()->unregister_function(std::move(name), std::move(fnc));
}

pType Parser::find_type(std::wstring const &name) const
{
    assert(!namespaces.empty());
    return namespaces.back()->find_type(name);
}

void Parser::register_type(std::wstring name, pType type)
{
    assert(!namespaces.empty());
    namespaces.back()->register_type(std::move(name), std::move(type));
}

void Parser::push_namespace(pNamespace const &ns)
{
    assert(ns != nullptr);
    namespaces.emplace_back(ns);
}

pNamespace const &Parser::push_new_namespace()
{
    if (namespaces.empty()) {
        namespaces.emplace_back(std::make_shared<Namespace>(nullptr));
    } else {
        namespaces.emplace_back(std::make_shared<Namespace>(namespaces.back()));
    }
    return namespaces.back();
}

void Parser::pop_namespace()
{
    assert(!namespaces.empty());
    namespaces.pop_back();
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

pType Parser::bind_error(TokenLocation location, std::wstring msg)
{
    append(location, msg);
    return make_error(location, msg);
}
}
