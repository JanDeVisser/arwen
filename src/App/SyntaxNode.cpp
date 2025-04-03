/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Util/Lexer.h"
#include <iostream>
#include <memory>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <Util/Logging.h>
#include <Util/StringUtil.h>
#include <string_view>
#include <unistd.h>

namespace Arwen {

char const *SyntaxNodeType_name(SyntaxNodeType type)
{
    switch (type) {
#undef S
#define S(T)                \
    case SyntaxNodeType::T: \
        return #T;
        SyntaxNodeTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

void print_indent(int indent)
{
    printf("%*.*s", indent, indent, "");
}

SyntaxNode::SyntaxNode(SyntaxNodeType type)
    : type(type)
{
}

void SyntaxNode::dump(int indent)
{
    print_indent(indent);
    std::cout << SyntaxNodeType_name(type) << " (" << location.index << ".." << location.index + location.length << ") ";
    header();
    std::cout << std::endl;
    dump_node(indent);
}

void SyntaxNode::header()
{
}

void SyntaxNode::dump_node(int indent)
{
}

pSyntaxNode SyntaxNode::normalize()
{
    return this->shared_from_this();
}

ConstantExpression::ConstantExpression(SyntaxNodeType type)
    : SyntaxNode(type)
{
}

pSyntaxNode ConstantExpression::evaluate_binop(Operator op, pConstantExpression const &rhs)
{
    switch (op) {
#undef S
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_##O(rhs);
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

Block::Block(SyntaxNodes statements)
    : SyntaxNode(SyntaxNodeType::Block)
    , statements(std::move(statements))
{
}

pSyntaxNode Block::normalize()
{
    SyntaxNodes normalized;
    for (auto const &stmt : statements) {
        normalized.emplace_back(stmt->normalize());
    }
    return make_node<Block>(location, normalized);
}

pBoundNode Block::bind()
{
    return nullptr;
}

void Block::dump_node(int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

BoolConstant::BoolConstant(bool value)
    : ConstantExpression(SyntaxNodeType::BoolConstant)
    , value(value)
{
}

pBoundNode BoolConstant::bind()
{
    return nullptr;
}

void BoolConstant::header()
{
    std::wcout << ((value) ? L"True" : L"False");
}

Break::Break(Label label)
    : SyntaxNode(SyntaxNodeType::Break)
    , label(std::move(label))
{
}

pBoundNode Break::bind()
{
    return nullptr;
}

void Break::header()
{
    if (label) {
        std::wcout << *label;
    }
}

Continue::Continue(Label label)
    : SyntaxNode(SyntaxNodeType::Continue)
    , label(std::move(label))
{
}

pBoundNode Continue::bind()
{
    return nullptr;
}

void Continue::header()
{
    if (label) {
        std::wcout << *label;
    }
}
DeferStatement::DeferStatement(pSyntaxNode stmt)
    : SyntaxNode(SyntaxNodeType::DeferStatement)
    , stmt(std::move(stmt))
{
}

pSyntaxNode DeferStatement::normalize()
{
    return make_node<DeferStatement>(
        location,
        stmt->normalize());
}

pBoundNode DeferStatement::bind()
{
    return nullptr;
}

void DeferStatement::dump_node(int indent)
{
    stmt->dump(indent + 4);
}

DoubleQuotedString::DoubleQuotedString(std::wstring_view string, bool strip_quotes)
    : ConstantExpression(SyntaxNodeType::DoubleQuotedString)
    , string(strip_quotes ? string.substr(0, string.length() - 1).substr(1) : string)
{
}

pBoundNode DoubleQuotedString::bind()
{
    return nullptr;
}

void DoubleQuotedString::header()
{
    std::wcout << string;
}

pSyntaxNode DoubleQuotedString::evaluate_Add(pConstantExpression const &rhs)
{
    if (auto rhs_string = std::dynamic_pointer_cast<DoubleQuotedString>(rhs); rhs_string != nullptr) {
        return make_node<DoubleQuotedString>(location + rhs->location, string + rhs_string->string, false);
    }
    return nullptr;
}

Dummy::Dummy()
    : SyntaxNode(SyntaxNodeType::Dummy)
{
}

pBoundNode Dummy::bind()
{
    return nullptr;
}

ExpressionList::ExpressionList(SyntaxNodes expressions)
    : SyntaxNode(SyntaxNodeType::ExpressionList)
    , expressions(std::move(expressions))
{
}

pSyntaxNode ExpressionList::normalize()
{
    SyntaxNodes normalized;
    for (auto const &expr : expressions) {
        normalized.emplace_back(expr->normalize());
    }
    return make_node<ExpressionList>(location, normalized);
}

pBoundNode ExpressionList::bind()
{
    return nullptr;
}

void ExpressionList::dump_node(int indent)
{
    for (auto const &stmt : expressions) {
        stmt->dump(indent + 4);
    }
}

Identifier::Identifier(std::wstring_view identifier)
    : SyntaxNode(SyntaxNodeType::Identifier)
    , identifier(identifier)
{
}

pBoundNode Identifier::bind()
{
    return nullptr;
}

void Identifier::header()
{
    std::wcout << identifier;
}

IfStatement::IfStatement(pSyntaxNode condition, pSyntaxNode if_branch, pSyntaxNode else_branch)
    : SyntaxNode(SyntaxNodeType::IfStatement)
    , condition(condition)
    , if_branch(if_branch)
    , else_branch(else_branch)
{
    assert(condition != nullptr && if_branch != nullptr);
}

pSyntaxNode IfStatement::normalize()
{
    return make_node<IfStatement>(
        location,
        condition->normalize(),
        if_branch->normalize(),
        (else_branch != nullptr) ? else_branch->normalize() : nullptr);
}

pBoundNode IfStatement::bind()
{
    return nullptr;
}

void IfStatement::dump_node(int indent)
{
    condition->dump(indent + 4);
    if_branch->dump(indent + 4);
    if (else_branch != nullptr) {
        else_branch->dump(indent + 4);
    }
}

LoopStatement::LoopStatement(Label label, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::LoopStatement)
    , label(std::move(label))
    , statement(statement)
{
    assert(statement != nullptr);
}

pSyntaxNode LoopStatement::normalize()
{
    return make_node<LoopStatement>(
        location,
        label,
        statement->normalize());
}

pBoundNode LoopStatement::bind()
{
    return nullptr;
}

void LoopStatement::dump_node(int indent)
{
    statement->dump(indent + 4);
}

void LoopStatement::header()
{
    if (label) {
        std::wcout << *label;
    }
}

Module::Module(std::string_view name, std::wstring_view source, SyntaxNodes statements)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(name)
    , source(source)
{
    switch (statements.size()) {
    case 0:
        this->statements = make_node<Dummy>({0,0,0,0});
        break;
    case 1:
        this->statements = statements[0];
    default: {
        TokenLocation loc = statements.front()->location + statements.back()->location;
        this->statements = make_node<Block>(loc, std::move(statements));
    }
    }
}

Module::Module(std::string_view name, std::wstring_view source, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(name)
    , source(source)
    , statements(statement)
{
}

pSyntaxNode Module::normalize()
{
    return make_node<Module>(location, name, source, statements->normalize());
}

pBoundNode Module::bind()
{
    return nullptr;
}

void Module::header()
{
    std::cout << name;
}

void Module::dump_node(int indent)
{
    statements->dump(indent + 4);
}

QuotedString::QuotedString(std::wstring_view str, QuoteType type)
    : SyntaxNode(SyntaxNodeType::QuotedString)
    , string(str)
    , quote_type(type)
{
}

pBoundNode QuotedString::bind()
{
    return nullptr;
}

void QuotedString::header()
{
    std::wcout << string;
}

pSyntaxNode QuotedString::normalize()
{
    switch (quote_type) {
    case QuoteType::DoubleQuote:
        return make_node<DoubleQuotedString>(location, string, true);
    case QuoteType::SingleQuote:
        return make_node<SingleQuotedString>(location, string, true);
    default:
        UNREACHABLE();
    }
}

SingleQuotedString::SingleQuotedString(std::wstring_view string, bool strip_quotes)
    : ConstantExpression(SyntaxNodeType::SingleQuotedString)
    , string(strip_quotes ? string.substr(0, string.length() - 1).substr(1) : string)
{
}

pBoundNode SingleQuotedString::bind()
{
    return nullptr;
}

void SingleQuotedString::header()
{
    std::wcout << string;
}

UnaryExpression::UnaryExpression(Operator op, pSyntaxNode operand)
    : SyntaxNode(SyntaxNodeType::UnaryExpression)
    , op(op)
    , operand(operand)
{
}

pSyntaxNode UnaryExpression::normalize()
{
    return make_node<UnaryExpression>(location, op, operand->normalize());
}

pBoundNode UnaryExpression::bind()
{
    return nullptr;
}

void UnaryExpression::header()
{
    std::cout << Operator_name(op);
}

void UnaryExpression::dump_node(int indent)
{
    operand->dump_node(indent + 4);
}

VariableDeclaration::VariableDeclaration(std::wstring name, std::optional<std::wstring> type_name, pSyntaxNode initializer)
    : SyntaxNode(SyntaxNodeType::VariableDeclaration)
    , name(std::move(name))
    , type_name(type_name)
    , initializer(std::move(initializer))
{
}

pBoundNode VariableDeclaration::bind()
{
    return nullptr;
}

pSyntaxNode VariableDeclaration::normalize()
{
    return make_node<VariableDeclaration>(
        location,
        name,
        type_name,
        initializer->normalize());
}

void VariableDeclaration::dump_node(int indent)
{
    if (initializer != nullptr) {
        initializer->dump(indent + 4);
    }
}

void VariableDeclaration::header()
{
    if (type_name) {
        std::wcout << *type_name << ' ';
    }
    std::wcout << name;
}

Void::Void()
    : ConstantExpression(SyntaxNodeType::Void)
{
}

pBoundNode Void::bind()
{
    return nullptr;
}

WhileStatement::WhileStatement(Label label, pSyntaxNode condition, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::WhileStatement)
    , label(std::move(label))
    , condition(condition)
    , statement(statement)
{
    assert(condition != nullptr && statement != nullptr);
}

pSyntaxNode WhileStatement::normalize()
{
    return make_node<WhileStatement>(
        location,
        label,
        condition->normalize(),
        statement->normalize());
}

pBoundNode WhileStatement::bind()
{
    return nullptr;
}

void WhileStatement::dump_node(int indent)
{
    condition->dump(indent + 4);
    statement->dump(indent + 4);
}

void WhileStatement::header()
{
    if (label) {
        std::wcout << *label;
    }
}

Yield::Yield(Label label, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::Yield)
    , label(std::move(label))
    , statement(std::move(statement))
{
}

pSyntaxNode Yield::normalize()
{
    return make_node<Yield>(
        location,
        label,
        statement->normalize());
}

pBoundNode Yield::bind()
{
    return nullptr;
}

void Yield::dump_node(int indent)
{
    statement->dump(indent + 4);
}

void Yield::header()
{
    if (label) {
        std::wcout << *label;
    }
}

}
