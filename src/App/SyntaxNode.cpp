/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <functional>
#include <memory>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>

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
    std::cout << SyntaxNodeType_name(type) << " ";
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

BinaryExpression::BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs)
    : SyntaxNode(SyntaxNodeType::BinaryExpression)
    , lhs(lhs)
    , op(op)
    , rhs(rhs)
{
}

pSyntaxNode BinaryExpression::normalize()
{
    if (op != Operator::Sequence) {
        return make_node<BinaryExpression>(lhs->normalize(), op, rhs->normalize());
    }

    SyntaxNodes                      nodes;
    std::function<void(pSyntaxNode)> flatten;
    flatten = [&nodes, &flatten](pSyntaxNode n) {
        if (auto binex = std::dynamic_pointer_cast<BinaryExpression>(n); binex) {
            if (binex->op == Operator::Sequence) {
                nodes.push_back(binex->lhs);
                flatten(binex->rhs);
            } else {
                nodes.push_back(binex);
            }
        } else {
            nodes.push_back(n);
        }
    };
    flatten(shared_from_this());
    return make_node<ExpressionList>(nodes)->normalize();
}

pBoundNode BinaryExpression::bind()
{
    return nullptr;
}

void BinaryExpression::header()
{
    std::cout << Operator_name(op);
}

void BinaryExpression::dump_node(int indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
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
    return make_node<Block>(normalized);
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
    return make_node<ExpressionList>(normalized);
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

Module::Module(std::string_view name, SyntaxNodes statements)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(name)
{
    switch (statements.size()) {
    case 0:
        this->statements = make_node<Dummy>();
        break;
    case 1:
        this->statements = statements[0];
    default:
        this->statements = make_node<Block>(std::move(statements));
    }
}

Module::Module(std::string_view name, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(name)
    , statements(statement)
{
}

pSyntaxNode Module::normalize()
{
    return make_node<Module>(name, statements->normalize());
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

Number::Number(std::wstring_view number, NumberType type)
    : SyntaxNode(SyntaxNodeType::Number)
    , number(number)
    , number_type(type)
{
}

pBoundNode Number::bind()
{
    return nullptr;
}

void Number::header()
{
    std::wcout << number << L" ";
    std::cout << NumberType_name(number_type);
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

UnaryExpression::UnaryExpression(Operator op, pSyntaxNode operand)
    : SyntaxNode(SyntaxNodeType::UnaryExpression)
    , op(op)
    , operand(operand)
{
}

pSyntaxNode UnaryExpression::normalize()
{
    return make_node<UnaryExpression>(op,  operand->normalize());
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

}
