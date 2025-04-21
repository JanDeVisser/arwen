/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>
#include <string_view>
#include <unistd.h>

#include <Util/Defer.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/StringUtil.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

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
    if (bound_type != nullptr) {
        std::wcout << " -> " << bound_type->name;
    }
    std::cout << std::endl;
    dump_node(indent);
}

void SyntaxNode::header()
{
}

void SyntaxNode::dump_node(int indent)
{
}

pSyntaxNode SyntaxNode::normalize(Parser &parser)
{
    return this->shared_from_this();
}

pSyntaxNode SyntaxNode::coerce(pType const &target, Parser &)
{
    assert(bound_type != nullptr);
    if (target == bound_type) {
        return shared_from_this();
    }
    return nullptr;
}

ConstantExpression::ConstantExpression(SyntaxNodeType type)
    : SyntaxNode(type)
{
}

pSyntaxNode ConstantExpression::evaluate(Operator op, pConstantExpression const &rhs)
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

#undef S
#undef S
#define S(O)                                                                     \
    pSyntaxNode ConstantExpression::evaluate_##O(pConstantExpression const &rhs) \
    {                                                                            \
        if (rhs != nullptr) {                                                    \
            return make_node<BinaryExpression>(                                  \
                this->location + rhs->location,                                  \
                this->shared_from_this(), Operator::O, rhs);                     \
        }                                                                        \
        return make_node<UnaryExpression>(                                       \
            this->location + rhs->location,                                      \
            Operator::O, this->shared_from_this());                              \
    }
Operators(S)
#undef S

    Block::Block(SyntaxNodes statements)
    : SyntaxNode(SyntaxNodeType::Block)
    , statements(std::move(statements))
{
}

pSyntaxNode Block::normalize(Parser &parser)
{
    SyntaxNodes normalized;
    for (auto const &stmt : statements) {
        normalized.emplace_back(stmt->normalize(parser));
    }
    return make_node<Block>(location, normalized);
}

pType Block::bind(Parser &parser)
{
    parser.push_scope(shared_from_this());
    Defer pop_scope { [&parser]() { parser.pop_scope(); } };
    pType type = TypeRegistry::void_;
    for (auto &statement : statements) {
        type = bind_node(statement, parser);
    }
    return type;
}

void Block::dump_node(int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

DeferStatement::DeferStatement(pSyntaxNode stmt)
    : SyntaxNode(SyntaxNodeType::DeferStatement)
    , stmt(std::move(stmt))
{
}

pSyntaxNode DeferStatement::normalize(Parser &parser)
{
    return make_node<DeferStatement>(
        location,
        stmt->normalize(parser));
}

pType DeferStatement::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

void DeferStatement::dump_node(int indent)
{
    stmt->dump(indent + 4);
}

Dummy::Dummy()
    : SyntaxNode(SyntaxNodeType::Dummy)
{
}

pType Dummy::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

Module::Module(std::wstring name, std::wstring source, SyntaxNodes statements)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(std::move(name))
    , source(std::move(source))
{
    switch (statements.size()) {
    case 0:
        this->statements = make_node<Dummy>({ 0, 0, 0, 0 });
        break;
    case 1:
        this->statements = statements[0];
    default: {
        TokenLocation loc = statements.front()->location + statements.back()->location;
        this->statements = make_node<Block>(loc, std::move(statements));
    }
    }
}

Module::Module(std::wstring name, std::wstring source, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(std::move(name))
    , source(std::move(source))
    , statements(statement)
{
}

pSyntaxNode Module::normalize(Parser &parser)
{
    return make_node<Module>(location, name, std::move(source), statements->normalize(parser));
}

pType Module::bind(Parser &parser)
{
    bind_node(statements, parser);
    return make_type(as_wstring(name), NamespaceType {});
}

void Module::header()
{
    std::wcout << name;
}

void Module::dump_node(int indent)
{
    statements->dump(indent + 4);
}

pType bind_node(pSyntaxNode node, Parser &parser)
{
    assert(node != nullptr);
    if (node->bound_type && node->bound_type != TypeRegistry::undetermined) {
        return node->bound_type;
    }
    auto ret = node->bind(parser);
    node->bound_type = ret;
    return ret;
}

}
