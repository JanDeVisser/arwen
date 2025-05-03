/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>
#include <ostream>
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

SyntaxNode::SyntaxNode(SyntaxNodeType type, pNamespace ns)
    : type(type)
    , ns(ns)
{
}

std::wostream &SyntaxNode::header_line(std::wostream &os)
{
    os << SyntaxNodeType_name(type) << " (" << location.index << ".." << location.index + location.length << ") ";
    header(std::wcout);
    if (bound_type != nullptr) {
        os << " -> " << bound_type->name;
    }
    return os;
}

void SyntaxNode::dump(int indent)
{
    print_indent(indent);
    header_line(std::wcout);
    std::cout << std::endl;
    dump_node(indent);
}

std::wostream &SyntaxNode::header(std::wostream &os)
{
    return os;
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
    auto stmt_type = bind_node(stmt, parser);
    if (stmt_type == TypeRegistry::undetermined) {
        return stmt_type;
    }
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

pType bind_node(pSyntaxNode node, Parser &parser)
{
    assert(node != nullptr);
    if (node->bound_type && node->bound_type != TypeRegistry::undetermined) {
        return node->bound_type;
    }
    auto ret = node->bind(parser);
    if ((parser.pass > 0) && (ret == TypeRegistry::undetermined)) {
        parser.append(node->location, "Cannot determine type");
    }
    node->bound_type = ret;
    return ret;
}

}

std::wostream &operator<<(std::wostream &os, Arwen::SyntaxNode const &node)
{
    os << SyntaxNodeType_name(node.type) << " (" << node.location.index << ".." << node.location.index + node.location.length << ") ";
    return os;
}
