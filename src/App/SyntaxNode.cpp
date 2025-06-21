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
        os << " -> " << bound_type->to_string();
    }
    return os;
}

void SyntaxNode::dump(int indent)
{
    print_indent(indent);
    header_line(std::wcout);
    std::cout << std::endl;
    if (ns != nullptr) {
        for (auto const &[n, t] : ns->types) {
            print_indent(indent + 4);
            std::wcout << n << ": " << t->to_string() << "\n";
        }
    }
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

pType SyntaxNode::bind(Parser &)
{
    fatal("Node of type `{}` should have been elided during normalization", SyntaxNodeType_name(type));
}

pSyntaxNode SyntaxNode::stamp(Parser &)
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

DeferStatement::DeferStatement(pSyntaxNode stmt)
    : SyntaxNode(SyntaxNodeType::DeferStatement)
    , stmt(std::move(stmt))
{
}

pSyntaxNode DeferStatement::normalize(Parser &parser)
{
    return make_node<DeferStatement>(
        location,
        normalize_node(stmt, parser));
}

pSyntaxNode DeferStatement::stamp(Parser &parser)
{
    return make_node<DeferStatement>(
        location,
        stmt->stamp(parser));
}

pType DeferStatement::bind(Parser &parser)
{
    if (auto const stmt_type = bind_node(stmt, parser); stmt_type == TypeRegistry::undetermined) {
        return stmt_type;
    }
    return TypeRegistry::void_;
}

void DeferStatement::dump_node(int const indent)
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

pType bind_node(pSyntaxNode const &node, Parser &parser)
{
    assert(node != nullptr && node->status >= SyntaxNode::Status::Normalized);
    if (node->status == SyntaxNode::Status::Bound) {
        return node->bound_type;
    }
    auto ret = node->bind(parser);
    assert(ret != nullptr);
    if (ret == TypeRegistry::undetermined) {
        parser.unbound_nodes.push_back(node);
        parser.unbound++;
    } else if (ret->is<BindErrors>()) {
        node->status = SyntaxNode::Status::BindErrors;
    } else if (ret->is<Undetermined>()) {
        node->status = SyntaxNode::Status::Undetermined;
    } else if (ret->is<Ambiguous>()) {
        node->status = SyntaxNode::Status::Ambiguous;
    } else {
        node->status = SyntaxNode::Status::Bound;
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
