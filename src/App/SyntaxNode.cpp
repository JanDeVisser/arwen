/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
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
        print_indent(indent);
        std::cout << "{" << std::endl;
        if (ns->parent != nullptr) {
            print_indent(indent + 4);
            std::wcout << "parent: ";
            if (ns->parent->node != nullptr) {
                ns->parent->node->header_line(std::wcout) << std::endl;
            } else {
                std::cout << "[null]\n";
            }
        }
        for (auto const &[n, t] : ns->types) {
            print_indent(indent + 4);
            std::wcout << n << ": " << t->to_string() << "\n";
        }
        for (auto const &[n, f] : ns->functions) {
            print_indent(indent + 4);
            std::wcout << n << ": " << f->bound_type->to_string() << "\n";
        }
        for (auto const &[n, v] : ns->variables) {
            print_indent(indent + 4);
            std::wcout << n << ": " << v->bound_type->to_string() << "\n";
        }
        print_indent(indent);
        std::cout << "}" << std::endl;
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
        stamp_node(stmt, parser));
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

pSyntaxNode normalize_node_(pSyntaxNode const &node, Parser &parser)
{
    assert(node != nullptr);
    if (node->status != SyntaxNode::Status::Initialized) {
        return node;
    }
    if (node->ns != nullptr) {
        parser.push_namespace(node->ns);
    }
    auto ret = node->normalize(parser);
    if (node->ns != nullptr) {
        parser.pop_namespace();
    }
    if (ret != nullptr && ret->status == SyntaxNode::Status::Initialized) {
        ret->status = SyntaxNode::Status::Normalized;
    }
    return ret;
}

std::vector<pSyntaxNode> normalize_nodes_(std::vector<pSyntaxNode> const &nodes, Parser &parser)
{
    std::vector<pSyntaxNode> normalized;
    for (auto const &n : nodes) {
        normalized.emplace_back(normalize_node(n, parser));
    }
    return normalized;
}

pSyntaxNode stamp_node_(pSyntaxNode const &node, Parser &parser)
{
    pSyntaxNode ret { nullptr };
    if (node != nullptr) {
        if (node->ns != nullptr) {
            parser.push_namespace(node->ns);
        }
        ret = node->stamp(parser);
        if (node->ns != nullptr) {
            parser.pop_namespace();
        }
    }
    return ret;
}

std::vector<pSyntaxNode> stamp_nodes_(std::vector<pSyntaxNode> const &nodes, Parser &parser)
{
    std::vector<pSyntaxNode> normalized;
    for (auto const &n : nodes) {
        normalized.emplace_back(stamp_node(n, parser));
    }
    return normalized;
}

pType bind_node(pSyntaxNode const &node, Parser &parser)
{
    assert(node != nullptr && node->status >= SyntaxNode::Status::Normalized);
    if (node->status == SyntaxNode::Status::Bound) {
        return node->bound_type;
    }
    if (node->ns != nullptr) {
        parser.push_namespace(node->ns);
    }
    auto ret = node->bind(parser);
    if (node->ns != nullptr) {
        parser.pop_namespace();
    }
    if (ret == nullptr) {
        node->dump();
    }
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
