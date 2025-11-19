/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <iostream>
#include <ostream>
#include <string>
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
#include <variant>

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

TokenLocation ASTNode::operator+(ASTNode const &other)
{
    assert(*this && other);
    assert(repo == other.repo);
    return (*this)->location + other->location;
}

TokenLocation ASTNode::operator+(TokenLocation const &other)
{
    assert(*this);
    return (*this)->location + other;
}

void ASTNode::error(std::wstring const &msg) const
{
    repo->append((*this)->location, msg);
}

void ASTNode::error(std::string const &msg) const
{
    repo->append((*this)->location, msg);
}

pType ASTNode::bind_error(std::wstring const &msg) const
{
    return repo->bind_error((*this)->location, msg);
}

pType AbstractSyntaxNode::bind(ASTNode const &n)
{
    fatal("Node of type `{}` should have been elided during normalization", SyntaxNodeType_name(n->type()));
}

ASTNode AbstractSyntaxNode::normalize(ASTNode const &n)
{
    return n;
}

ASTNode AbstractSyntaxNode::stamp(ASTNode const &n)
{
    return n;
}

std::wostream &AbstractSyntaxNode::header(ASTNode const &, std::wostream &os)
{
    return os;
}

void AbstractSyntaxNode::dump_node(ASTNode const &, int)
{
}

ASTNode AbstractSyntaxNode::coerce(ASTNode const &n, pType const &target)
{
    assert(n->bound_type);
    if (target == n->bound_type) {
        return n;
    }
    return {};
}

void ASTNodeImpl::init_namespace()
{
    ns = Namespace {};
    if (!id.repo->namespaces.empty()) {
        ns->parent = id.repo->namespaces.back();
    }
    id.repo->push_namespace(id);
}

std::wostream &ASTNodeImpl::header_line(std::wostream &os)
{
    os << SyntaxNodeType_name(type()) << " (" << location.index << ".." << location.index + location.length << ") ";
    std::visit([this](auto &n) { n.header(id, std::wcout); }, node);
    if (bound_type != nullptr) {
        os << " -> " << bound_type->to_string();
    }
    return os;
}

void ASTNodeImpl::dump(int indent)
{
    print_indent(indent);
    header_line(std::wcout);
    std::cout << std::endl;
    if (ns.has_value()) {
        print_indent(indent);
        std::cout << "{" << std::endl;
        if (ns->parent != nullptr) {
            print_indent(indent + 4);
            std::wcout << "parent: ";
            if (ns->parent != nullptr) {
                ns->parent->header_line(std::wcout) << std::endl;
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
    dump_node(id, indent);
}

ASTNode ASTNodeImpl::normalize()
{
    if (status != Status::Initialized) {
        return id;
    }
    if (ns.has_value()) {
        id.repo->push_namespace(id);
    }
    auto ret = std::visit([this](auto &n) { return n.normalize(id); }, node);
    if (ns.has_value()) {
        id.repo->pop_namespace();
    }
    if (ret != nullptr && ret->status == ASTNodeImpl::Status::Initialized) {
        ret->status = Status::Normalized;
    }
    return ret;
}

ASTNode ASTNodeImpl::stamp()
{
    if (ns.has_value()) {
        id.repo->push_namespace(id);
    }
    ASTNode ret = std::visit([this](auto &n) { return n.stamp(id); }, node);
    if (ns.has_value()) {
        id.repo->pop_namespace();
    }
    return ret;
}

pType ASTNodeImpl::bind()
{
    assert(status >= Status::Normalized);
    if (status == Status::Bound) {
        return bound_type;
    }
    if (ns.has_value()) {
        id.repo->push_namespace(id);
    }
    auto ret = std::visit([this](auto &n) { return n.bind(id); }, node);
    if (ns.has_value()) {
        id.repo->pop_namespace();
    }
    if (ret == nullptr) {
        dump();
    }
    assert(ret != nullptr);
    if (ret == TypeRegistry::undetermined) {
        id.repo->unbound_nodes.push_back(id);
        id.repo->unbound++;
    } else if (is<BindErrors>(ret)) {
        status = Status::BindErrors;
    } else if (is<Undetermined>(ret)) {
        status = Status::Undetermined;
    } else if (is<Ambiguous>(ret)) {
        status = Status::Ambiguous;
    } else {
        status = Status::Bound;
    }
    bound_type = ret;
    return ret;
}

ASTNode ASTNodeImpl::coerce(pType const &target)
{
    return std::visit([this, &target](auto &n) { return n.coerce(id, target); }, node);
}

DeferStatement::DeferStatement(ASTNode stmt)
    : stmt(std::move(stmt))
{
}

ASTNode DeferStatement::normalize(ASTNode const &n)
{
    return make_node<DeferStatement>(stmt, stmt->normalize());
}

ASTNode DeferStatement::stamp(ASTNode const &n)
{
    return make_node<DeferStatement>(n, stmt->stamp());
}

pType DeferStatement::bind(ASTNode const &n)
{
    if (auto const stmt_type = stmt->bind(); stmt_type == TypeRegistry::undetermined) {
        return stmt_type;
    }
    return TypeRegistry::void_;
}

void DeferStatement::dump_node(ASTNode const &n, int const indent)
{
    stmt->dump(indent + 4);
}

pType Dummy::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

ASTNodes normalize_nodes(ASTNodes const &nodes)
{
    ASTNodes normalized;
    for (auto const &n : nodes) {
        normalized.emplace_back(n->normalize());
    }
    return normalized;
}

ASTNodes stamp_nodes(ASTNodes const &nodes)
{
    ASTNodes stamped;
    for (auto const &n : nodes) {
        stamped.emplace_back(n->stamp());
    }
    return stamped;
}

}

std::wostream &operator<<(std::wostream &os, Arwen::ASTNode const &node)
{
    os << SyntaxNodeType_name(node->type()) << " (" << node->location.index << ".." << node->location.index + node->location.length << ") ";
    return os;
}
