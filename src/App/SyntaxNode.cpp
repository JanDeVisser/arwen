/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <ostream>
#include <ranges>
#include <string>
#include <unistd.h>
#include <variant>

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
    header(os);
    return os;
}

std::wostream &ASTNodeImpl::header(std::wostream &os)
{
    os << id.id.value() << " " << SyntaxNodeType_name(type()) << " (" << location.index << ".." << location.index + location.length << ") ";
    std::visit([this](auto &n) { n.header(id, std::wcerr); }, node);
    if (bound_type != nullptr) {
        os << " -> " << bound_type->to_string();
    }
    os << " ";
    switch (status) {
    case Status::Initialized:
        std::wcerr << L"Initialized";
        break;
    case Status::Normalized:
        std::wcerr << L"Normalized";
        break;
    case Status::Bound:
        std::wcerr << L"Bound";
        break;
    case Status::Undetermined:
        std::wcerr << L"Undetermined";
        break;
    case Status::Ambiguous:
        std::wcerr << L"Ambiguous";
        break;
    case Status::BindErrors:
        std::wcerr << L"BindErrors";
        break;
    }
    return os;
}

void ASTNodeImpl::dump(int indent)
{
    print_indent(indent);
    header_line(std::wcerr);
    std::cerr << std::endl;
    if (ns.has_value()) {
        print_indent(indent);
        std::cerr << "{" << std::endl;
        if (ns->parent != nullptr) {
            print_indent(indent + 4);
            std::wcerr << "parent: ";
            if (ns->parent != nullptr) {
                ns->parent->header_line(std::wcerr) << std::endl;
            } else {
                std::cerr << "[null]\n";
            }
        }
        for (auto const &[n, t] : ns->types) {
            print_indent(indent + 4);
            std::wcerr << n << ": " << t->to_string() << "\n";
        }
        for (auto const &[n, f] : ns->functions) {
            print_indent(indent + 4);
            std::wcerr << n << ": " << f->bound_type->to_string() << "\n";
        }
        for (auto const &[n, v] : ns->variables) {
            print_indent(indent + 4);
            std::wcerr << n << ": " << v->bound_type->to_string() << "\n";
        }
        print_indent(indent);
        std::cerr << "}" << std::endl;
    }
    std::visit([this, indent](auto &n) { return n.dump_node(id, indent); }, node);
}

ASTNode ASTNodeImpl::clone()
{
    return std::visit([this](auto n) -> ASTNode {
        return make_node<decltype(n)>(id, n);
    },
        node);
}

ASTNode ASTNodeImpl::normalize()
{
    // std::wcerr << L"[->N] ";
    // header(std::wcerr);
    // std::wcerr << "\n";
    if (status >= Status::Normalized) {
        return id;
    }
    if (ns.has_value()) {
        id.repo->push_namespace(id);
    }
    auto ret = std::visit([this](auto n) {
        ASTNode ret = n.normalize(id);
        if (ret && ret.id == id.id) {
            ret->node.emplace<decltype(n)>(n);
        }
        return ret;
    },
        node);
    if (id->ns.has_value()) {
        id.repo->pop_namespace();
    }
    if (ret != nullptr && ret->status < ASTNodeImpl::Status::Normalized) {
        ret->status = Status::Normalized;
    }
    // std::wcerr << L"[N->] ";
    // ret->header(std::wcerr);
    // std::wcerr << "\n";
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
    // std::wcerr << L"[B] ";
    // header(std::wcerr);
    // std::wcerr << "\n";
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
    // std::wcerr << L"[B->] ";
    // header(std::wcerr);
    // std::wcerr << "\n";
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
    stmt = stmt->normalize();
    return n;
}

ASTNode DeferStatement::stamp(ASTNode const &n)
{
    stmt = stmt->stamp();
    return n;
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

void normalize_nodes(ASTNodes &nodes)
{
    std::vector<size_t> deleted;
    for (size_t ix = 0; ix < nodes.size(); ++ix) {
        auto normalized = nodes[ix]->normalize();
        if (normalized == nullptr) {
            deleted.emplace_back(ix);
            continue;
        }
        nodes[ix] = normalized;
    }
    for (auto ix : deleted | std::ranges::views::reverse) {
        nodes.erase(nodes.begin() + ix);
    }
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
