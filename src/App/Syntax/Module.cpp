/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Module::Module()
    : SyntaxNode(SyntaxNodeType::Module, nullptr)
    , name(L"")
    , source(L"")
    , statements(nullptr)
{
}

Module::Module(std::wstring const &n, std::wstring const &src, SyntaxNodes const &stmts, pNamespace const &name_space)
    : SyntaxNode(SyntaxNodeType::Module, name_space)
    , name(n)
    , source(src)
{
    switch (stmts.size()) {
    case 0:
        this->statements = make_node<Dummy>({ 0, 0, 0, 0 });
        break;
    case 1:
        this->statements = stmts[0];
    default: {
        TokenLocation loc = stmts.front()->location + stmts.back()->location;
        this->statements = make_node<Block>(
            loc,
            stmts,
            name_space);
    }
    }
}

Module::Module(std::wstring name, std::wstring source, pSyntaxNode statement, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::Module, ns)
    , name(std::move(name))
    , source(std::move(source))
    , statements(statement)
{
}

pSyntaxNode Module::normalize(Parser &parser)
{
    return make_node<Module>(
        location,
        name,
        source,
        normalize_node(statements, parser),
        ns);
}

pSyntaxNode Module::stamp(Parser &parser)
{
    auto  new_ns = parser.push_new_namespace();
    Defer pop_ns { [&parser]() { parser.pop_namespace(); } };
    return make_node<Module>(
        location,
        name,
        source,
        stamp_node(statements, parser),
        new_ns);
}

pType Module::bind(Parser &parser)
{
    parser.push_namespace(ns);
    Defer pop_ns { [&parser]() { parser.pop_namespace(); } };
    auto  stmt_type = bind_node(statements, parser);
    if (stmt_type == TypeRegistry::undetermined) {
        return stmt_type;
    }
    auto ret = make_type(as_wstring(name), NamespaceType {});
    return ret;
}

std::wostream &Module::header(std::wostream &os)
{
    return os << name;
}

void Module::dump_node(int indent)
{
    statements->dump(indent + 4);
}

}
