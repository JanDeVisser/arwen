/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <utility>

namespace Arwen {

Module::Module(std::wstring name, std::wstring source, SyntaxNodes const &statements, pNamespace const &ns)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(std::move(name))
    , source(std::move(source))
    , statements(make_node<Block>(location, statements, ns))
{
}

Module::Module(std::wstring name, std::wstring source, pBlock statements)
    : SyntaxNode(SyntaxNodeType::Module)
    , name(std::move(name))
    , source(std::move(source))
    , statements(std::move(statements))
{
}

pSyntaxNode Module::normalize(Parser &parser)
{
    return make_node<Module>(
        location,
        name,
        source,
        normalize_node(statements, parser));
}

pSyntaxNode Module::stamp(Parser &parser)
{
    auto  new_ns = parser.push_new_namespace();
    Defer pop_ns { [&parser] { parser.pop_namespace(); } };
    return make_node<Module>(
        location,
        name,
        source,
        stamp_node(statements, parser));
}

pType Module::bind(Parser &parser)
{
    return bind_node(statements, parser);
}

std::wostream &Module::header(std::wostream &os)
{
    return os << name;
}

void Module::dump_node(int const indent)
{
    statements->dump(indent + 4);
}

}
