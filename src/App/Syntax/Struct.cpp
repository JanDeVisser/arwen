/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>

#include <App/SyntaxNode.h>

namespace Arwen {

StructMember::StructMember(std::wstring label, pTypeSpecification type)
    : SyntaxNode(SyntaxNodeType::StructMember)
    , label(std::move(label))
    , type(std::move(type))
{
    assert(this->type != nullptr);
}

pSyntaxNode StructMember::normalize(Parser &parser)
{
    return make_node<StructMember>(
        location,
        label,
        normalize_node(type, parser));
}

pBoundNode StructMember::bind()
{
    return nullptr;
}

void StructMember::header()
{
    std::wcout << label << ' ' << type->to_string();
}

Struct::Struct(std::wstring name, StructMembers members)
    : SyntaxNode(SyntaxNodeType::Struct)
    , name(std::move(name))
    , members(std::move(members))
{
}

pSyntaxNode Struct::normalize(Parser &parser)
{
    return make_node<Struct>(
        location,
        name,
        normalize_nodes(members, parser));
}

pBoundNode Struct::bind()
{
    return nullptr;
}

void Struct::dump_node(int indent)
{
    for (auto const &m : members) {
        m->dump(indent + 4);
    }
}

void Struct::header()
{
    std::wcout << name;
}

}
