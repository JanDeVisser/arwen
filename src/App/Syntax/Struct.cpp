/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

StructMember::StructMember(std::wstring label, pTypeSpecification type)
    : SyntaxNode(SyntaxNodeType::StructMember)
    , label(std::move(label))
    , member_type(std::move(type))
{
    assert(this->member_type != nullptr);
}

pSyntaxNode StructMember::normalize(Parser &parser)
{
    return make_node<StructMember>(
        location,
        label,
        normalize_node(member_type, parser));
}

pSyntaxNode StructMember::stamp(Parser &parser)
{
    return make_node<StructMember>(
        location,
        label,
        stamp_node(member_type, parser));
}

pType StructMember::bind(Parser &parser)
{
    return bind_node(member_type, parser);
}

std::wostream &StructMember::header(std::wostream &os)
{
    return os << label << ' ' << member_type->to_string();
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

pSyntaxNode Struct::stamp(Parser &parser)
{
    return make_node<Struct>(
        location,
        name,
        stamp_nodes(members, parser));
}

pType Struct::bind(Parser &parser)
{
    StructType::Fields fields;
    for (auto const &m : members) {
        bind_node(m, parser);
        if (m->status != SyntaxNode::Status::Bound) {
            return m->bound_type;
        }
        fields.emplace_back(m->label, m->bound_type);
    }
    auto ret = TypeRegistry::the().struct_of(fields);
    parser.register_type(name, ret);
    return ret;
}

void Struct::dump_node(int indent)
{
    for (auto const &m : members) {
        m->dump(indent + 4);
    }
}

std::wostream &Struct::header(std::wostream &os)
{
    return os << name;
}

}
