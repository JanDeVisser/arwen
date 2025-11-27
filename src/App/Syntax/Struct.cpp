/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

StructMember::StructMember(std::wstring label, ASTNode type)
    : label(std::move(label))
    , member_type(std::move(type))
{
    assert(this->member_type != nullptr);
}

ASTNode StructMember::normalize(ASTNode const &n)
{
    member_type = member_type->normalize();
    return n;
}

ASTNode StructMember::stamp(ASTNode const &n)
{
    member_type = member_type->stamp();
    return n;
}

pType StructMember::bind(ASTNode const &n)
{
    return member_type->bind();
}

std::wostream &StructMember::header(ASTNode const &, std::wostream &os)
{
    return os << label << ' ' << get<TypeSpecification>(member_type).to_string();
}

Struct::Struct(std::wstring name, ASTNodes members)
    : name(std::move(name))
    , members(std::move(members))
{
}

ASTNode Struct::normalize(ASTNode const &n)
{
    normalize_nodes(members);
    return n;
}

ASTNode Struct::stamp(ASTNode const &n)
{
    members = stamp_nodes(members);
    return n;
}

pType Struct::bind(ASTNode const &n)
{
    StructType::Fields fields;
    for (auto const &m : members) {
        m->bind();
        if (m->status != ASTNodeImpl::Status::Bound) {
            return m->bound_type;
        }
        fields.emplace_back(get<StructMember>(m).label, m->bound_type);
    }
    n.repo->register_type(name, TypeRegistry::the().struct_of(fields));
    return TypeRegistry::void_;
}

void Struct::dump_node(ASTNode const &, int indent)
{
    for (auto const &m : members) {
        m->dump(indent + 4);
    }
}

std::wostream &Struct::header(ASTNode const &, std::wostream &os)
{
    return os << name;
}

}
