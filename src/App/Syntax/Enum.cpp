/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <ostream>

#include <Util/Logging.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>

namespace Arwen {

EnumValue::EnumValue(std::wstring label, ASTNode value, ASTNode payload)
    : label(std::move(label))
    , value(std::move(value))
    , payload(std::move(payload))
{
}

ASTNode EnumValue::normalize(ASTNode const &n)
{
    value = value->normalize();
    if (payload != nullptr) {
        payload = payload->normalize();
    }
    return n;
}

ASTNode EnumValue::stamp(ASTNode const &n)
{
    value = value->stamp();
    if (payload != nullptr) {
        payload = payload->stamp();
    }
    return n;
}

pType EnumValue::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

std::wostream &EnumValue::header(ASTNode const &, std::wostream &os)
{
    os << label;
    if (value != nullptr) {
        os << " = ";
        value->header(value, os);
    }
    if (payload != nullptr) {
        os << " ("
           << get<TypeSpecification>(payload).to_string()
           << ")";
    }
    return os;
}

Enum::Enum(std::wstring name, ASTNode underlying_type, ASTNodes values)
    : name(std::move(name))
    , underlying_type(std::move(underlying_type))
    , values(std::move(values))
{
}

ASTNode Enum::normalize(ASTNode const &n)
{
    ASTNodes vals {};
    for (auto const &v : values) {
        vals.emplace_back(v->normalize());
    }
    values = vals;
    underlying_type = underlying_type->normalize();
    return n;
}

ASTNode Enum::stamp(ASTNode const &n)
{
    ASTNodes vals {};
    for (auto const &v : values) {
        vals.emplace_back(v->stamp());
    }
    values = vals;
    underlying_type = underlying_type->stamp();
    return n;
}

pType Enum::bind(ASTNode const &n)
{
    EnumType enoom;
    size_t   ix = 0;
    for (auto const &v : values) {
        pType payload { nullptr };
        auto  enum_value = get<EnumValue>(v);
        if (enum_value.payload != nullptr) {
            auto value_payload = get<TypeSpecification>(enum_value.payload);
            payload = value_payload.resolve(n);
            if (payload == nullptr) {
                return n.bind_error(L"Could not resolve type `{}`", value_payload.to_string());
            }
        }
        size_t value = (enum_value.value != nullptr) ? as<int64_t>(get<Constant>(enum_value.value).bound_value.value()) : ix;
        enoom.values.emplace_back(enum_value.label, value, payload);
        ++ix;
    }
    enoom.underlying_type = nullptr;
    if (underlying_type != nullptr) {
        auto underlying = get<TypeSpecification>(underlying_type);
        enoom.underlying_type = underlying.resolve(n);
        if (enoom.underlying_type == nullptr) {
            return n.bind_error(L"Could not resolve type `{}`", underlying.to_string());
        }
    }
    auto ret = make_type(name, enoom);
    n.repo->register_type(name, ret);
    return ret;
}

void Enum::dump_node(ASTNode const &n, int indent)
{
    for (auto const &v : values) {
        v->dump(indent + 4);
    }
}

std::wostream &Enum::header(ASTNode const &, std::wostream &os)
{
    os << name;
    if (underlying_type != nullptr) {
        os << ": " << get<TypeSpecification>(underlying_type).to_string();
    }
    return os;
}

}
