/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>

#include <Util/Logging.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <ostream>

namespace Arwen {

EnumValue::EnumValue(std::wstring label, pSyntaxNode value, pTypeSpecification payload)
    : SyntaxNode(SyntaxNodeType::EnumValue)
    , label(std::move(label))
    , value(std::move(value))
    , payload(std::move(payload))
{
}

pSyntaxNode EnumValue::normalize(Parser &parser)
{
    return make_node<EnumValue>(
        location,
        label,
        normalize_node(value, parser),
        normalize_node(payload, parser));
}

pSyntaxNode EnumValue::stamp(Parser &parser)
{
    return make_node<EnumValue>(
        location,
        label,
        stamp_node(value, parser),
        stamp_node(payload, parser));
}

pType EnumValue::bind(Parser &parser)
{
    UNREACHABLE();
}

std::wostream &EnumValue::header(std::wostream &os)
{
    os << label;
    if (value != nullptr) {
        os << " = ";
        value->header(os);
    }
    if (payload != nullptr) {
        os << " ("
           << payload->to_string()
           << ")";
    }
    return os;
}

Enum::Enum(std::wstring name, pTypeSpecification underlying_type, EnumValues values)
    : SyntaxNode(SyntaxNodeType::Enum)
    , name(std::move(name))
    , underlying_type(std::move(underlying_type))
    , values(std::move(values))
{
}

pSyntaxNode Enum::normalize(Parser &parser)
{
    EnumValues vals {};
    for (auto const &v : values) {
        vals.emplace_back(std::dynamic_pointer_cast<EnumValue>(v->normalize(parser)));
    }
    return make_node<Enum>(
        location,
        name,
        normalize_node(underlying_type, parser),
        normalize_nodes(values, parser));
}

pSyntaxNode Enum::stamp(Parser &parser)
{
    return make_node<Enum>(
        location,
        name,
        stamp_node(underlying_type, parser),
        stamp_nodes(values, parser));
}

pType Enum::bind(Parser &parser)
{
    EnumType enoom;
    size_t   ix = 0;
    for (auto const &v : values) {
        pType payload { nullptr };
        if (v->payload != nullptr) {
            payload = v->payload->resolve(parser);
            if (payload == nullptr) {
                return parser.bind_error(v->payload->location, L"Could not resolve type `{}`", v->payload->to_string());
            }
        }
        size_t value = (v->value != nullptr) ? as<int64_t>(std::dynamic_pointer_cast<Constant>(v->value)->bound_value.value()) : ix;
        enoom.values.emplace_back(v->label, value, payload);
        ++ix;
    }
    enoom.underlying_type = nullptr;
    if (underlying_type != nullptr) {
        enoom.underlying_type = underlying_type->resolve(parser);
        if (enoom.underlying_type == nullptr) {
            return parser.bind_error(underlying_type->location, L"Could not resolve type `{}`", underlying_type->to_string());
        }
    }
    auto ret = make_type(name, enoom);
    parser.register_type(name, ret);
    return ret;
}

void Enum::dump_node(int indent)
{
    for (auto const &v : values) {
        v->dump(indent + 4);
    }
}

std::wostream &Enum::header(std::wostream &os)
{
    os << name;
    if (underlying_type != nullptr) {
        os << ": " << underlying_type->to_string();
    }
    return os;
}

}
