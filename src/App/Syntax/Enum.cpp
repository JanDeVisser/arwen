/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Util/Logging.h"
#include <iostream>
#include <memory>

#include <App/SyntaxNode.h>
#include <App/Parser.h>

namespace Arwen {

EnumValue::EnumValue(std::wstring label, pConstantExpression value, pTypeSpecification payload)
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

pType EnumValue::bind(Parser &parser)
{
    UNREACHABLE();
}

void EnumValue::header()
{
    std::wcout << label;
    if (value != nullptr) {
        std::wcout << " = ";
        value->header();
    }
    if (payload != nullptr) {
        std::wcout << " ("
                   << payload->to_string()
                   << ")";
    }
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

pType Enum::bind(Parser &parser)
{
    EnumType enoom;
    size_t   ix = 0;
    for (auto const &v : values) {
        pType payload { nullptr };
        if (v->payload != nullptr) {
            payload = v->payload->resolve();
            if (payload == nullptr) {
                return parser.bind_error(v->payload->location, L"Could not resolve type `{}`", v->payload->to_string());
            }
        }
        size_t value = (v->value != nullptr) ? std::dynamic_pointer_cast<Integer>(v->value)->value : ix;
        enoom.values.emplace_back(v->label, value, payload);
        ++ix;
    }
    enoom.underlying_type = nullptr;
    if (underlying_type != nullptr) {
        enoom.underlying_type = underlying_type->resolve();
        if (enoom.underlying_type == nullptr) {
            return parser.bind_error(underlying_type->location, L"Could not resolve type `{}`", underlying_type->to_string());
        }
    }
    return make_type(name, enoom);
}

void Enum::dump_node(int indent)
{
    for (auto const &v : values) {
        v->dump(indent + 4);
    }
}

void Enum::header()
{
    std::wcout << name;
    if (underlying_type != nullptr) {
        std::wcout << ": " << underlying_type->to_string();
    }
}

}
