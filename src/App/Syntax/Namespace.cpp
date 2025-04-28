/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Util/Utf8.h"
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Namespace::Namespace(pNamespace parent)
    : parent(std::move(parent))
{
}

pType Namespace::find_type(std::wstring const &name) const
{
    if (types.contains(name)) {
        return types.at(name);
    }
    if (parent != nullptr) {
        return parent->find_type(name);
    }
    return nullptr;
}

void Namespace::register_type(std::wstring name, pType type)
{
    assert(!types.contains(name));
    types[name] = std::move(type);
}

pSyntaxNode Namespace::find_name(std::wstring const &name) const
{
    if (names.contains(name)) {
        return names.at(name);
    }
    if (parent != nullptr) {
        return parent->find_name(name);
    }
    return nullptr;
}

pType Namespace::type_of(std::wstring const &name) const
{
    auto n = find_name(name);
    if (n == nullptr) {
        return nullptr;
    }
    if (n->bound_type == nullptr) {
        n->bound_type = TypeRegistry::the().undetermined;
    }
    return n->bound_type;
}

void Namespace::register_name(std::wstring name, pSyntaxNode node)
{
    assert(!names.contains(name));
    names[name] = std::move(node);
}

}
