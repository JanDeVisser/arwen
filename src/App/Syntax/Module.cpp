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
    : SyntaxNode(SyntaxNodeType::Module, ns)
    , name(std::move(name))
    , source(std::move(source))
    , statements(statements)
{
}

// Module::Module(std::wstring name, std::wstring source, pBlock statements)
//     : SyntaxNode(SyntaxNodeType::Module)
//     , name(std::move(name))
//     , source(std::move(source))
//     , statements(std::move(statements))
// {
// }

pSyntaxNode Module::normalize(Parser &parser)
{
    return make_node<Module>(
        location,
        name,
        source,
        normalize_nodes(statements, parser),
        ns);
}

pType Module::bind(Parser &parser)
{
    assert(ns != nullptr);
    pType type = TypeRegistry::void_;
    pType undetermined { nullptr };
    for (auto &statement : statements) {
        type = bind_node(statement, parser);
        if (type == TypeRegistry::undetermined) {
            undetermined = TypeRegistry::undetermined;
        }
    }
    if (undetermined != nullptr) {
        return undetermined;
    }
    return type;
}

std::wostream &Module::header(std::wostream &os)
{
    return os << name;
}

void Module::dump_node(int const indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

}
