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

Module::Module(std::wstring name, std::wstring source)
    : name(std::move(name))
    , source(std::move(source))
{
}

Module::Module(std::wstring name, std::wstring source, ASTNodes const &statements)
    : name(std::move(name))
    , source(std::move(source))
    , statements(statements)
{
}

ASTNode Module::normalize(ASTNode const &n)
{
    n->init_namespace();
    statements = normalize_nodes(statements);
    return n;
}

pType Module::bind(ASTNode const &n)
{
    pType type = TypeRegistry::void_;
    pType undetermined { nullptr };
    for (auto &statement : statements) {
        type = statement->bind();
        if (type == TypeRegistry::undetermined) {
            undetermined = TypeRegistry::undetermined;
        }
    }
    if (undetermined != nullptr) {
        return undetermined;
    }
    return type;
}

std::wostream &Module::header(ASTNode const &n, std::wostream &os)
{
    return os << name;
}

void Module::dump_node(ASTNode const &n, int const indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

}
