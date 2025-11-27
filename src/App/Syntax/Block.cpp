/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Block::Block(ASTNodes statements)
    : statements(std::move(statements))
{
}

ASTNode Block::normalize(ASTNode const &n)
{
    const_cast<ASTNode &>(n)->init_namespace();
    normalize_nodes(statements);
    return n;
}

ASTNode Block::stamp(ASTNode const &n)
{
    ASTNodes stamped;
    for (auto const &stmt : statements) {
        auto new_stmt = stmt->stamp();
        if (new_stmt == nullptr) {
            n.repo->append(n->location, "Stamping statement failed");
            return nullptr;
        }
        stamped.emplace_back(new_stmt);
    }
    statements = stamped;
    return n;
}

pType Block::bind(ASTNode const &n)
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

void Block::dump_node(ASTNode const &n, int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

}
