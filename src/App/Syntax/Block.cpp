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

Block::Block(SyntaxNodes statements, pNamespace const& ns)
    : SyntaxNode(SyntaxNodeType::Block, ns)
    , statements(statements)
{
    if (this->ns == nullptr) {
        assert(this->ns != nullptr);
    }
}

pSyntaxNode Block::normalize(Parser &parser)
{
    assert(ns != nullptr);
    SyntaxNodes normalized;
    for (auto const &stmt : statements) {
        normalized.emplace_back(stmt->normalize(parser));
    }
    return make_node<Block>(location, normalized, ns);
}

pType Block::bind(Parser &parser)
{
    assert(ns != nullptr);
    parser.push_namespace(ns);
    Defer pop_scope { [&parser]() { parser.pop_namespace(); } };
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

void Block::dump_node(int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
}

}
