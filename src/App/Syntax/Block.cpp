/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <cstddef>
#include <memory>

namespace Arwen {

Block::Block(SyntaxNodes statements, SyntaxNodes deferred, pNamespace const &ns)
    : SyntaxNode(SyntaxNodeType::Block, ns)
    , statements(std::move(statements))
    , deferred_statements(std::move(deferred))
{
    if (this->ns == nullptr) {
        assert(this->ns != nullptr);
    }
}

Block::Block(SyntaxNodes statements, pNamespace const &ns)
    : SyntaxNode(SyntaxNodeType::Block, ns)
    , statements(std::move(statements))
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
        auto new_stmt = stmt->normalize(parser);
        if (new_stmt == nullptr) {
            parser.append(location, "Folding statement failed");
            return nullptr;
        }
        if (new_stmt->ns != nullptr) {
            new_stmt->ns->parent = ns;
        }
        if (auto defer = std::dynamic_pointer_cast<DeferStatement>(new_stmt); defer != nullptr) {
            deferred_statements.push_back(defer->stmt);
        } else {
            normalized.emplace_back(new_stmt);
        }
    }
    return make_node<Block>(location, normalized, deferred_statements, ns);
}

pSyntaxNode Block::stamp(Parser &parser)
{
    assert(ns != nullptr);
    auto        new_ns = parser.push_new_namespace();
    Defer       pop_ns { [&parser]() { parser.pop_namespace(); } };
    SyntaxNodes normalized;
    for (auto const &stmt : statements) {
        auto new_stmt = stmt->stamp(parser);
        if (new_stmt == nullptr) {
            parser.append(location, "Stamping statement failed");
            return nullptr;
        }
        normalized.emplace_back(new_stmt);
    }
    SyntaxNodes deferred;
    for (auto const &stmt : deferred_statements) {
        auto new_stmt = stmt->stamp(parser);
        if (new_stmt == nullptr) {
            parser.append(location, "Stamping statement failed");
            return nullptr;
        }
        deferred.push_back(new_stmt);
    }
    return make_node<Block>(location, normalized, deferred, new_ns);
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
    for (auto &statement : deferred_statements) {
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
    if (!deferred_statements.empty()) {
        printf("%*.*sDeferred\n", indent, indent, "");
        for (auto const &stmt : deferred_statements) {
            stmt->dump(indent + 4);
        }
    }
}

}
