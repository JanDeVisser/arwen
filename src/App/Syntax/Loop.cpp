/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Type.h"
#include <memory>

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>

namespace Arwen {

ForStatement::ForStatement(std::wstring var, pSyntaxNode expr, pSyntaxNode statement, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::ForStatement, std::move(ns))
    , range_variable(std::move(var))
    , range_expr(std::move(expr))
    , statement(statement)
{
    assert(this->range_expr != nullptr);
    assert(this->statement != nullptr);
}

pSyntaxNode ForStatement::normalize(Parser &parser)
{
    return make_node<ForStatement>(
        location,
        range_variable,
        normalize_node(range_expr, parser),
        normalize_node(statement, parser),
        ns);
}

pSyntaxNode ForStatement::stamp(Parser &parser)
{
    auto  new_ns = parser.push_new_namespace();
    Defer pop_ns { [&parser]() { parser.pop_namespace(); } };
    return make_node<ForStatement>(
        location,
        range_variable,
        stamp_node(range_expr, parser),
        stamp_node(statement, parser),
        new_ns);
}

pType ForStatement::bind(Parser &parser)
{
    auto range_type = bind_node(range_expr, parser);
    if (!range_type->is<RangeType>()) {
        return parser.bind_error(
            location,
            L"`for` loop range expression is a `{}`, not a range",
            range_type->name);
    }
    parser.push_namespace(ns);
    Defer pop_namespace { [&parser]() { parser.pop_namespace(); } };
    parser.register_variable(range_variable, range_expr);
    return bind_node(statement, parser);
}

void ForStatement::dump_node(int indent)
{
    range_expr->dump(indent + 4);
    statement->dump(indent + 4);
}

std::wostream &ForStatement::header(std::wostream &os)
{
    return os << range_variable;
}

LoopStatement::LoopStatement(Label label, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::LoopStatement)
    , label(std::move(label))
    , statement(std::move(statement))
{
    assert(this->statement != nullptr);
}

pSyntaxNode LoopStatement::normalize(Parser &parser)
{
    return make_node<LoopStatement>(
        location,
        label,
        normalize_node(statement, parser));
}

pSyntaxNode LoopStatement::stamp(Parser &parser)
{
    return make_node<LoopStatement>(
        location,
        label,
        stamp_node(statement, parser));
}

pType LoopStatement::bind(Parser &parser)
{
    return bind_node(statement, parser);
}

void LoopStatement::dump_node(int indent)
{
    statement->dump(indent + 4);
}

std::wostream &LoopStatement::header(std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

WhileStatement::WhileStatement(Label label, pSyntaxNode condition, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::WhileStatement)
    , label(std::move(label))
    , condition(std::move(condition))
    , statement(std::move(statement))
{
    assert(this->condition != nullptr && this->statement != nullptr);
}

pSyntaxNode WhileStatement::normalize(Parser &parser)
{
    return make_node<WhileStatement>(
        location,
        label,
        normalize_node(condition, parser),
        normalize_node(statement, parser));
}

pSyntaxNode WhileStatement::stamp(Parser &parser)
{
    return make_node<WhileStatement>(
        location,
        label,
        stamp_node(condition, parser),
        stamp_node(statement, parser));
}

pType WhileStatement::bind(Parser &parser)
{
    if (auto cond_type = bind_node(condition, parser);
        cond_type->is<Undetermined>() || cond_type->is<BindErrors>()) {
        return cond_type;
    } else if (!cond_type->is<BoolType>()) {
        return parser.bind_error(
            location,
            L"`while` loop condition is a `{}`, not a boolean",
            cond_type->name);
    }
    return bind_node(statement, parser);
}

void WhileStatement::dump_node(int indent)
{
    condition->dump(indent + 4);
    statement->dump(indent + 4);
}

std::wostream &WhileStatement::header(std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

}
