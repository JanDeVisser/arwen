/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include "App/Type.h"

namespace Arwen {

ForStatement::ForStatement(std::wstring var, ASTNode expr, ASTNode statement)
    : range_variable(std::move(var))
    , range_expr(std::move(expr))
    , statement(statement)
{
    assert(this->range_expr != nullptr);
    assert(this->statement != nullptr);
}

ASTNode ForStatement::normalize(ASTNode const &n)
{
    n->init_namespace();
    range_expr = range_expr->normalize();
    n->ns->register_variable(range_variable, range_expr);
    statement = statement->normalize();
    return n;
}

ASTNode ForStatement::stamp(ASTNode const &n)
{
    range_expr = range_expr->stamp();
    statement = statement->stamp();
    return n;
}

pType ForStatement::bind(ASTNode const &n)
{
    auto range_type = range_expr->bind();
    if (!is<RangeType>(range_type)) {
        return n.bind_error(L"`for` loop range expression is a `{}`, not a range", range_type->name);
    }
    return statement->bind();
}

void ForStatement::dump_node(ASTNode const &, int indent)
{
    range_expr->dump(indent + 4);
    statement->dump(indent + 4);
}

std::wostream &ForStatement::header(ASTNode const &, std::wostream &os)
{
    return os << range_variable;
}

LoopStatement::LoopStatement(Label label, ASTNode statement)
    : label(std::move(label))
    , statement(std::move(statement))
{
    assert(this->statement != nullptr);
}

ASTNode LoopStatement::normalize(ASTNode const &n)
{
    statement = statement->normalize();
    return n;
}

ASTNode LoopStatement::stamp(ASTNode const &n)
{
    statement = statement->stamp();
    return n;
}

pType LoopStatement::bind(ASTNode const &n)
{
    return statement->bind();
}

void LoopStatement::dump_node(ASTNode const &, int indent)
{
    statement->dump(indent + 4);
}

std::wostream &LoopStatement::header(ASTNode const &, std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

WhileStatement::WhileStatement(Label label, ASTNode condition, ASTNode statement)
    : label(std::move(label))
    , condition(std::move(condition))
    , statement(std::move(statement))
{
    assert(this->condition != nullptr && this->statement != nullptr);
}

ASTNode WhileStatement::normalize(ASTNode const &n)
{
    condition = condition->normalize();
    statement = statement->normalize();
    return n;
}

ASTNode WhileStatement::stamp(ASTNode const &n)
{
    condition = condition->stamp();
    statement = statement->stamp();
    return n;
}

pType WhileStatement::bind(ASTNode const &n)
{
    if (auto cond_type = condition->bind(); is<Undetermined>(cond_type) || is<BindErrors>(cond_type)) {
        return cond_type;
    } else if (!is<BoolType>(cond_type)) {
        return n.bind_error(L"`while` loop condition is a `{}`, not a boolean", cond_type->name);
    }
    return statement->bind();
}

void WhileStatement::dump_node(ASTNode const &, int indent)
{
    condition->dump(indent + 4);
    statement->dump(indent + 4);
}

std::wostream &WhileStatement::header(ASTNode const &, std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

}
