/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

IfStatement::IfStatement(ASTNode condition, ASTNode if_branch, ASTNode else_branch)
    : condition(condition)
    , if_branch(if_branch)
    , else_branch(else_branch)
{
    assert(condition != nullptr && if_branch != nullptr);
}

ASTNode IfStatement::normalize(ASTNode const &n)
{
    condition = condition->normalize();
    if_branch = if_branch->normalize();
    if (else_branch) {
        else_branch = else_branch->normalize();
    }
    return n;
}

ASTNode IfStatement::stamp(ASTNode const &n)
{
    condition = condition->stamp();
    if_branch = if_branch->stamp();
    if (else_branch) {
        else_branch = else_branch->stamp();
    }
    return n;
}

pType IfStatement::bind(ASTNode const &n)
{
    if (auto cond_type = condition->bind(); is<Undetermined>(cond_type) || is<BindErrors>(cond_type)) {
        return cond_type;
    } else if (!is<BoolType>(cond_type)) {
        return n.bind_error(
            L"`while` loop condition is a `{}`, not a boolean",
            cond_type->name);
    }
    auto if_type = if_branch->bind();
    auto else_type = (else_branch != nullptr) ? else_branch->bind() : nullptr;
    return (else_type == nullptr || else_type == if_type) ? if_type : TypeRegistry::the().ambiguous;
}

void IfStatement::dump_node(ASTNode const &n, int indent)
{
    condition->dump(indent + 4);
    if_branch->dump(indent + 4);
    if (else_branch != nullptr) {
        else_branch->dump(indent + 4);
    }
}

}
