/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

IfStatement::IfStatement(pSyntaxNode condition, pSyntaxNode if_branch, pSyntaxNode else_branch)
    : SyntaxNode(SyntaxNodeType::IfStatement)
    , condition(condition)
    , if_branch(if_branch)
    , else_branch(else_branch)
{
    assert(condition != nullptr && if_branch != nullptr);
}

pSyntaxNode IfStatement::normalize(Parser &parser)
{
    return make_node<IfStatement>(
        location,
        condition->normalize(parser),
        if_branch->normalize(parser),
        (else_branch != nullptr) ? else_branch->normalize(parser) : nullptr);
}

pSyntaxNode IfStatement::stamp(Parser &parser)
{
    return make_node<IfStatement>(
        location,
        condition->stamp(parser),
        if_branch->stamp(parser),
        (else_branch != nullptr) ? else_branch->stamp(parser) : nullptr);
}

pType IfStatement::bind(Parser &parser)
{
    auto if_type = bind_node(if_branch, parser);
    auto else_type = (else_branch != nullptr) ? bind_node(else_branch, parser) : nullptr;
    return (else_type == nullptr || else_type == if_type) ? if_type : TypeRegistry::the().ambiguous;
}

void IfStatement::dump_node(int indent)
{
    condition->dump(indent + 4);
    if_branch->dump(indent + 4);
    if (else_branch != nullptr) {
        else_branch->dump(indent + 4);
    }
}

}
