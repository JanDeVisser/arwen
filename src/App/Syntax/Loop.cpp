/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>

namespace Arwen {

ForStatement::ForStatement(std::wstring var, pSyntaxNode expr, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::ForStatement)
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
        range_expr->normalize(parser),
        statement->normalize(parser));
}

pType ForStatement::bind(Parser &parser)
{
    auto range_type = bind_node(range_expr, parser);
    if (!range_type->is<RangeType>()) {
        return parser.bind_error(
            location,
            L"`for` loop range expression is a `{}`, not a range",
            range_type->name
        );
    }
    parser.push_scope(shared_from_this());
    parser.register_name(range_variable, std::get<RangeType>(range_type->description).range_of);
    auto block_type = bind_node(statement, parser);
    return block_type;
}

void ForStatement::dump_node(int indent)
{
    range_expr->dump(indent + 4);
    statement->dump(indent + 4);
}

void ForStatement::header()
{
    std::wcout << range_variable;
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
        statement->normalize(parser));
}

pType LoopStatement::bind(Parser &parser)
{
    return nullptr;
}

void LoopStatement::dump_node(int indent)
{
    statement->dump(indent + 4);
}

void LoopStatement::header()
{
    if (label) {
        std::wcout << *label;
    }
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
        condition->normalize(parser),
        statement->normalize(parser));
}

pType WhileStatement::bind(Parser &parser)
{
    return nullptr;
}

void WhileStatement::dump_node(int indent)
{
    condition->dump(indent + 4);
    statement->dump(indent + 4);
}

void WhileStatement::header()
{
    if (label) {
        std::wcout << *label;
    }
}

}
