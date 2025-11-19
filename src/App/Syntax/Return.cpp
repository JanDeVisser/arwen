/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>

namespace Arwen {

Break::Break(Label label)
    : label(std::move(label))
{
}

pType Break::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

std::wostream &Break::header(ASTNode const &, std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

Continue::Continue(Label label)
    : label(std::move(label))
{
}

pType Continue::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

std::wostream &Continue::header(ASTNode const &, std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

Error::Error(ASTNode expression)
    : expression(std::move(expression))
{
}

ASTNode Error::normalize(ASTNode const &n)
{
    expression = expression->normalize();
    return n;
}

ASTNode Error::stamp(ASTNode const &n)
{
    expression = expression->stamp();
    return n;
}

pType Error::bind(ASTNode const &n)
{
    return expression->bind();
}

void Error::dump_node(ASTNode const &, int indent)
{
    if (expression) {
        expression->dump(indent + 4);
    }
}

Return::Return(ASTNode expression)
    : expression(std::move(expression))
{
}

ASTNode Return::normalize(ASTNode const &n)
{
    expression = expression->normalize();
    return n;
}

ASTNode Return::stamp(ASTNode const &n)
{
    expression = expression->stamp();
    return n;
}

pType Return::bind(ASTNode const &n)
{
    return expression->bind();
}

void Return::dump_node(ASTNode const &, int indent)
{
    if (expression) {
        expression->dump(indent + 4);
    }
}

Yield::Yield(Label label, ASTNode statement)
    : label(std::move(label))
    , statement(std::move(statement))
{
}

ASTNode Yield::normalize(ASTNode const &n)
{
    statement = statement->normalize();
    return n;
}

ASTNode Yield::stamp(ASTNode const &n)
{
    statement = statement->stamp();
    return n;
}

pType Yield::bind(ASTNode const &n)
{
    return statement->bind();
}

void Yield::dump_node(ASTNode const &, int indent)
{
    statement->dump(indent + 4);
}

std::wostream &Yield::header(ASTNode const &, std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

}
