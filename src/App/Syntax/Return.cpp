/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/SyntaxNode.h>

namespace Arwen {

Break::Break(Label label)
    : SyntaxNode(SyntaxNodeType::Break)
    , label(std::move(label))
{
}

pSyntaxNode Break::stamp(Parser& parser)
{
    return make_node<Break>(location, label);
}

pType Break::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

std::wostream& Break::header(std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

Continue::Continue(Label label)
    : SyntaxNode(SyntaxNodeType::Continue)
    , label(std::move(label))
{
}

pSyntaxNode Continue::stamp(Parser& parser)
{
    return make_node<Break>(location, label);
}

pType Continue::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

std::wostream& Continue::header(std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

Error::Error(pSyntaxNode expression)
    : SyntaxNode(SyntaxNodeType::Error)
    , expression(std::move(expression))
{
}

pSyntaxNode Error::normalize(Parser &parser)
{
    return make_node<Error>(location, normalize_node(expression, parser));
}

pSyntaxNode Error::stamp(Parser &parser)
{
    return make_node<Error>(location, stamp_node(expression, parser));
}

pType Error::bind(Parser &parser)
{
    return bind_node(expression, parser);
}

void Error::dump_node(int indent)
{
    if (expression) {
        expression->dump(indent + 4);
    }
}

Return::Return(pSyntaxNode expression)
    : SyntaxNode(SyntaxNodeType::Return)
    , expression(std::move(expression))
{
}

pSyntaxNode Return::normalize(Parser &parser)
{
    return make_node<Return>(location, normalize_node(expression, parser));
}

pSyntaxNode Return::stamp(Parser &parser)
{
    return make_node<Return>(location, stamp_node(expression, parser));
}

pType Return::bind(Parser &parser)
{
    return bind_node(expression, parser);
}

void Return::dump_node(int indent)
{
    if (expression) {
        expression->dump(indent + 4);
    }
}

Yield::Yield(Label label, pSyntaxNode statement)
    : SyntaxNode(SyntaxNodeType::Yield)
    , label(std::move(label))
    , statement(std::move(statement))
{
}

pSyntaxNode Yield::normalize(Parser &parser)
{
    return make_node<Yield>(
        location,
        label,
        normalize_node(statement, parser));
}

pSyntaxNode Yield::stamp(Parser &parser)
{
    return make_node<Yield>(
        location,
        label,
        stamp_node(statement, parser));
}

pType Yield::bind(Parser &parser)
{
    return bind_node(statement, parser);
}

void Yield::dump_node(int indent)
{
    statement->dump(indent + 4);
}

std::wostream& Yield::header(std::wostream &os)
{
    if (label) {
        os << *label;
    }
    return os;
}

}
