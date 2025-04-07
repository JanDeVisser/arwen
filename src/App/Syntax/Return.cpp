/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/SyntaxNode.h>

namespace Arwen {

Error::Error(pSyntaxNode expression)
    : SyntaxNode(SyntaxNodeType::Error)
    , expression(std::move(expression))
{
}

pSyntaxNode Error::normalize(Parser &parser)
{
    return make_node<Error>(location, expression->normalize(parser));
}

pBoundNode Error::bind()
{
    return nullptr;
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
    return make_node<Return>(location, expression->normalize(parser));
}

pBoundNode Return::bind()
{
    return nullptr;
}

void Return::dump_node(int indent)
{
    if (expression) {
        expression->dump(indent + 4);
    }
}

}
