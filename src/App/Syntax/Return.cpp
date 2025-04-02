/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/SyntaxNode.h>

namespace Arwen {

Return::Return(pSyntaxNode expression)
    : SyntaxNode(SyntaxNodeType::Return)
    , expression(std::move(expression))
{
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
