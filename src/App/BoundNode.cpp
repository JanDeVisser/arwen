/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/BoundNode.h>

namespace Arwen {
using namespace Util;

BoundNode::BoundNode(pSyntaxNode node, pType type)
    : node(std::move(node))
    , type(std::move(type))
{
}

}
