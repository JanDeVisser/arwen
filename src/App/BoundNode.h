/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

using namespace Util;

struct BoundNode {
    pSyntaxNode node;
    pType       type;

    BoundNode(pSyntaxNode node, pType type);
};

}
