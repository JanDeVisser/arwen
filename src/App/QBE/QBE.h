/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <expected>
#include <string>

#include <App/SyntaxNode.h>

namespace Arwen::QBE {

using GenResult = std::expected<int, std::wstring>;
GenResult generate_qbe(ASTNode const &node);

}
