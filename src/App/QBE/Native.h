/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <string_view>
#include <vector>

#include <App/Type.h>
#include <App/Value.h>

namespace Arwen::QBE {

using namespace Arwen;

extern bool native_call(std::string_view name, void *params, std::vector<pType> const &types, void *return_value, pType const &return_type);

}
