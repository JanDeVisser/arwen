/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <string_view>
#include <vector>

#include <Resolve.h>
#include <Result.h>
#include <Type/Value.h>
#include <Type/Type.h>

namespace Arwen {

Result<Value, ResolveError> foreign_call(std::string_view name, std::vector<Value> const &values, PrimitiveType ret_type);

}
