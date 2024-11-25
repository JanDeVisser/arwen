/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

#include <Result.h>

namespace Arwen {

enum class UnescapeError {
    NumberFormat,
    IntOutOfRange,
    EscapeSyntax,
};

Result<std::optional<std::string>, UnescapeError> unescape(std::string_view s);
size_t                                            unescape(char *buffer, size_t size);

}
