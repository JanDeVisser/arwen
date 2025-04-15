/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstdlib>

namespace Util {

struct TokenLocation {
    TokenLocation() = default;
    TokenLocation(TokenLocation const &) = default;
    TokenLocation(TokenLocation const &first, TokenLocation const &second)
    {
        index = std::min(first.index, second.index);
        length = std::max(first.index + first.length, second.index + second.length) - index;
        line = std::min(first.line, second.line);
        column = std::min(first.column, second.column);
    }
    TokenLocation operator+(TokenLocation const &other) const { return TokenLocation { *this, other }; }

    size_t index { 0 };
    size_t length { 0 };
    size_t line { 0 };
    size_t column { 0 };

    TokenLocation merge(TokenLocation const &other) const
    {
        return TokenLocation { *this, other };
    }
};

}
