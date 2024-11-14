/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <format>
#include <sstream>

#include <SimpleFormat.h>

namespace Arwen {

struct SimpleFormatParser {
    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end())
            return it;
        if (*it != '}')
            throw std::format_error("Invalid format args for SymbolType.");
        return it;
    }
};

}
