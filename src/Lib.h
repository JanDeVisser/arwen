/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <format>
#include <optional>
#include <print>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>

#include <Logging.h>

namespace Arwen {

inline bool ichar_equals(char a, char b)
{
    return std::tolower(static_cast<unsigned char>(a)) == std::tolower(static_cast<unsigned char>(b));
}

inline bool iequals(std::string_view const a, std::string_view const b)
{
    return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin(), ichar_equals);
}

inline std::string_view trim(std::string_view const &s)
{
    if (s.empty())
        return s;
    auto start = 0;
    while (start < s.length() && isspace(s[start]))
        ++start;
    if (start == s.length())
        return {};

    auto end = s.length() - 1;
    while (static_cast<ssize_t>(end) >= 0 && isspace(s[end]))
        --end;
    return s.substr(start, end + 1);
}

template<typename T>
inline std::optional<T> decode(std::string_view s, ...)
{
    UNREACHABLE();
}

template<typename T>
inline constexpr std::string_view to_string(T const &)
{
    return "";
}

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
