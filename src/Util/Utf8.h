/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <string>

#include <Util/Result.h>

namespace Util {

Result<std::string>  to_utf8(std::wstring_view const &s);
Result<std::wstring> to_wstring(std::string_view const &s);
Result<ssize_t>      write_utf8(std::ofstream &os, std::wstring_view const &contents);
Result<std::wstring> read_utf8(std::ifstream &is);

template<class T>
std::string as_utf8(std::basic_string_view<T> const &)
{
    static_assert(false);
}

template<>
inline std::string as_utf8(std::string_view const &s)
{
    return std::string { s };
}

template<>
inline std::string as_utf8(std::wstring_view const &s)
{
    if (auto result_maybe = to_utf8(s); result_maybe.is_error()) {
        abort();
    } else {
        return result_maybe.value();
    }
}

template<class T>
std::string as_utf8(std::basic_string<T> const &)
{
    static_assert(false);
}

template<>
inline std::string as_utf8(std::string const &s)
{
    return s;
}

template<>
inline std::string as_utf8(std::wstring const &s)
{
    if (auto result_maybe = to_utf8(s); result_maybe.is_error()) {
        abort();
    } else {
        return result_maybe.value();
    }
}

inline std::string as_utf8(const char *s)
{
    return as_utf8(std::string_view { s });
}

inline std::string as_utf8(const wchar_t *s)
{
    return as_utf8(std::wstring_view { s });
}

template<class T>
std::wstring as_wstring(std::basic_string_view<T> const &)
{
    static_assert(false);
}

template<>
inline std::wstring as_wstring(std::string_view const &s)
{
    if (auto result_maybe = to_wstring(s); result_maybe.is_error()) {
        abort();
    } else {
        return result_maybe.value();
    }
}

template<>
inline std::wstring as_wstring(std::wstring_view const &s)
{
    return { std::wstring { s } };
}

template<class T>
std::wstring as_wstring(std::basic_string<T> const &)
{
    static_assert(false);
}

template<>
inline std::wstring as_wstring(std::string const &s)
{
    if (auto result_maybe = to_wstring(s); result_maybe.is_error()) {
        abort();
    } else {
        return result_maybe.value();
    }
}

template<>
inline std::wstring as_wstring(std::wstring const &s)
{
    return s;
}

inline std::wstring as_wstring(const char *s)
{
    return as_wstring(std::string_view { s });
}

inline std::wstring as_wstring(const wchar_t *s)
{
    return as_wstring(std::wstring_view { s });
}

}
