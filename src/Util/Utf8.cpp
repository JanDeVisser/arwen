/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <fstream>
#include <iconv.h>
#include <print>

#include <Util/Error.h>
#include <Util/Utf8.h>
#include <utility>

namespace Util {

struct UTF8 {
    UTF8()
        : cd_in(iconv_open("WCHAR_T", "UTF-8"))
        , cd_out(iconv_open("UTF-8", "WCHAR_T"))
    {
        assert(cd_in != reinterpret_cast<iconv_t>(-1));
        assert(cd_out != reinterpret_cast<iconv_t>(-1));
    }

    static UTF8 utf8;

    iconv_t cd_in;
    iconv_t cd_out;

    Result<std::string> to_utf8(std::wstring_view const &s)
    {
        auto        data = const_cast<wchar_t *>(s.data());
        size_t      in_count = s.length() * sizeof(wchar_t);
        std::string ret;
        ret.resize(s.length() * 16, '\0');
        size_t out_count = s.length() * 16;
        auto   out_buffer = ret.data();
        if (auto done = iconv(cd_out,
                reinterpret_cast<char **>(&data), &in_count,
                &out_buffer, &out_count);
            static_cast<int>(done) < 0) {
            return std::unexpected<LibCError>(std::in_place_t {});
        }
        ret.resize(s.length() * 16 - out_count);
        while (!ret.empty() && ret[ret.size() - 1] == 0) {
            ret.resize(ret.size() - 1); // HACK somehow iconv sometimes leaves
                                        // '0' chars at the end of the
                                        // converted string
        }
        return ret;
    }

    Result<std::wstring> to_wstring(std::string_view const &s)
    {
        auto         data = const_cast<char *>(s.data());
        size_t       in_count = s.length();
        std::wstring ret;
        ret.resize(2 * s.length(), '\0');
        size_t out_count = 2 * s.length() * sizeof(wchar_t);
        auto   out_buffer = ret.data();
        if (auto done = iconv(cd_in,
                &data, &in_count,
                reinterpret_cast<char **>(&out_buffer), &out_count);
            static_cast<int>(done) < 0) {
            return std::unexpected<LibCError>(std::in_place_t {});
        }
        ret.resize(2 * s.length() * sizeof(wchar_t) - out_count);
        while (!ret.empty() && ret[ret.size() - 1] == 0) {
            ret.resize(ret.size() - 1);
        }
        return ret;
    }

    Result<ssize_t> write(std::ofstream &os, std::wstring_view const &contents)
    {
        // TODO stream instead of allocating whole new string but RAM is cheap
        // so whatever
        if (auto converted_maybe = to_utf8(contents); converted_maybe.has_value()) {
            os << converted_maybe.value();
            if (os.fail() || os.bad()) {
                return make_error();
            }
            return contents.length();
        } else {
            return std::unexpected<LibCError>(converted_maybe.error());
        }
    }

    Result<std::wstring> read(std::ifstream &is)
    {
        std::string contents;
        for (char ch; is.get(ch);) {
            contents += ch;
        }
        if (auto converted_maybe = to_wstring(contents); converted_maybe.has_value()) {
            return converted_maybe.value();
        } else {
            return std::unexpected<LibCError>(converted_maybe.error());
        }
    }
};

UTF8 UTF8::utf8 {};

Result<std::string> to_utf8(std::wstring_view const &s)
{
    return UTF8::utf8.to_utf8(s);
}

Result<std::wstring> to_wstring(std::string_view const &s)
{
    return UTF8::utf8.to_wstring(s);
}

Result<ssize_t> write_utf8(std::ofstream &os, std::wstring_view const &contents)
{
    return UTF8::utf8.write(os, contents);
}

Result<std::wstring> read_utf8(std::ifstream &is)
{
    return UTF8::utf8.read(is);
}

}
