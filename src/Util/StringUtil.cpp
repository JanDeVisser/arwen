/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cctype>
#include <cstring>

#include "StringUtil.h"

namespace Util {

int isbdigit(int ch)
{
    return ch == '0' || ch == '1';
}

int stricmp(char const *a, char const *b)
{
    unsigned char ca, cb;
    do {
        ca = tolower(toupper((unsigned char) *a++));
        cb = tolower(toupper((unsigned char) *b++));
    } while (ca == cb && ca != '\0');
    return ca - cb;
}

std::string to_upper(std::string_view const &input)
{
    std::string ret;
    for (auto &ch : input) {
        ret += (char) toupper((int) ch);
    }
    return ret;
}

std::string to_lower(std::string_view const &input)
{
    std::string ret;
    for (auto &ch : input) {
        ret += (char) tolower((int) ch);
    }
    return ret;
}

std::string capitalize(std::string_view const &input)
{
    std::string ret { input };
    if (!ret.empty()) {
        ret[0] = (char) toupper(ret[0]);
    }
    return ret;
}

std::wstring capitalize(std::wstring_view const &input)
{
    std::wstring ret { input };
    if (!ret.empty()) {
        ret[0] = (char) towupper(ret[0]);
    }
    return ret;
}

std::size_t replace_all(std::string &inout, std::string_view what, std::string_view with)
{
    std::size_t count {};
    for (std::string::size_type pos {};
        inout.npos != (pos = inout.find(what.data(), pos, what.length()));
        pos += with.length(), ++count) {
        inout.replace(pos, what.length(), with.data(), with.length());
    }
    return count;
}

std::string c_escape(std::string const &s)
{
    std::string ret;
    for (char c : s) {
        if (c == '"' || c == '\'' || c == '\\') {
            ret += "\\";
        }
        ret += c;
    }
    return ret;
}

// std::vector<std::pair<std::string, std::string>> parse_pairs(std::string const &s, char pair_sep, char name_value_sep)
// {
//     auto                                             pairs = split(s, pair_sep);
//     std::vector<std::pair<std::string, std::string>> ret;
//     for (auto &pair : pairs) {
//         auto nvp = split(strip(pair), name_value_sep);
//         switch (nvp.size()) {
//         case 0:
//             break;
//         case 1:
//             if (!strip(nvp[0]).empty())
//                 ret.emplace_back(strip(nvp[0]), "");
//             break;
//         case 2:
//             if (!strip(nvp[0]).empty())
//                 ret.emplace_back(strip(nvp[0]), strip(nvp[1]));
//             break;
//         default:
//             if (!strip(nvp[0]).empty()) {
//                 std::vector<std::string> tail;
//                 for (auto ix = 1u; ix < nvp.size(); ix++) {
//                     tail.emplace_back(nvp[ix]);
//                 }
//                 auto value = strip(join(tail, name_value_sep));
//                 ret.emplace_back(strip(nvp[0]), strip(value));
//             }
//         }
//     }
//     return ret;
// }

std::string dequote(std::string const &str, char quote)
{
    auto s = strip(std::string_view { str });
    if (!s.empty() && s.starts_with(quote) && s.ends_with(quote))
        s = s.substr(1, s.length() - 2);
    return std::string { s };
}

unsigned long parse_binary(char const *str, char **end)
{
    unsigned long ret = 0;
    if ((strlen(str) > 2) && (str[0] == '0') && (toupper(str[1]) == 'B')) {
        str += 2;
    }
    for (*end = const_cast<char *>(str); **end; (*end)++) {
        if (**end == '0') {
            ret <<= 1;
            continue;
        }
        if (**end == '1') {
            ret = (ret << 1) + 1;
            continue;
        }
        break;
    }
    return ret;
}

}
