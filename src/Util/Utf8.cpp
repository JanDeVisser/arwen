/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <fstream>
#include <iconv.h>

#include <Util/Error.h>
#include <Util/Utf8.h>
#include <string>
#include <string_view>

namespace Util {

size_t utf32_length(std::string_view const &s)
{
    size_t ret = 0;
    for (size_t ix = 0; ix < s.length(); ++ret) {
        uint8_t first_byte = s[ix];

        int num_bytes;
        if ((first_byte & 0x80) == 0) {
            // Single byte character (0xxxxxxx)
            num_bytes = 1;
        } else if ((first_byte & 0xE0) == 0xC0) {
            // Two byte character (110xxxxx 10xxxxxx)
            num_bytes = 2;
        } else if ((first_byte & 0xF0) == 0xE0) {
            // Three byte character (1110xxxx 10xxxxxx 10xxxxxx)
            num_bytes = 3;
        } else if ((first_byte & 0xF8) == 0xF0) {
            // Four byte character (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
            num_bytes = 4;
        } else {
            // Invalid UTF-8 byte, skip it
            num_bytes = 1;
            continue;
        }
        ix += num_bytes;
    }
    return ret;
}

size_t utf8_length(std::wstring_view const &s)
{
    size_t utf8_pos = 0;

    size_t ret = 0;
    for (size_t i = 0; i < s.length(); i++) {
        uint32_t code_point = s[i];

        // Validate the code point
        if (code_point > 0x10FFFF) {
            code_point = 0xFFFD; // Replacement character for invalid code points
        }

        // Encode the code point in UTF-8
        if (code_point <= 0x7F) {
            // Single byte: 0xxxxxxx
            ++ret;
        } else if (code_point <= 0x7FF) {
            // Two bytes: 110xxxxx 10xxxxxx
            ret += 2;
        } else if (code_point <= 0xFFFF) {
            // Three bytes: 1110xxxx 10xxxxxx 10xxxxxx
            ret += 3;
        } else {
            // Four bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
            ret += 4;
        }
    }
    return ret;
}

Result<std::string> to_utf8(std::wstring_view const &s)
{
    size_t out_count = utf8_length(s) + 1;
    char   out_buffer[out_count];
    for (auto ix = 0; ix < out_count; ++ix) {
        out_buffer[ix] = 0;
    }

    size_t utf8_pos = 0;
    for (size_t i = 0; i < s.length(); i++) {
        uint32_t code_point = s[i];

        // Validate the code point
        if (code_point > 0x10FFFF) {
            code_point = 0xFFFD; // Replacement character for invalid code points
        }

        // Encode the code point in UTF-8
        if (code_point <= 0x7F) {
            // Single byte: 0xxxxxxx
            out_buffer[utf8_pos++] = (uint8_t) code_point;
        } else if (code_point <= 0x7FF) {
            // Two bytes: 110xxxxx 10xxxxxx
            out_buffer[utf8_pos++] = 0xC0 | (code_point >> 6);
            out_buffer[utf8_pos++] = 0x80 | (code_point & 0x3F);
        } else if (code_point <= 0xFFFF) {
            // Three bytes: 1110xxxx 10xxxxxx 10xxxxxx
            out_buffer[utf8_pos++] = 0xE0 | (code_point >> 12);
            out_buffer[utf8_pos++] = 0x80 | ((code_point >> 6) & 0x3F);
            out_buffer[utf8_pos++] = 0x80 | (code_point & 0x3F);
        } else {
            // Four bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
            out_buffer[utf8_pos++] = 0xF0 | (code_point >> 18);
            out_buffer[utf8_pos++] = 0x80 | ((code_point >> 12) & 0x3F);
            out_buffer[utf8_pos++] = 0x80 | ((code_point >> 6) & 0x3F);
            out_buffer[utf8_pos++] = 0x80 | (code_point & 0x3F);
        }
    }
    return std::string { out_buffer };
}

Result<std::wstring> to_wstring(std::string_view const &s)
{
    size_t  out_count = utf32_length(s) + 1;
    wchar_t out_buffer[sizeof(wchar_t) * out_count];
    for (auto ix = 0; ix < out_count; ++ix) {
        out_buffer[ix] = 0;
    }

    size_t utf32_pos = 0;
    size_t utf8_pos = 0;

    while (utf8_pos < s.length()) {
        uint32_t code_point = 0;
        uint8_t  first_byte = s[utf8_pos++];

        // Determine the number of bytes in this character
        int num_bytes;
        if ((first_byte & 0x80) == 0) {
            // Single byte character (0xxxxxxx)
            code_point = first_byte;
            num_bytes = 1;
        } else if ((first_byte & 0xE0) == 0xC0) {
            // Two byte character (110xxxxx 10xxxxxx)
            code_point = first_byte & 0x1F;
            num_bytes = 2;
        } else if ((first_byte & 0xF0) == 0xE0) {
            // Three byte character (1110xxxx 10xxxxxx 10xxxxxx)
            code_point = first_byte & 0x0F;
            num_bytes = 3;
        } else if ((first_byte & 0xF8) == 0xF0) {
            // Four byte character (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
            code_point = first_byte & 0x07;
            num_bytes = 4;
        } else {
            // Invalid UTF-8 byte, skip it
            continue;
        }

        // Process continuation bytes (if any)
        for (int i = 1; i < num_bytes && utf8_pos < s.length(); i++) {
            uint8_t byte = s[utf8_pos++];
            if ((byte & 0xC0) != 0x80) {
                // Invalid continuation byte
                code_point = 0xFFFD; // Replacement character
                break;
            }
            // Add the 6 bits from the continuation byte
            code_point = (code_point << 6) | (byte & 0x3F);
        }

        // Store the code point in UTF-32 format
        out_buffer[utf32_pos++] = code_point;
    }
    return std::wstring { out_buffer };
}

Result<ssize_t> write_utf8(std::ofstream &os, std::wstring_view const &contents)
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

Result<std::wstring> read_utf8(std::ifstream &is)
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

}
