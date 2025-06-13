/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <vector>

#include <Util/Logging.h>
#include <Util/Utf8.h>

namespace Util {

struct Arena {
    struct Block {
        void  *ptr;
        size_t size;
        size_t used { 0 };

        Block(size_t size);
        ~Block();
        void *allocate(size_t bytes);
    };

    constexpr static size_t BLOCK_SIZE = 4096;
    size_t                  blocksize { 0 };
    std::vector<Block>      blocks;

    Arena(size_t blocksize = BLOCK_SIZE);
    void *allocate(size_t size);
};

struct InternedStringBlock {
    enum class Encoding {
        UTF8,
        UTF32,
    };
    constexpr static uint64_t MAGIC = 0xCAFEBABEDEADBEEF;

    void    *twin;
    Encoding encoding;
    int64_t  length;
    uint64_t magic { MAGIC };
};

template<typename T>
struct InternedString {
    T const             *ptr;
    InternedStringBlock *block;

    explicit InternedString(T const *str)
        : ptr(str)
        , block(reinterpret_cast<InternedStringBlock *>(reinterpret_cast<char *>(const_cast<T *>(str)) - sizeof(InternedStringBlock)))
    {
        assert(block->magic == InternedStringBlock::MAGIC);
        if constexpr (std::is_same_v<T, char>) {
            assert(block->encoding == InternedStringBlock::Encoding::UTF8);
        }
        if constexpr (std::is_same_v<T, wchar_t>) {
            assert(block->encoding == InternedStringBlock::Encoding::UTF32);
        }
    }

    int64_t length() const
    {
        return block->length;
    }

    template<typename U>
    U const *converted() const
    {
        if constexpr (std::is_same_v<T, char>) {
            static_assert(std::is_same_v<U, wchar_t>);
        }
        if constexpr (std::is_same_v<T, wchar_t>) {
            static_assert(std::is_same_v<U, char>);
        }
        return static_cast<U const *>(block->twin);
    }
};

template<typename T>
T const *make_interned_string(Arena &, std::basic_string_view<T> const)
{
    static_assert("Wrong string character type");
    return nullptr;
}

template<>
inline char const *make_interned_string(Arena &arena, std::string_view const str)
{
    auto const utf32 = as_wstring(str);
    auto const utf8_ptr = arena.allocate(sizeof(InternedStringBlock) + str.length());
    auto const utf8_block = static_cast<InternedStringBlock *>(utf8_ptr);
    utf8_block->length = str.length();
    utf8_block->encoding = InternedStringBlock::Encoding::UTF8;
    utf8_block->magic = InternedStringBlock::MAGIC;
    auto const utf8_str = static_cast<char *>(static_cast<char*>(utf8_ptr) + sizeof(InternedStringBlock));
    auto const utf32_ptr = arena.allocate(sizeof(InternedStringBlock) + utf32.length() * sizeof(wchar_t));
    auto const utf32_block = static_cast<InternedStringBlock *>(utf32_ptr);
    utf32_block->length = utf32.length();
    utf32_block->encoding = InternedStringBlock::Encoding::UTF32;
    utf32_block->magic = InternedStringBlock::MAGIC;
    auto const utf32_str = reinterpret_cast<wchar_t *>(static_cast<char*>(utf32_ptr) + sizeof(InternedStringBlock));
    wcsncpy(utf32_str, utf32.data(), utf32.length());
    utf8_block->twin = utf32_str;
    utf32_block->twin = utf8_str;
    return utf8_str;
}

template<>
inline wchar_t const *make_interned_string(Arena &arena, std::wstring_view const str)
{
    auto const utf8 = as_utf8(str);
    auto const utf8_ptr = arena.allocate(sizeof(InternedStringBlock) + utf8.length());
    auto const utf8_block = static_cast<InternedStringBlock *>(utf8_ptr);
    utf8_block->length = str.length();
    utf8_block->encoding = InternedStringBlock::Encoding::UTF8;
    utf8_block->magic = InternedStringBlock::MAGIC;
    auto const utf8_str = static_cast<char*>(utf8_ptr) + sizeof(InternedStringBlock);
    auto const utf32_ptr = arena.allocate(sizeof(InternedStringBlock) + str.length() * sizeof(wchar_t));
    auto const utf32_block = static_cast<InternedStringBlock *>(utf32_ptr);
    utf32_block->length = str.length();
    utf32_block->encoding = InternedStringBlock::Encoding::UTF32;
    utf32_block->magic = InternedStringBlock::MAGIC;
    auto const utf32_str = reinterpret_cast<wchar_t *>(static_cast<char*>(utf32_ptr) + sizeof(InternedStringBlock));
    wcsncpy(utf32_str, str.data(), str.length());
    utf8_block->twin = utf32_str;
    utf32_block->twin = utf8_str;
    return utf32_str;
}

}
