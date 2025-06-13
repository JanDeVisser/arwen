/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Arena.h>

namespace Util {
Arena::Block::Block(size_t const size)
    : ptr(nullptr)
    , size(size)
{
    assert(size > 0 && size % 8 == 0);
    ptr = malloc(size);
    assert(ptr);
}

Arena::Block::~Block()
{
    free(ptr);
}

void *Arena::Block::allocate(size_t const bytes)
{
    if (used + bytes <= size) {
        void *ret = static_cast<char *>(ptr) + used;
        used += size;
        return ret;
    }
    return nullptr;
}

Arena::Arena(size_t const blocksize)
    : blocksize(blocksize)
{
}

void *Arena::allocate(size_t const size)
{
    for (auto &block : blocks) {
        if (auto const ret = block.allocate(size); ret) {
            return ret;
        }
    }
    auto const sz = std::max(blocksize, size);
    blocks.emplace_back(sz);
    return blocks.back().allocate(size);
}

}
