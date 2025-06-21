/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Align.h>
#include <Util/Arena.h>

namespace Util {

Arena::Block::Block(size_t const &size) noexcept
    : ptr(nullptr)
    , size(size)
    , used(0)
{
    assert(size > 0 && size % 8 == 0);
    ptr = static_cast<char*>(malloc(size));
    assert(ptr != nullptr && "Out of memory");
    std::cout << "Block(" << size << "): used " << used << " size " << size << std::endl;
}

Arena::Block::~Block()
{
    std::cout << "~Block()" << std::endl;
    free(ptr);
}

void *Arena::Block::allocate(size_t const &bytes)
{
    auto b = alignat(bytes, 8);
    assert(b % 8 == 0 && b != 0);
    if (used + b <= size) {
        void *ret = ptr + used;
        used += b;
        std::cout << "allocate(" << bytes << "): aligned " << b << " used " << used << " size " << size << std::endl;
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
