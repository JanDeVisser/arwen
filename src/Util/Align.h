/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>

#include <Util/Logging.h>

namespace Util {

template<int Align>
intptr_t alignat(intptr_t bytes)
{
    static_assert(Align > 0 && (Align & (Align - 1)) == 0); // Align must be power of 2
    return (bytes + (Align - 1)) & ~(Align - 1);
}

template<typename T>
intptr_t alignat(intptr_t bytes)
{
    return alignat<alignof(T)>(bytes);
}

inline intptr_t alignat(intptr_t bytes, intptr_t align)
{
    assert(align > 0 && (align & (align - 1)) == 0); // Align must be power of 2
    return (bytes + (align - 1)) & ~(align - 1);
}

template<uint8_t WordSize = 8>
inline intptr_t words_needed(intptr_t bytes)
{
    auto ret { bytes / WordSize };
    return (bytes % WordSize) ? ret + 1 : ret;
}

}
