/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __ARWEN_H__
#define __ARWEN_H__

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

typedef struct slice {
    void   *ptr;
    int64_t size;
} slice_t;

typedef struct dynarr {
    void   *ptr;
    int64_t size;
    int64_t capacity;
} dynarr_t;

#endif /* __ARWEN_H__ */
