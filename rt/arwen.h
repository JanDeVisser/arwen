/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __ARWEN_H__
#define __ARWEN_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct slice {
    void   *ptr;
    int64_t size;
} slice_t;

typedef struct dynarr {
    void   *ptr;
    int64_t size;
    int64_t capacity;
} dynarr_t;

typedef struct array {
    void   *ptr;
    int64_t size;
} array_t;

size_t dynarr_append(dynarr_t *arr, slice_t const slice, size_t elem_size);
void   dynarr_clear(dynarr_t *arr);
void   dynarr_free(dynarr_t *arr);

#define ALIGNAT(bytes, align) ((bytes + (align - 1)) & ~(align - 1))
#define AS_SLICE(dynarr) (*(slice_t *) (&dynarr))

#ifdef __cplusplus
}
#endif

#endif /* __ARWEN_H__ */
