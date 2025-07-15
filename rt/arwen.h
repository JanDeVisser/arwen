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

#ifdef __cplusplus
extern "C" {
#endif

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

size_t      utf32_length_for_utf8_slice(slice_t slice);
size_t      utf32_length_for_cstring(char const *c_string);
size_t      utf8_length_for_utf32_slice(slice_t slice);
slice_t     to_utf8(slice_t utf32);
slice_t     to_utf32(slice_t utf8);
slice_t     cstring_to_string(char const *cstring);
char const *string_to_cstring(slice_t string);

#define ALIGNAT(bytes, align) ((bytes + (align - 1)) & ~(align - 1))
#define AS_SLICE(dynarr) (*(slice_t *) (&dynarr))

#ifdef __cplusplus
}
#endif

#endif /* __ARWEN_H__ */
