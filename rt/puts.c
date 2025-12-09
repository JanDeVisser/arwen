/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <wchar.h>

#include <rt/arwen.h>

static slice_t to_string_int64(slice_t dest, int64_t i, int radix);
static slice_t to_string_uint64(slice_t dest, uint64_t i, int radix);

size_t arwen$puts(wchar_t const *ptr, uint64_t len)
{
    slice_t utf8 = to_utf8((slice_t) { (void *) ptr, len });
    size_t  ret = write(1, utf8.ptr, utf8.size);
    return ret;
}

size_t arwen$endln()
{
    return write(1, "\n", 1);
}

size_t arwen$putln(wchar_t const *ptr, uint64_t len)
{
    size_t ret = arwen$puts(ptr, len);
    ret += arwen$endln();
    return ret;
}

size_t arwen$puti(int64_t i)
{
    wchar_t buf[32];
    slice_t str = to_string_int64((slice_t) { buf, 32 }, i, 10);
    if (str.ptr == NULL) {
        return -1;
    }
    return write(1, str.ptr, str.size);
}

size_t arwen$putu(uint64_t i)
{
    wchar_t buf[32];
    slice_t str = to_string_uint64((slice_t) { buf, 32 }, i, 10);
    if (str.ptr == NULL) {
        return -1;
    }
    return write(1, str.ptr, str.size);
}

slice_t to_string_int64(slice_t dest, int64_t i, int radix)
{
    char   *ptr = dest.ptr + dest.size;
    wchar_t digit;
    int64_t num = i;
    int     len = 0;

    if (dest.ptr == NULL || dest.size <= 0) {
        return (slice_t) { NULL, -1 };
    }
    if (radix == 0) {
        radix = 10;
    }
    do {
        --ptr;
        ++len;
        if (len > dest.size) {
            return (slice_t) { NULL, -1 };
        }
        digit = num % radix;
        digit += '0';
        if (digit > '9') {
            digit += 'A' - ('0' + 10);
        }
        *ptr = digit;
        num /= radix;
    } while (num > 0);
    if (i < 0) {
        if (len >= dest.size) {
            return (slice_t) { NULL, -1 };
        }
        --ptr;
        ++len;
        *ptr = '-';
    }
    return (slice_t) { ptr, len };
}

slice_t to_string_uint64(slice_t dest, uint64_t i, int radix)
{
    char   *ptr = dest.ptr + dest.size;
    wchar_t digit;
    int64_t num = i;
    int     len = 0;

    if (dest.ptr == NULL || dest.size <= 0) {
        return (slice_t) { NULL, -1 };
    }
    if (radix == 0) {
        radix = 10;
    }
    do {
        --ptr;
        ++len;
        if (len > dest.size) {
            return (slice_t) { NULL, -1 };
        }
        digit = num % radix;
        digit += '0';
        if (digit > '9') {
            digit += 'A' - ('0' + 10);
        }
        *ptr = digit;
        num /= radix;
    } while (num > 0);
    return (slice_t) { ptr, len };
}
