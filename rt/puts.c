/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <assert.h>
#include <stdlib.h>
#include <unistd.h>
#include <wchar.h>

//#include "arwen.h"

size_t arwen$puts(wchar_t const *ptr, uint64_t len)
{
    // setlocale(LC_CTYPE, ""); // Ensure locale is set for UTF-8
    size_t mb_len = wcsnrtombs(NULL, &ptr, len, 0, NULL);
    assert(mb_len != (size_t)-1);
    char *mbstr = malloc(mb_len + 1);
    assert(mbstr != NULL);
    wcsnrtombs(mbstr, &ptr, len, mb_len + 1, NULL);
    size_t ret = write(1, mbstr, mb_len);
    free(mbstr);
    return ret;
}
