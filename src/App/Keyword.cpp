/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Logging.h>

#include <App/Operator.h>

namespace Arwen {

char const *ArwenKeyword_name(ArwenKeyword kw)
{
    switch (kw) {
#undef S
#define S(KW, S)           \
    case ArwenKeyword::KW: \
        return S;
        ArwenKeywords(S)
#undef S
            default : UNREACHABLE();
    }
}

char const *Operator_name(Operator op)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return #O;
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

}
