/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Grammar/Grammar.h>

namespace Arwen {

Result<ssize_t, GrammarError> Sequence::build_firsts()
{
//    std::println("{}::build_firsts", *this);
    auto ret = Symbol::firsts(symbols, 0, grammar, firsts);
//    std::println("{}::build_firsts {}", *this, firsts);
    return ret;
}

Result<std::optional<int64_t>, GrammarError> Sequence::check_LL1(SymbolSet f_i, Sequence::Iterator tail_begin, Sequence::Iterator tail_end, size_t j)
{
    if (tail_begin == tail_end) {
        return { {} };
    }
    SymbolSet f_j {};
    f_j.union_with(tail_begin->firsts);
    f_j.intersect(f_i);
    if (!f_j.empty()) {
        return j;
    }
    return check_LL1(f_i, ++tail_begin, tail_end, j + 1);
}

}
