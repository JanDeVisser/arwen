/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <cstdint>

#include <Result.h>
#include <Grammar/Grammar.h>
#include <ScopeGuard.h>

namespace Arwen {

bool Symbol::operator<(Symbol const &rhs) const
{
    if (type() != rhs.type()) {
        return static_cast<int>(type()) < static_cast<int>(rhs.type());
    }
    return symbol < rhs.symbol;
}

Result<int64_t, GrammarError> Symbol::firsts(Symbols const& symbols, size_t ix, Grammar &grammar, SymbolSet &f)
{
    auto sz = static_cast<int64_t>(f.size());
    int64_t count { 0 };
    for (auto i = ix; i < symbols.size(); ++i) {
        f.remove(Symbol {});
        auto   &head = symbols[i];
        switch (head.type()) {
        case SymbolType::End:
        case SymbolType::Empty:
        case SymbolType::Terminal:
            f.add(head);
            return count + static_cast<int64_t>(f.size()) - sz;
        case SymbolType::Action:
            break;
        case SymbolType::NonTerminal: {
            auto nt_name = std::get<std::string_view>(head.symbol);
            if (auto it = grammar.rules.find(nt_name); it != grammar.rules.end()) {
                Rule &rule = it->second;
                count += TRY_EVAL(rule.update_firsts());
                f.union_with(rule.firsts);
                return count + static_cast<int64_t>(f.size()) - sz;
            } else {
                std::println(std::cerr, "Rule '{}' not found", nt_name);
                return GrammarError::RuleNotFound;
            }
        }
        }
        if (f.empty()) {
            f.add({});
        }
        if (!f.has({})) {
            return count + static_cast<int64_t>(f.size()) - sz;
        }
    }
    f.add({});
    count += static_cast<int64_t>(f.size()) - sz;
    return count;
}

}
