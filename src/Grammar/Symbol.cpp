/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Grammar/Grammar.h>
#include <ScopeGuard.h>

namespace Arwen {

bool Symbol::operator<(Symbol const &rhs) const
{
    if (m_type != rhs.m_type) {
        return static_cast<int>(m_type) < static_cast<int>(rhs.m_type);
    }
    switch (m_type) {
    case Arwen::SymbolType::Terminal:
        return symbol.terminal < rhs.symbol.terminal;
    case Arwen::SymbolType::NonTerminal:
        return symbol.non_terminal < rhs.symbol.non_terminal;
    case Arwen::SymbolType::Action:
        return symbol.action < rhs.symbol.action;
    default:
        return false;
    }
}

Result<ssize_t, GrammarError> Symbol::firsts(Symbols const& symbols, size_t ix, Grammar &grammar, SymbolSet &f)
{
    auto sz = static_cast<ssize_t>(f.size());
    ssize_t count { 0 };
    for (auto i = ix; i < symbols.size(); ++i) {
        f.remove(Symbol {});
        auto   &head = symbols[i];
        switch (head.type()) {
        case SymbolType::End:
        case SymbolType::Empty:
        case SymbolType::Terminal:
            f.add(head);
            return count + static_cast<ssize_t>(f.size()) - sz;
        case SymbolType::Action:
            break;
        case SymbolType::NonTerminal: {
            auto nt_name = head.symbol.non_terminal;
            if (auto it = grammar.rules.find(nt_name); it != grammar.rules.end()) {
                Rule &rule = it->second;
                count += TRY_EVAL(rule.update_firsts());
                f.union_with(rule.firsts);
                return count + static_cast<ssize_t>(f.size()) - sz;
            } else {
                return GrammarError::RuleNotFound;
            }
        }
        }
        if (f.empty()) {
            f.add({});
        }
        if (!f.has({})) {
            return count + static_cast<ssize_t>(f.size()) - sz;
        }
    }
    f.add({});
    count += static_cast<ssize_t>(f.size()) - sz;
    return count;
}

}
