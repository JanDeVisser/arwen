/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Grammar/Grammar.h>
#include <ScopeGuard.h>

namespace Arwen {

Result<ssize_t, GrammarError> Rule::update_firsts()
{
    if (firsts_in_progress) {
        return 0;
    }
    firsts_in_progress = true;
    ssize_t count { 0 };
    for (auto &seq : sequences) {
        count += TRY_EVAL(seq.build_firsts());
        auto sz = static_cast<ssize_t>(firsts.size());
        firsts.union_with(seq.firsts);
        count += static_cast<ssize_t>(firsts.size()) - sz;
    }
    if (firsts.empty()) {
        firsts.add({});
        count += 1;
    }
    return count;
}

Result<ssize_t, GrammarError> Rule::update_follows()
{
    if (follows_in_progress) {
        return 0;
    }
    follows_in_progress = true;
    ssize_t count { 0 };
    for (auto &seq : sequences) {
        for (auto ix = 0; ix < seq.symbols.size(); ++ix) {
            auto &symbol = seq.symbols[ix];
            switch (symbol.type()) {
            case SymbolType::NonTerminal: {
                auto nt = symbol.non_terminal();
                if (!grammar.rules.contains(nt)) {
                    log_error("build_follows(): rule for non-terminal '{}' not found\n", nt);
                    return GrammarError::RuleNotFound;
                }
                auto     &non_terminal_rule = grammar.rules.at(nt);
                SymbolSet f {};
                TRY_EVAL(Symbol::firsts(seq.symbols, ix + 1, grammar, f));
                if (f.has({})) {
                    auto sz = static_cast<ssize_t>(non_terminal_rule.follows.size());
                    non_terminal_rule.follows.union_with(follows);
                    count += static_cast<ssize_t>(non_terminal_rule.follows.size()) - sz;
                    f.remove(Symbol{});
                }
                auto sz = static_cast<ssize_t>(non_terminal_rule.follows.size());
                non_terminal_rule.follows.union_with(f);
                count += static_cast<ssize_t>(non_terminal_rule.follows.size()) - sz;
            } break;
            default:
                break;
            }
        }
    }
    return count;
}

Error<GrammarError> Rule::check_LL1()
{
    auto has_empty { false };
    for (auto i = 0; i < sequences.size(); ++i) {
        auto &seq = sequences[i];
        if (auto res = seq.check_LL1(seq.firsts, sequences.begin() + i + 1, sequences.end(), i + 1); res.has_value()) {
            auto j = res.value().value();
            log_error("LL1 check: first sets {} ({}) and {} ({}) of non-terminal '{}' are not disjoint\n", i, sequences[i].firsts, j, sequences[j].firsts, non_terminal);
            return GrammarError::GrammarNotLL1;
        }
        if (sequences.size() > 1) {
            auto is_empty = seq.firsts.has({});
            if (is_empty) {
                if (has_empty) {
                    log_error("LL1 check: non-terminal '{}' has more than one sequence deriving the Empty symbol\n", non_terminal);
                    return GrammarError::GrammarNotLL1;
                }
                has_empty = true;
                SymbolSet f_i_intersect_followA {};
                f_i_intersect_followA.union_with(seq.firsts);
                f_i_intersect_followA.intersect(follows);
                if (!f_i_intersect_followA.empty()) {
                    log_error("LL1 check: follow set and first set {} of non-terminal '{}' are not disjoint\n", i, non_terminal);
                    return GrammarError::GrammarNotLL1;
                }
            }
        }
    }
    return {};
}

void Rule::add_transition(Symbol const &symbol, size_t ix)
{
    switch (symbol.type()) {
    case SymbolType::Empty: {
        for (auto &follow : follows) {
            add_transition(follow, ix);
        }
    } break;
    default: {
        if (!parse_table.contains(symbol)) {
            parse_table.emplace(symbol, ix);
        }
    } break;
    }
}

void Rule::build_parse_table()
{
    for (auto ix = 0; ix < sequences.size(); ++ix) {
        auto &seq = sequences[ix];
        for (auto &symbol : seq.firsts) {
            add_transition(symbol, ix);
        }
    }
}

void Rule::dump_parse_table() const
{
    for (auto entry : parse_table) {
        std::print("{}: {} => {}\n", non_terminal, entry.first, sequences[entry.second]);
    }
}

}
