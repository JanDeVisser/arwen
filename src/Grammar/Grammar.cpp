/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <print>
#include <string_view>

#include <Grammar/Grammar.h>
#include <Lexer/Lexer.h>
#include <Lib.h>
#include <Result.h>
#include <ScopeGuard.h>

namespace Arwen {

Error<GrammarError> Grammar::configure(std::string_view name, std::string_view value)
{
    if (iequals(name, "prefix")) {
        resolver.prefix = value;
    }
    if (iequals(name, "library")) {
        resolver.lib = value;
    }
    if (iequals(name, "lexer")) {
        std::string_view key;
        if (auto colon = value.find(':'); colon != std::string_view::npos) {
            key = value.substr(0, colon);
            value = value.substr(colon + 1);
        } else {
            key = value;
            value = {};
        }
        key = trim(key);
        value = trim(value);
        lexer.configure(key, value);
    }
    if (iequals(name, "parser")) {
        std::string_view key;
        if (auto colon = value.find(':'); colon != std::string_view::npos) {
            key = value.substr(0, colon);
            value = value.substr(colon + 1);
        } else {
            key = value;
            value = {};
        }
        key = trim(key);
        value = trim(value);
        parser_config.emplace(key, value);
    }
    return {};
}

Error<GrammarError> Grammar::build_firsts()
{
    auto reset_flags = [this]() {
        for (auto &r : rules) {
            r.second.firsts_in_progress = false;
        }
    };
    ScopeGuard sg { reset_flags };
    ssize_t    count { 0 };
    do {
        count = 0;
        reset_flags();
        for (auto &entry : rules) {
            auto   &rule = entry.second;
            ssize_t inc = TRY_EVAL(rule.update_firsts());
            count += inc;
        }
    } while (count > 0);
    return {};
}

Error<GrammarError> Grammar::build_follows()
{
    if (entry_point) {
        if (rules.contains(*entry_point)) {
            auto &rule = rules.at(*entry_point);
            rule.follows.add(Symbol::end());
        }
    }
    auto reset_flags = [this]() {
        for (auto &r : rules) {
            r.second.follows_in_progress = false;
        }
    };
    ScopeGuard sg { reset_flags };

    ssize_t count { 0 };
    do {
        count = 0;
        reset_flags();
        for (auto &entry : rules) {
            count += TRY_EVAL(entry.second.update_follows());
        }
    } while (count != 0);
    return {};
}

Error<GrammarError> Grammar::analyze()
{
    TRY(build_firsts());
    TRY(build_follows());
    TRY(check_LL1());
    return {};
}

Error<GrammarError> Grammar::check_LL1()
{
    for (auto &entry : rules) {
        TRY(entry.second.check_LL1());
    }
    return {};
}

Error<GrammarError> Grammar::build_parse_table()
{
    TRY(analyze());
    for (auto &entry : rules) {
        entry.second.build_parse_table();
    }
    return {};
}

void Grammar::dump_parse_table() const
{
    for (auto &entry : rules) {
        entry.second.dump_parse_table();
    }
}

void Grammar::dump() const
{
    for (auto &entry : rules) {
        auto &r = entry.second;
        std::println("\n{} :=", r.non_terminal);
        for (auto &seq : r.sequences) {
            std::println("    {} {} ", seq, seq.firsts);
        }
        std::println("firsts {} follows {}", r.firsts, r.follows);
        std::println("Parse table:");
        r.dump_parse_table();
    }
    std::println("");
}

void test_Build_Grammar()
{
    // program    := [ init ] statements [ done ]
    //            ;
    //
    // statements := [ stmt_start ] statement [ stmt_end ] statements
    //            |
    //            ;
    Grammar grammar;
    {
        Rule     r { grammar, "program" };
        Sequence seq { grammar };
        seq.symbols.emplace_back(GrammarAction { "init", {} });
        seq.symbols.emplace_back("statements");
        seq.symbols.emplace_back(GrammarAction { "done", {} });
        r.sequences.push_back(seq);
        grammar.rules.emplace(r.non_terminal, r);
    }

    {
        Rule r { grammar, "statements" };
        {
            Sequence seq = Sequence { grammar };
            seq.symbols.emplace_back(GrammarAction { "stmt_start", {} });
            seq.symbols.emplace_back("statement");
            seq.symbols.emplace_back(GrammarAction { "stmt_end", {} });
            seq.symbols.emplace_back("statements");
            r.sequences.push_back(seq);
        }
        {
            Sequence seq { grammar };
            r.sequences.push_back(seq);
            grammar.rules.emplace(r.non_terminal, r);
        }
    }

    std::println("\n{}", grammar);
}

Grammar build_test_grammar()
{
    //  E    := T Eopt;
    //  Eopt := '+' T Eopt | '-' T Eopt | ;
    //  T    := F Topt;
    //  Topt := '*' F Topt | '/' F Topt | ;
    //  F    := #int | '(' E ')';

    Grammar grammar;
    grammar.lexer.Number.on = true;
    grammar.lexer.Number.signed_numbers = false;
    grammar.entry_point = "E";
    grammar.add_rule("E", "T", "Eopt");
    {
        auto &rule = grammar.add_rule("Eopt", TokenKind { Symbol_Value, '+' }, "T", "Eopt");
        rule.add_sequence(TokenKind { Symbol_Value, '-' }, "T", "Eopt");
        rule.add_sequence();
    }
    grammar.add_rule("T", "F", "Topt");
    {
        auto &rule = grammar.add_rule("Topt", TokenKind { Symbol_Value, '*' }, "F", "Topt");
        rule.add_sequence(TokenKind { Symbol_Value, '/' }, "F", "Topt");
        rule.add_sequence();
    }
    {
        auto &rule = grammar.add_rule("F", TokenKind { NumberType::Int });
        rule.add_sequence(TokenKind { Symbol_Value, '(' }, "E", TokenKind { Symbol_Value, ')' });
    }
    std::println("\n{}", grammar);
    return grammar;
}

Grammar build_and_analyze()
{
    auto grammar = build_test_grammar();
    grammar.build_parse_table().must();
    return grammar;
}

void test_Firsts()
{
    Grammar grammar = build_test_grammar();
    grammar.build_firsts().must();
    for (auto &r : grammar.rules) {
        std::print("Firsts {}: {} ({})\n", r.first, r.second.firsts, r.second.firsts.size());
    }
    std::println("");
}

void test_Follows()
{
    Grammar grammar = build_test_grammar();
    grammar.build_follows().must();
    for (auto &r : grammar.rules) {
        std::print("Follows {}: {} ({})\n", r.first, r.second.follows, r.second.follows.size());
    }
    std::println("");
}

void test_Analyze()
{
    Grammar grammar = build_test_grammar();
    grammar.check_LL1().must();
    std::println("Grammar is LL1");
}

void test_Parse_Table()
{
    Grammar grammar = build_and_analyze();
    std::println("");
    grammar.dump_parse_table();
}

// test "Parse Table"
//{
//     grammar const = try build_test_grammar();
//     std.debug.print("\n", . {});
//     grammar.dump_parse_table();
// }

void tests_Grammar()
{
    test_Build_Grammar();
    test_Firsts();
    test_Follows();
    test_Analyze();
    test_Parse_Table();
}

}
