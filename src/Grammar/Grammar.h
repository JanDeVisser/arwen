/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <functional>
#include <vector>

#include <Lexer/Lexer.h>
#include <Lib.h>
#include <Resolve.h>
#include <Result.h>
#include <Set.h>
#include <Value.h>

namespace Arwen {

#define GrammarErrors(S) \
    S(ActionUnresolved)  \
    S(GrammarNotLL1)     \
    S(RuleNotFound)

enum GrammarError {
#undef S
#define S(E) E,
    GrammarErrors(S)
#undef S
};

template<>
inline std::optional<GrammarError> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return GrammarError::S;
    GrammarErrors(S)
#undef S
        return {};
}

template<>
inline std::string_view to_string(GrammarError const &v)
{
    switch (v) {
#undef S
#define S(S)              \
    case GrammarError::S: \
        return #S;
        GrammarErrors(S)
#undef S
    }
}

#define SymbolTypes(S) \
    S(Empty)           \
    S(End)             \
    S(Action)          \
    S(Terminal)        \
    S(NonTerminal)

enum class SymbolType {
#undef S
#define S(ST) ST,
    SymbolTypes(S)
#undef S
};

template<>
inline std::optional<SymbolType> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return SymbolType::S;
    SymbolTypes(S)
#undef S
        return {};
}

template<>
inline std::string_view to_string(SymbolType const &v)
{
    switch (v) {
#undef S
#define S(S)            \
    case SymbolType::S: \
        return #S;
        SymbolTypes(S)
#undef S
    }
}

struct GrammarAction {
    std::string_view     full_name {};
    std::optional<Value> data {};

    GrammarAction() = default;
    GrammarAction(std::string_view name, std::optional<Value> data)
        : full_name(name)
        , data(data)
    {
    }

    template<typename Parser, typename R=SearchingResolver>
    Result<std::function<void(Parser *, Value const *)>, GrammarError> action(R const& resolver) const
    {
        using RawAction = void(Parser *, Value const *);
        using Action = std::function<RawAction>;
        auto result = resolver.template resolve<RawAction>(full_name);
        if (result.has_value()) {
            return result.value();
        }
        return GrammarError::ActionUnresolved;
    }

    template<typename Parser, typename R=SearchingResolver>
    Error<GrammarError> call(Parser &parser) const
    {
        auto         fnc = TRY_EVAL(action<Parser>(parser.grammar.resolver));
        Value const *d = (data) ? &data.value() : nullptr;
        fnc(&parser, d);
        return {};
    }

    [[nodiscard]] bool operator<(GrammarAction const &rhs) const
    {
        if (full_name != rhs.full_name) {
            return full_name < rhs.full_name;
        }
        if (!rhs.data) {
            return false;
        } else if (!data) {
            return true;
        } else {
            return *data < rhs.data;
        }
    }

    [[nodiscard]] bool operator==(GrammarAction const &rhs) const
    {
        return !(*this < rhs) && !(rhs < *this);
    }
};

struct Symbol;
using Symbols = std::vector<Symbol>;
using SymbolSet = Set<Symbol>;

struct Symbol {
    using Iterator = Symbols::iterator;

    Symbol()
        : Symbol { SymbolType::Empty }
    {
    }

    static Symbol end()
    {
        return Symbol { SymbolType::End };
    }

    explicit Symbol(GrammarAction const &action)
        : Symbol { SymbolType::Action }
    {
        symbol = {
            .action = action,
        };
    }

    [[nodiscard]] GrammarAction const &action() const
    {
        assert(m_type == SymbolType::Action);
        return symbol.action;
    }

    explicit Symbol(TokenKind const &terminal)
        : Symbol { SymbolType::Terminal }
    {
        symbol = {
            .terminal = terminal,
        };
    }

    [[nodiscard]] TokenKind const &terminal() const
    {
        assert(m_type == SymbolType::Terminal);
        return symbol.terminal;
    }

    explicit Symbol(std::string_view const &non_terminal)
        : Symbol { SymbolType::NonTerminal }
    {
        symbol = {
            .non_terminal = non_terminal,
        };
    }

    [[nodiscard]] std::string_view const &non_terminal() const
    {
        assert(m_type == SymbolType::NonTerminal);
        return symbol.non_terminal;
    }

    [[nodiscard]] SymbolType type() const
    {
        return m_type;
    }

    [[nodiscard]] bool operator<(Symbol const &rhs) const;

    static Result<ssize_t, GrammarError> firsts(Symbols const& symbols, size_t ix,
        struct Grammar &grammar, SymbolSet &f);

private:
    SymbolType m_type { SymbolType::Empty };
    union {
        bool             dummy { false };
        GrammarAction    action;
        TokenKind        terminal;
        std::string_view non_terminal;
    } symbol;

    explicit Symbol(SymbolType type)
        : m_type(type)
    {
    }
};

struct Sequence {
    using Iterator = std::vector<Sequence>::iterator;

    struct Grammar &grammar;
    Symbols         symbols {};
    SymbolSet       firsts {};

    explicit Sequence(Grammar &grammar)
        : grammar(grammar)
    {
    }

    Result<ssize_t, GrammarError> build_firsts();

    template<typename... Args>
    void add_symbols(Symbol const &sym, Args &&...syms)
    {
        symbols.push_back(sym);
        if constexpr (sizeof...(syms) > 0) {
            add_symbols(syms...);
        }
    }

    template<typename... Args>
    void add_symbols(std::string_view nt, Args &&...syms)
    {
        add_symbols(Symbol { nt }, syms...);
    }

    template<typename... Args>
    void add_symbols(GrammarAction const &action, Args &&...syms)
    {
        add_symbols(Symbol { action }, syms...);
    }

    template<typename... Args>
    void add_symbols(TokenKind const &token, Args &&...syms)
    {
        add_symbols(Symbol { token }, syms...);
    }

    Result<std::optional<int64_t>, GrammarError>
    check_LL1(SymbolSet f_i, Iterator tail_begin, Iterator tail_end, size_t j);
};

struct Rule {
    struct Grammar          &grammar;
    std::string_view         non_terminal;
    std::vector<Sequence>    sequences {};
    std::map<Symbol, size_t> parse_table {};
    SymbolSet         firsts {};
    SymbolSet         follows {};
    bool                     firsts_in_progress { false };
    bool                     follows_in_progress { false };

    Rule(Grammar &grammar, std::string_view non_terminal)
        : grammar(grammar)
        , non_terminal(non_terminal)
    {
    }

    template<typename... Args>
    void add_sequence(Args &&...symbols)
    {
        sequences.emplace_back(grammar);
        auto &seq = sequences.back();
        if constexpr (sizeof...(symbols) > 0) {
            seq.add_symbols(symbols...);
        }
    }

    Result<ssize_t, GrammarError> update_firsts();
    Result<ssize_t, GrammarError> update_follows();
    Error<GrammarError>           check_LL1();
    void                          add_transition(Symbol const &symbol, size_t ix);
    void                          build_parse_table();
    void                          dump_parse_table() const;
};

struct Grammar {
    Config                                       lexer {};
    SearchingResolver                            resolver {};
    std::map<std::string_view, Rule>             rules {};
    std::optional<std::string_view>              entry_point {};
    std::map<std::string_view, std::string_view> parser_config {};
    std::optional<std::string_view>              build_func = {};

    template<typename... Args>
    inline Rule &add_rule(std::string_view nt, Args &&...symbols)
    {
        rules.emplace(nt, Rule { *this, nt });
        auto &ret = rules.at(nt);
        if constexpr (sizeof...(symbols) > 0) {
            ret.add_sequence(symbols...);
        }
        return rules.at(nt);
    }

    Error<GrammarError> configure(std::string_view name, std::string_view value);
    Error<GrammarError> build_firsts();
    Error<GrammarError> build_follows();
    Error<GrammarError> analyze();
    Error<GrammarError> check_LL1();
    Error<GrammarError> build_parse_table();
    void                dump_parse_table() const;
    void                dump() const;
};

}

template<typename T>
struct std::formatter<Arwen::Set<T>, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Set<T> s, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("{}", s.set);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::SymbolType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::SymbolType s, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(s);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Symbol, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Symbol const &s, FmtContext &ctx) const
    {
        std::ostringstream out;
        switch (s.type()) {
        case Arwen::SymbolType::Empty:
            out << "ε";
            break;
        case Arwen::SymbolType::End:
            out << "☐";
            break;
        case Arwen::SymbolType::Action:
            out << "[ " << s.action().full_name << " ]";
            break;
        case Arwen::SymbolType::Terminal:
            out << std::format("{}", s.terminal());
            break;
        case Arwen::SymbolType::NonTerminal:
            out << s.non_terminal();
            break;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Sequence, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Sequence const &s, FmtContext &ctx) const
    {
        std::ostringstream out;
        if (s.symbols.empty()) {
            out << " ε";
        } else {
            for (auto &sym : s.symbols) {
                out << " " << std::format("{}", sym);
            }
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Rule, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Rule const &r, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << r.non_terminal << " :=";
        auto first { true };
        for (auto &seq : r.sequences) {
            if (!first) {
                out << '|';
            }
            first = false;
            out << std::format("{} ", seq);
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Grammar, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Grammar const &grammar, FmtContext &ctx) const
    {
        std::ostringstream out;
        for (auto &rule : grammar.rules) {
            out << std::format("{}\n", rule.second);
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
