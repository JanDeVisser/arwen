/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <string>
#include <variant>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

using ValueReference = int;

struct ValueStack {
    using EntryValue = std::variant<Value, ValueReference>;
    struct Entry {
        EntryValue   value;
        std::wstring annotation;

        [[nodiscard]] std::wstring to_string() const;
    };
    std::vector<Entry> stack {};

    [[nodiscard]] static std::wstring to_string(Entry const &e);
    void                              evaluate(Operator op);
    [[nodiscard]] size_t              size() const noexcept;
    void                              push(Value value, std::wstring annotation = L"", bool dump = true);
    void                              push(ValueReference ref, std::wstring annotation = L"", bool dump = true);
    void                              push(Entry const &entry, bool dump_it = true);
    void                              set(ValueReference index, EntryValue entry);
    void                              set(ValueReference index, Value value);
    void                              set(ValueReference index, ValueReference ref);
    Value                            &get(ValueReference index);
    void                              pop_back(int count = 1);
};

void dump(ValueStack const &stack);

template<typename T>
void push(ValueStack &stack, T const &value, std::wstring annotation = L"", bool dump = true)
{
    stack.push(make_value(value), std::move(annotation), dump);
}

template<>
inline void push(ValueStack &stack, Value const &val, std::wstring annotation, bool dump)
{
    stack.push(val, std::move(annotation), dump);
}

using ValueAddress = std::variant<std::wstring, size_t>;

struct Scope {
    struct Interpreter                    *interpreter;
    Scope                                 *parent { nullptr };
    pSyntaxNode                            owner;
    std::map<std::wstring, ValueReference> values {};
    size_t                                 bp { 0 };

    Scope(Interpreter *interpreter, pSyntaxNode owner, Scope *parent = nullptr);
    ~Scope();
    void                         execute(pSyntaxNode const &node);
    void                         execute_block(std::shared_ptr<Block> const &block);
    [[nodiscard]] ValueReference ref_of(std::wstring const &name) const;
    [[nodiscard]] Value          ptr_to(ValueReference reference) const;
    void                         add_value(std::wstring const &name, Value const &value);
    [[nodiscard]] Value         &value(std::wstring const &name) const;
    void                         reassign(std::wstring const &name, Value const &value);
    [[nodiscard]] pSyntaxNode    name(std::wstring const &name) const;
    void                         allocate();
    void                         release();
    void                         push_back(Value const &val) const;
    void                         push_back(ValueReference const ref) const;
    void                         set(ValueReference const ref, Value const &val) const;
    [[nodiscard]] Value         &back() const;
    [[nodiscard]] Value         &get(ValueReference const &ref) const;
    void                         pop_back(int count = 1) const;
};

struct Interpreter {
    std::vector<Scope>          scopes;
    ValueStack                  stack;
    std::optional<std::wstring> break_;
    std::optional<std::wstring> continue_;

    Interpreter() = default;
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;
};

}
