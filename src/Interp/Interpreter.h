/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <string>
#include <variant>

#include <Util/Logging.h>

#include <App/IR/IR.h>
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

    ValueStack()
    {
        stack.reserve(1024);
    }

    ValueStack(ValueStack &) = delete;
    ValueStack(ValueStack &&) = delete;

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
    uint64_t                               variables;
    IR::IRNode                             ir;
    std::optional<uint64_t>                parent {};
    std::map<std::wstring, ValueReference> values {};
    size_t                                 bp { 0 };
    size_t                                 vp { 0 };

    [[nodiscard]] ValueReference ref_of(std::wstring const &name) const;
    [[nodiscard]] Value          ptr_to(IR::VarPath const &var_path) const;
    void                         add_value(std::wstring const &name, Value const &value);
    [[nodiscard]] Value         &value(std::wstring const &name) const;
    void                         reassign(std::wstring const &name, Value const &value);
    void                         set(ValueReference ref, Value const &val) const;
    [[nodiscard]] Value         &back() const;
    [[nodiscard]] Value         &get(ValueReference const &ref) const;
    void                         allocate();
    void                         release();
};

struct Interpreter {
    struct Context {
        IR::IRNode ir;
        uint64_t   ip;
    };

    std::vector<Scope>   scopes;
    ValueStack           stack;
    std::vector<Context> call_stack;

    Interpreter() = default;
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;

    void   execute_operations(IR::IRNode const &ir);
    Value  execute(IR::IRNode const &ir);
    Scope &current_scope();
    Scope &new_scope(IR::IRNode const &ir, uint64_t variables);
    void   drop_scope();
};

void  execute_op(IR::Operation const &op, Interpreter &interpreter);
Value execute_ir(IR::IRNode const &ir);

}
