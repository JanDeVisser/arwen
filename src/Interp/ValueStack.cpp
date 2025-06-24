/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Interp/Interpreter.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

void dump(ValueStack const &stack)
{
    int ix = 0;
    trace("");
    for (auto const &e : stack.stack) {
        trace(L"{:4}.  {:40.40} {:15.15} {:016x}", ix++, e.to_string(), e.annotation, reinterpret_cast<uint64_t>(&e));
    }
    trace("-- {} -----------------------------------------------", stack.size());
}

std::wstring ValueStack::Entry::to_string() const
{
    return std::visit(overloads {
                          [](ValueReference const &ref) -> std::wstring {
                              return std::format(L"&{}", ref);
                          },
                          [](Value const &value) -> std::wstring {
                              return std::format(L"{}:{}", value.to_string(), value.type->to_string());
                          } },
        value);
}

void ValueStack::evaluate(Operator op)
{
    assert(stack.size() >= 2);
    Value const &rhs = get(-1);
    Value const &lhs = get(-2);
    Value        res = Arwen::evaluate(lhs, op, rhs);
    pop_back(2);
    push(res);
}

size_t ValueStack::size() const noexcept
{
    return stack.size();
}

void ValueStack::push(Value value, std::wstring annotation, bool dump)
{
    push(Entry { std::move(value), std::move(annotation) }, dump);
}

void ValueStack::push(ValueReference ref, std::wstring annotation, bool dump)
{
    push(Entry { ref, std::move(annotation) }, dump);
}

void ValueStack::push(Entry const &entry, bool const dump_it)
{
    stack.emplace_back(entry);
    if (dump_it) {
        dump(*this);
    }
}

void ValueStack::set(ValueReference index, EntryValue entry)
{
    assert(!stack.empty());
    if (index < 0) {
        index = static_cast<int>(stack.size()) + index;
    }
    assert(index >= 0 && index < stack.size());
    std::swap(stack[index].value, entry);
    dump(*this);
}

void ValueStack::set(ValueReference const index, Value value)
{
    set(index, EntryValue { std::move(value) });
}

void ValueStack::set(ValueReference const index, ValueReference const ref)
{
    set(index, EntryValue { ref });
}

Value &ValueStack::get(ValueReference index)
{
    assert(!stack.empty());
    if (index < 0) {
        index = static_cast<int>(stack.size()) + index;
    }
    assert(index >= 0 && index < stack.size());
    if (stack[index].annotation.empty()) {
        trace(L"get({}) -> {}", index, stack[index].to_string());
    } else {
        trace(L"get({}:{}) -> {}", index, stack[index].annotation, stack[index].to_string());
    }
    return std::visit(overloads {
                          [index](Value &val) -> Value & {
                              return val;
                          },
                          [this, index](ValueReference const &ref) -> Value & {
                              return get(ref);
                          } },
        stack[index].value);
}

void ValueStack::pop_back(int count)
{
    assert(stack.size() >= count);
    while (count-- > 0) {
        stack.pop_back();
    }
    dump(*this);
}

}
