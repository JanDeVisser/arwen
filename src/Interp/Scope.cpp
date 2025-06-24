/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Value.h"
#include <App/IR/IR.h>
#include <Interp/Interpreter.h>
#include <variant>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

void Scope::allocate()
{
    bp = interpreter->stack.size();
    vp = interpreter->stack.size();
    for (auto ix = 0; ix < variables; ++ix) {
        push(interpreter->stack, Value {}, L"", false);
    }
    trace("Scope::allocate: bp {} vp {} stack sz {}", bp, vp, interpreter->stack.size());
}

void Scope::release()
{
    if (std::holds_alternative<IR::pProgram>(ir) || std::holds_alternative<IR::pModule>(ir)) {
        trace("Scope::release: persistant Scope bp: {}", bp);
        return;
    }
    interpreter->stack.stack.erase(interpreter->stack.stack.begin() + static_cast<ssize_t>(bp), interpreter->stack.stack.end());
    trace("Scope::release: stack sz {}", interpreter->stack.size());
}

ValueReference Scope::ref_of(std::wstring const &name) const
{
    if (values.contains(name)) {
        trace(L"ref_of({}) -> {}", name, values.at(name));
        return values.at(name);
    }
    if (parent) {
        auto &p = interpreter->scopes[*parent];
        return p.ref_of(name);
    }
    fatal(L"Variable `{}` not found", name);
}

Value Scope::ptr_to(ValueReference const reference) const
{
    Value &v = get(reference);
    return Value { TypeRegistry::the().referencing(v.type), address_of(v) };
}

Value &Scope::value(std::wstring const &name) const
{
    return get(ref_of(name));
}

void Scope::add_value(std::wstring const &name, Value const &value)
{
    values.try_emplace(name, vp++);
    if (value.type->kind() != TypeKind::VoidType) {
        set(values[name], value);
    }
}

void Scope::reassign(std::wstring const &name, Value const &value)
{
    if (values.contains(name)) {
        set(values[name], value);
        return;
    }
    if (parent) {
        auto &p = interpreter->scopes[*parent];
        return p.reassign(name, value);
    }
}

void Scope::set(ValueReference const ref, Value const &val) const
{
    interpreter->stack.set(ref, val);
}

Value &Scope::get(ValueReference const &ref) const
{
    return interpreter->stack.get(ref);
}

}
