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
#include <App/Type.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

struct ValueStack {
    using Entry = std::variant<pType, Atom>;
    std::vector<Entry> stack {};

    void evaluate(Operator op)
    {
        assert(stack.size() >= 4);
        Value rhs = pop_value();
        Value lhs = pop_value();
        push(Arwen::Interpreter::evaluate(lhs, op, rhs));
    }

    template<typename T>
    void push(T const &value)
    {
        push(make_value(value));
    }

    template<>
    void push(Value const &val)
    {
        std::visit(overloads {
                       [this, val](Atom const &atom) -> void {
                           stack.emplace_back(atom);
                       },
                       [this, val](Atoms const &atoms) -> void {
                           trace(L"push({}) #atoms = {}", val.type->to_string(), atoms.size());
                           for (auto const &atom : atoms) {
                               stack.emplace_back(atom);
                           }
                       } },
            val.payload);
        stack.emplace_back(val.type);
        trace(L"push({}) #={}", val.type->to_string(), stack.size());
    }

    template<>
    void push(Atom const &val)
    {
        stack.emplace_back(val);
    }

    template<>
    void push(pType const &type)
    {
        stack.emplace_back(type);
    }

    pType pop_type()
    {
        assert(!stack.empty());
        auto const entry = stack.back();
        assert(std::holds_alternative<pType>(entry));
        stack.pop_back();
        return std::get<pType>(entry);
    }

    Atom pop_atom()
    {
        assert(!stack.empty());
        auto const entry = stack.back();
        assert(std::holds_alternative<Atom>(entry));
        stack.pop_back();
        return std::get<Atom>(entry);
    }

    Value pop_value()
    {
        assert(stack.size() >= 2);
        Value ret;
        switch (auto const t = pop_type(); t->kind()) {
        case TypeKind::IntType:
        case TypeKind::FloatType:
        case TypeKind::BoolType:
            ret = Value { t, pop_atom() };
            break;
        case TypeKind::PointerType:
            ret = Value { t, pop_atom() };
            break;
        case TypeKind::SliceType: {
            auto size = pop_atom();
            auto ptr = pop_atom();
            ret = Value { t, Atoms { ptr, size } };
        } break;
        case TypeKind::DynArray: {
            auto cap = pop_atom();
            auto size = pop_atom();
            auto ptr = pop_atom();
            ret = Value { t, Atoms { ptr, size, cap } };
        } break;
        default:
            UNREACHABLE();
        }
        trace(L"pop() -> {} #={}", ret.type->to_string(), stack.size());
        return ret;
    }
};

template<typename T>
T pop(ValueStack &stack)
{
    return as<T>(stack.pop_value());
}

struct Scope {
    struct Interpreter           *interpreter;
    Scope                        *parent { nullptr };
    pSyntaxNode                   owner;
    std::map<std::wstring, Value> values;

    void                       execute(pSyntaxNode const &node);
    [[nodiscard]] Value const &value(std::wstring const &name) const;
    void                       reassign(std::wstring const &name, Value value);
    [[nodiscard]] pSyntaxNode  name(std::wstring const &name) const;
};

struct Interpreter {
    std::vector<Scope>          scopes;
    ValueStack                  stack;
    std::optional<std::wstring> break_;
    std::optional<std::wstring> continue_;

    Interpreter();
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;

    Scope &execute(pSyntaxNode const &node);
    Scope &new_scope(pSyntaxNode const &node);
};
}
