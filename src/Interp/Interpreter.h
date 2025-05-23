/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <set>
#include <string>
#include <variant>

#include <Util/Logging.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

struct ValueStack {
    std::vector<pType> type_stack {};
    std::vector<Atom>  stack {};

    void evaluate(Operator op)
    {
        assert(type_stack.size() >= 2);
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
                           type_stack.push_back(val.type);
                           this->push(atom);
                       },
                       [this, val](Atoms const &atoms) -> void {
                           type_stack.push_back(val.type);
                           for (auto const &atom : atoms) {
                               this->push(atom);
                           }
                       } },
            val.payload);
    }

    template<>
    void push(Atom const &val)
    {
        stack.emplace_back(val);
    }

    pType pop_type()
    {
        assert(!type_stack.empty());
        auto ret = type_stack.back();
        type_stack.pop_back();
        return ret;
    }

    Atom pop_atom()
    {
        assert(!stack.empty());
        auto ret = stack.back();
        stack.pop_back();
        return ret;
    }

    Value pop_value()
    {
        auto t = type_stack.back();
        switch (t->kind()) {
        case TypeKind::IntType:
        case TypeKind::FloatType:
        case TypeKind::BoolType:
            return Value { pop_type(), pop_atom() };
        case TypeKind::SliceType: {
            auto size = pop_atom();
            auto ptr = pop_atom();
            return Value { pop_type(), Atoms { ptr, size } };
        }
        case TypeKind::DynArray: {
            auto cap = pop_atom();
            auto size = pop_atom();
            auto ptr = pop_atom();
            return Value { pop_type(), Atoms { ptr, size, cap } };
        }
        default:
            UNREACHABLE();
        }
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

    void         execute(pSyntaxNode const &node);
    Value const &value(std::wstring const &name) const;
    void         reassign(std::wstring const &name, Value value);
    pSyntaxNode  name(std::wstring const &name) const;
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
