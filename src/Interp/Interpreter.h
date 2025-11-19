/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <functional>
#include <string>
#include <variant>

#include <Util/Logging.h>

#include <App/IR/IR.h>
#include <App/Operator.h>
#include <App/SyntaxNode.h>

#include <Interp/Stack.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

using ValueAddress = intptr_t;

struct Interpreter;

void  execute_op(IR::Operation const &op, Interpreter &interpreter);
Value execute_node(Interpreter &interpreter, pIR const &ir, IR::Function const &function);
Value execute_node(Interpreter &interpreter, pIR const ir, IR::Program const &program);
Value execute_node(Interpreter &interpreter, pIR const &ir, IR::Module const &module);
Value execute_ir(IRNodes const &ir);

struct Scope {
    struct Variable {
        ValueAddress address;
        pType        type;
    };

    Scope(struct Interpreter &interpreter, pIR const &ir, IR::Declarations variables, std::optional<uint64_t> parent = {});
    Scope(Scope &) = default;
    Scope(Scope &&) = default;

    struct Interpreter              &interpreter;
    pIR                              ir;
    std::optional<uint64_t>          parent {};
    std::map<std::wstring, Variable> variables {};
    size_t                           bp { 0 };

    void allocate();
    void setup();
    void release();
};

struct Interpreter {
    struct Context {
        pIR const ir;
        uint64_t  ip;

        Context(pIR const &ir, int64_t ip)
            : ir(ir)
            , ip(ip)
        {
        }
    };

    enum class CallbackType {
        StartModule,
        EndModule,
        StartFunction,
        EndFunction,
        BeforeOperation,
        AfterOperation,
        OnScopeStart,
        AfterScopeStart,
        OnScopeDrop,
        AfterScopeDrop,
    };
    using CallbackPayload = std::variant<std::monostate, Operation, pIR, pType>;
    using Callback = std::function<bool(CallbackType, Interpreter &, CallbackPayload)>;

    std::map<pIR, std::map<uint64_t, uint64_t>> labels;
    std::vector<Scope>                          scopes;
    Stack                                       stack;
    std::vector<Context>                        call_stack;
    std::array<uint64_t, 20>                    registers;
    Callback                                    callback { nullptr };

    Interpreter() = default;
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;

    void     execute_operations(pIR const &ir);
    Value    execute(pIR const &ir);
    Scope   &current_scope();
    Scope   &new_scope(pIR const &ir, IR::Declarations const &variables = {});
    Scope   &emplace_scope(pIR const &ir, IR::Declarations const &variables = {});
    void     drop_scope();
    void     move_in(void *ptr, size_t size, uint8_t reg = 0);
    uint64_t move_out(uint8_t reg = 0);
    Value    move_out(pType const &type, uint8_t reg = 0);
    Value    pop(pType const &type);

    template<typename T>
    void move_in(T value, uint8_t reg = 0)
    {
        move_in(&value, sizeof(T), reg);
    }

    template<>
    inline void move_in(Value val, uint8_t reg)
    {
        std::visit(
            overloads {
                [reg, &val, this](IntType const &descr) -> void {
                    switch (descr.width_bits) {
                    case 8:
                        if (descr.is_signed) {
                            move_in<int8_t>(as<int8_t>(val), reg);
                        } else {
                            move_in<uint8_t>(as<uint8_t>(val), reg);
                        }
                        break;
                    case 16:
                        if (descr.is_signed) {
                            move_in<int16_t>(as<int16_t>(val), reg);
                        } else {
                            move_in<uint16_t>(as<uint32_t>(val), reg);
                        }
                        break;
                    case 32:
                        if (descr.is_signed) {
                            move_in<int32_t>(as<int32_t>(val), reg);
                        } else {
                            move_in<uint32_t>(as<uint32_t>(val), reg);
                        }
                        break;
                    case 64:
                        if (descr.is_signed) {
                            move_in<int64_t>(as<int64_t>(val), reg);
                        } else {
                            move_in<uint64_t>(as<uint64_t>(val), reg);
                        }
                        break;
                    default:
                        UNREACHABLE();
                    }
                },
                [&val, reg, this](FloatType const &descr) -> void {
                    switch (descr.width_bits) {
                    case 32:
                        move_in<float>(as<float>(val), reg);
                        break;
                    case 64:
                        move_in<double>(as<double>(val), reg);
                        break;
                    default:
                        UNREACHABLE();
                    }
                },
                [&val, reg, this](BoolType const &) -> void {
                    move_in<bool>(as<bool>(val), reg);
                },
                [&val, reg, this](SliceType const &) -> void {
                    move_in<Slice>(as<Slice>(val), reg);
                },
                [&val, reg, this](VoidType const &) -> void {
                    move_in<Void>(Void {}, reg);
                },
                [](auto const &) -> void {
                    NYI("push<Value>");
                } },
            val.type->description);
    }
};
}
