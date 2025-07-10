/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <functional>
#include <string>

#include <Util/Logging.h>

#include <App/IR/IR.h>
#include <App/Operator.h>
#include <App/SyntaxNode.h>

#include <Interp/Stack.h>
#include <variant>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

using ValueAddress = intptr_t;

struct Interpreter;

void  execute_op(IR::Operation const &op, Interpreter &interpreter);
Value execute_node(Interpreter &interpreter, IR::pFunction const &function);
Value execute_node(Interpreter &interpreter, IR::pProgram const &program);
Value execute_node(Interpreter &interpreter, IR::pModule const &module);
Value execute_ir(IR::IRNode const &ir);

struct Scope {
    struct Variable {
        ValueAddress address;
        pType        type;
    };

    Scope(struct Interpreter &interpreter, IRNode const &ir, std::vector<IRVariableDeclaration> variables, std::optional<uint64_t> parent = {});
    Scope(Scope &) = default;
    Scope(Scope &&) = default;

    struct Interpreter              &interpreter;
    IRNode                           ir;
    std::optional<uint64_t>          parent {};
    std::map<std::wstring, Variable> variables {};
    size_t                           bp { 0 };

    void allocate();
    void setup();
    void release(pType const &return_type);
};

struct Interpreter {
    struct Context {
        IR::IRNode ir;
        uint64_t   ip;
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
    using CallbackPayload = std::variant<std::monostate, Operation, pFunction, IR::pModule, pType>;
    using Callback = std::function<bool(CallbackType, Interpreter &, CallbackPayload)>;

    std::map<IRNode, std::map<uint64_t, uint64_t>> labels;
    std::vector<Scope>                             scopes;
    Stack                                          stack;
    std::vector<Context>                           call_stack;
    Callback                                       callback { nullptr };

    Interpreter() = default;
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;

    void   execute_operations(IR::IRNode const &ir);
    Value  execute(IR::IRNode const &ir);
    Scope &current_scope();
    Scope &new_scope(IRNode const &ir, std::vector<IRVariableDeclaration> const &variables = {});
    Scope &emplace_scope(IRNode const &ir, std::vector<IRVariableDeclaration> const &variables = {});
    void   drop_scope(pType const &return_type);
    Value  pop(pType const &type);
};

}
