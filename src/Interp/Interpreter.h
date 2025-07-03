/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <string>

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

    std::vector<Scope>   scopes;
    Stack                stack;
    std::vector<Context> call_stack;

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

void  execute_op(IR::Operation const &op, Interpreter &interpreter);
Value execute_ir(IR::IRNode const &ir);

}
