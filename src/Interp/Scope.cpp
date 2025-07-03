/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Util/Align.h"
#include <variant>

#include <App/IR/IR.h>
#include <App/Value.h>

#include <Interp/Interpreter.h>
#include <Interp/Stack.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

Scope::Scope(Interpreter &interpreter, IRNode const &ir, std::vector<IRVariableDeclaration> variables, std::optional<uint64_t> parent)
    : interpreter(interpreter)
    , ir(ir)
    , parent(parent)
{
    for (auto const &var : variables) {
        this->variables.emplace(var.name, Scope::Variable { 0, var.type });
    }
}

void Scope::allocate()
{
    bp = interpreter.stack.top;
    for (auto &[_, var] : variables) {
        var.address = interpreter.stack.reserve(var.type->size_of());
    }
    trace("Scope::allocate: bp {} stack sz {}", bp, interpreter.stack.top);
}

void Scope::setup()
{
    intptr_t depth { 0 };
    for (auto &[_, var] : variables) {
        depth += alignat(var.type->size_of(), 8);
    }
    bp = interpreter.stack.top - depth;
    auto offset { bp };
    for (auto &[_, var] : variables) {
        var.address = offset;
        offset += alignat(var.type->size_of(), 8);
    }
    trace("Scope::allocate: bp {} stack sz {}", bp, interpreter.stack.top);
}

void Scope::release(pType const &return_type)
{
    if (std::holds_alternative<IR::pProgram>(ir) || std::holds_alternative<IR::pModule>(ir)) {
        trace("Scope::release: static Scope bp: {}", bp);
        return;
    }
    interpreter.stack.discard(interpreter.stack.top - bp, return_type->size_of());
    trace("Scope::release: stack sz {}", interpreter.stack.top);
}

}
