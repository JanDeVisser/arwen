/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <variant>

#include <Util/Align.h>

#include <App/IR/IR.h>
#include <App/Value.h>

#include <Interp/Interpreter.h>
#include <Interp/Stack.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

Scope::Scope(Interpreter &interpreter, pIR const &ir, std::vector<IRVariableDeclaration> variables, std::optional<uint64_t> parent)
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
    trace("Scope::allocate: bp {} stack sz {}", bp, interpreter.stack.top - bp);
}

void Scope::release()
{
    trace(L"Scope::release()");
    if (is<IR::Program>(ir) || is<IR::Module>(ir)) {
        trace("Scope::release: static Scope bp: {}", bp);
        return;
    }
    interpreter.stack.discard(bp);
}

}
