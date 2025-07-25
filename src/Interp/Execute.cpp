/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <limits>
#include <memory>
#include <variant>

#include <Util/Align.h>
#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>
#include <Interp/Native.h>
#include <Interp/Stack.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

template<typename OpImpl>
void execute_op(Interpreter &interpreter, OpImpl const &impl)
{
    if constexpr (std::is_same_v<OpImpl, std::monostate>) {
        trace("execute_op(std::monostate)");
    } else {
        trace("execute_op({})", OpImpl::type);
    }
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::AssignFromRef const &impl)
{
    auto const var_ref = pop<uint64_t>(interpreter.stack);
    auto const val_ref = pop<uint64_t>(interpreter.stack);
    interpreter.stack.copy(var_ref, val_ref, impl.payload->size_of());
    push<uint64_t>(interpreter.stack, var_ref);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::AssignValue const &impl)
{
    auto const var_ref = pop<uint64_t>(interpreter.stack);
    interpreter.stack.copy_and_pop(var_ref, impl.payload->size_of());
    push<uint64_t>(interpreter.stack, var_ref);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::BinaryOperator const &impl)
{
    interpreter.stack.evaluate(impl.payload.lhs, impl.payload.op, impl.payload.rhs);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Call const &impl)
{
    auto find_func = [&impl](Scope &s) -> IRNode {
        return std::visit(
            overloads {
                [&impl](IR::pModule &mod) -> IRNode {
                    if (mod->functions.contains(impl.payload.name)) {
                        return mod->functions[impl.payload.name];
                    }
                    if (mod->program->functions.contains(impl.payload.name)) {
                        return mod->program->functions[impl.payload.name];
                    }
                    return {};
                },
                [&impl](IR::pProgram &prog) -> IRNode {
                    if (prog->functions.contains(impl.payload.name)) {
                        return prog->functions[impl.payload.name];
                    }
                    return {};
                },
                [](auto &) -> IRNode {
                    return {};
                },
            },
            s.ir);
    };

    auto scope_ix = interpreter.scopes.size() - 1;
    while (true) {
        Scope &s { interpreter.scopes[scope_ix] };
        if (auto const &f = find_func(s); std::holds_alternative<IR::pFunction>(f)) {
            auto const &ret = std::get<IR::pFunction>(f)->return_type;
            intptr_t    depth { 0 };
            for (auto const &param : impl.payload.parameters) {
                depth += alignat(param.type->size_of(), 8);
            }
            Scope &param_scope = interpreter.emplace_scope(f, impl.payload.parameters);
            interpreter.call_stack.emplace_back(f, 0);
            interpreter.execute_operations(f);
            interpreter.call_stack.pop_back();
            interpreter.drop_scope();
            auto return_value { make_from_buffer(impl.payload.return_type, interpreter.registers.data()) };
            push(interpreter.stack, return_value);
            interpreter.call_stack.back().ip++;
            return;
        }
        if (!s.parent) {
            break;
        }
        scope_ix = *s.parent;
    }
    UNREACHABLE();
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::DeclVar const &impl)
{
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Dereference const &impl)
{
    auto const ref = pop<uint64_t>(interpreter.stack);
    interpreter.stack.push_copy(ref, impl.payload->size_of());
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Discard const &impl)
{
    interpreter.stack.discard(alignat(impl.payload->size_of(), 8));
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Jump const &impl)
{
    auto &ctx { interpreter.call_stack.back() };
    assert(interpreter.labels.contains(ctx.ir));
    auto ip { interpreter.labels[ctx.ir][impl.payload] };
    interpreter.call_stack.back().ip = ip;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::JumpF const &impl)
{
    if (!pop<bool>(interpreter.stack)) {
        auto &ctx { interpreter.call_stack.back() };
        assert(interpreter.labels.contains(ctx.ir));
        auto ip { interpreter.labels[ctx.ir][impl.payload] };
        interpreter.call_stack.back().ip = ip;
        return;
    }
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::JumpT const &impl)
{
    if (pop<bool>(interpreter.stack)) {
        auto &ctx { interpreter.call_stack.back() };
        assert(interpreter.labels.contains(ctx.ir));
        auto ip { interpreter.labels[ctx.ir][impl.payload] };
        interpreter.call_stack.back().ip = ip;
        return;
    }
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Label const &)
{
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::NativeCall const &impl)
{
    intptr_t           depth { 0 };
    std::vector<pType> types;
    for (auto const &param : impl.payload.parameters) {
        depth += alignat(param.type->size_of(), 8);
        types.push_back(param.type);
    }
    void *ptr = interpreter.stack.stack + (interpreter.stack.top - depth);
    if (native_call(as_utf8(impl.payload.name), ptr, types, interpreter.registers.data(), impl.payload.return_type)) {
        auto return_value = make_from_buffer(impl.payload.return_type, interpreter.registers.data());
        interpreter.stack.discard(depth);
        push(interpreter.stack, return_value);
        interpreter.call_stack.back().ip++;
        return;
    }
    fatal(L"Error executing native function `{}`", impl.payload.name);
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Pop const &impl)
{
    Value return_value { interpreter.pop(impl.payload) };
    interpreter.move_in(return_value);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PushConstant const &impl)
{
    push<Value>(interpreter.stack, impl.payload);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PushValue const &impl)
{
    interpreter.stack.push_copy(interpreter.current_scope().variables[impl.payload.name].address + impl.payload.offset, impl.payload.type->size_of());
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PushVarAddress const &impl)
{
    push<uint64_t>(interpreter.stack, interpreter.current_scope().variables[impl.payload.name].address + impl.payload.offset);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Return const &impl)
{
    interpreter.call_stack.back().ip = std::numeric_limits<uint64_t>::max();
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::ScopeBegin const &impl)
{
    interpreter.new_scope(IRNode {}, impl.payload);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::ScopeEnd const &impl)
{
    interpreter.drop_scope();
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Sub const &impl)
{
    auto &ctx { interpreter.call_stack.back() };
    ctx.ip += 1;
    assert(interpreter.labels.contains(ctx.ir));
    auto ip { interpreter.labels[ctx.ir][impl.payload] };
    interpreter.call_stack.emplace_back(ctx.ir, ip);
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::SubRet const &impl)
{
    interpreter.call_stack.pop_back();
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::UnaryOperator const &impl)
{
    interpreter.stack.evaluate_unary(impl.payload.operand, impl.payload.op);
    interpreter.call_stack.back().ip++;
}

void execute_op(Operation const &op, Interpreter &interpreter)
{
    trace("Executing {} ip {}", op.type_name(), interpreter.call_stack.back().ip);
    if (interpreter.callback != nullptr
        && !interpreter.callback(Interpreter::CallbackType::BeforeOperation, interpreter, op)) {
        return;
    }
    std::visit(
        [&interpreter]<typename Op>(Op const &payload) -> void {
            return execute_op<Op>(interpreter, payload);
        },
        op.op);
    if (interpreter.callback != nullptr
        && !interpreter.callback(Interpreter::CallbackType::AfterOperation, interpreter, op)) {
        return;
    }
}

}
