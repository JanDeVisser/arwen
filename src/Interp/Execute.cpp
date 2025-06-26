/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <limits>
#include <memory>
#include <ranges>
#include <variant>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>
#include <Interp/Native.h>

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
void execute_op<>(Interpreter &interpreter, Operation::Assign const &)
{
    auto const &ref = interpreter.stack.get(-1);
    auto const &val = interpreter.stack.get(-2);
    interpreter.stack.set(as<int>(ref), val);
    interpreter.stack.pop_back(2);
    interpreter.stack.push(val);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::BinaryOperator const &impl)
{
    auto const &rhs = interpreter.stack.get(-1);
    auto const &lhs = interpreter.stack.get(-2);
    auto const  res = evaluate(lhs, impl.payload.op, rhs);
    interpreter.stack.pop_back(2);
    interpreter.stack.push(res);
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
                    return {};
                },
                [](auto &) -> IRNode {
                    return {};
                },
            },
            s.ir);
    };

    auto &s = interpreter.current_scope();
    while (true) {
        if (auto const &f = find_func(s); std::holds_alternative<IR::pFunction>(f)) {
            std::vector<Value> args;
            for (auto ix = 0; ix < impl.payload.parameters.size(); ++ix) {
                args.emplace_back(interpreter.stack.get(-1 - ix));
            }
            interpreter.stack.pop_back(static_cast<int>(args.size()));
            Scope &param_scope = interpreter.new_scope(f, impl.payload.parameters.size());
            for (auto const &[param_def, arg] : std::views::zip(impl.payload.parameters, args)) {
                param_scope.add_value(param_def.name, arg);
            }
            interpreter.call_stack.emplace_back(f, 0);
            Value ret { interpreter.execute(f) };
            interpreter.call_stack.pop_back();
            interpreter.drop_scope();
            interpreter.stack.push(ret);
            interpreter.call_stack.back().ip++;
            return;
        }
        if (!s.parent) {
            break;
        }
        s = interpreter.scopes[*s.parent];
    }
    UNREACHABLE();
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Cast const &impl)
{
    auto const &lhs = interpreter.stack.get(-1);
    auto const  rhs_type = impl.payload;
    assert(rhs_type != nullptr);
    if (auto const coerced_maybe = lhs.coerce(rhs_type); coerced_maybe) {
        interpreter.stack.pop_back();
        interpreter.stack.push(coerced_maybe.value());
        interpreter.call_stack.back().ip++;
        return;
    }
    fatal(L"Could not convert value of type `{}` to `{}`", lhs.type->to_string(), rhs_type->to_string());
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::DeclVar const &impl)
{
    auto &scope = interpreter.current_scope();
    scope.add_value(impl.payload.name, Value { impl.payload.type });
    interpreter.stack.push(scope.value(impl.payload.name));
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Discard const &)
{
    interpreter.stack.pop_back();
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Jump const &impl)
{
    interpreter.call_stack.back().ip = impl.payload;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::JumpF const &impl)
{
    if (auto const cond { interpreter.stack.get(-1) }; !as<bool>(cond)) {
        interpreter.call_stack.back().ip = impl.payload;
        return;
    }
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::JumpT const &impl)
{
    if (auto const cond { interpreter.stack.get(-1) }; as<bool>(cond)) {
        interpreter.call_stack.back().ip = impl.payload;
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
    std::vector<Value> native_args;
    for (auto ix = 0; ix < impl.payload.parameters.size(); ++ix) {
        native_args.emplace_back(interpreter.stack.get(-1 - ix));
    }
    interpreter.stack.pop_back(static_cast<int>(native_args.size()));
    if (auto const ret = native_call(as_utf8(impl.payload.name), native_args, impl.payload.return_type); ret) {
        interpreter.stack.push(*ret);
        interpreter.call_stack.back().ip++;
        return;
    }
    fatal(L"Error executing native function `{}`", impl.payload.name);
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PushConstant const &impl)
{
    interpreter.stack.push(impl.payload);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PushValue const &impl)
{
    auto const ref = interpreter.current_scope().ref_of(impl.payload);
    interpreter.stack.push(interpreter.current_scope().get(ref));
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::PusxhVarAddress const &impl)
{
    interpreter.stack.push(interpreter.current_scope().ptr_to(impl.payload));
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Return const &)
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
void execute_op<>(Interpreter &interpreter, Operation::ScopeEnd const &)
{
    Value ret { interpreter.stack.get(-1) };
    interpreter.drop_scope();
    interpreter.stack.push(ret);
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::Sub const &impl)
{
    auto &ctx { interpreter.call_stack.back() };
    ctx.ip += 1;
    interpreter.call_stack.emplace_back(ctx.ir, as<uint64_t>(impl.payload));
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::SubRet const &impl)
{
    interpreter.call_stack.pop_back();
    interpreter.call_stack.back().ip++;
}

template<>
void execute_op<>(Interpreter &interpreter, Operation::UnaryOperator const &impl)
{
    auto const &operand = interpreter.stack.get(-1);
    auto const  res = evaluate(operand, impl.payload.op, Value {});
    interpreter.stack.pop_back();
    interpreter.stack.push(res);
    interpreter.call_stack.back().ip++;
}

void execute_op(Operation const &op, Interpreter &interpreter)
{
    trace("Executing {} ip {}", op.type_name(), interpreter.call_stack.back().ip);
    return std::visit(
        [&interpreter]<typename Op>(Op const &payload) -> void {
            return execute_op<Op>(interpreter, payload);
        },
        op.op);
}

}
