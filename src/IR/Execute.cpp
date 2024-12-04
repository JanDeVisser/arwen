/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cstddef>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/Execute.h>
#include <IR/Foreign.h>
#include <IR/IR.h>
#include <Logging.h>
#include <Resolve.h>
#include <Result.h>
#include <Type/Type.h>
#include <Type/Value.h>
#include <Unescape.h>

namespace Arwen::IR {

using namespace Arwen;

template<typename Impl>
void execute(Operation const &op, Impl const &impl, Scope &scope)
{
    std::cerr << "No executor for " << typeid(Impl).name() << "\n";
}

template<>
void execute(Operation const &op, BinaryOperation const &impl, Scope &scope)
{
    auto                  lhs = scope.pop();
    auto                  rhs = scope.pop();
    BinaryOperatorMapping m { impl.op };
    auto                  ret = m(lhs, rhs)
                   .or_else([]() -> std::optional<Value> {
                       fatal("Could not apply operator");
                       return {};
                   })
                   .and_then([&](Value const &v) -> std::optional<Value> {
                       scope.push(v);
                       return v;
                   });
}

template<>
void execute(Operation const &op, Call const &impl, Scope &scope)
{
    auto               binder = scope.machine.program.binder;
    auto const        &decl = I(BoundFunction, impl.decl);
    std::vector<Value> args;
    for (auto const &p : decl.parameters) {
        args.emplace_back(std::move(scope.pop()));
    }
    if (scope.log) {
        std::println("{}({})", decl.name, args);
    }
    for (auto const &mod : scope.function.program.modules) {
        auto name = std::string { decl.name };
        if (mod.function_refs.contains(name)) {
            auto  ref = mod.function_refs.at(name);
            Scope child { scope, mod.functions.at(ref), args };
            if (auto ret = child.execute(); ret) {
                scope.push(*ret);
            }
            return;
        }
    }
}

template<>
void execute(Operation const &op, Discard const &impl, Scope &scope)
{
    scope.pop();
}

template<>
void execute(Operation const &op, ForeignCall const &impl, Scope &scope)
{
    auto               binder = scope.machine.program.binder;
    auto const        &decl = I(BoundFunction, impl.decl);
    std::vector<Value> args;
    for (auto const &p : decl.parameters) {
        args.emplace_back(std::move(scope.pop()));
    }
    if (scope.log) {
        std::println("{}({})", decl.name, args);
    }
    auto ret_type = *(binder[impl.decl].type);
    auto t = TypeRegistry::the()[ret_type];
    assert(t.typespec.tag() == TypeKind::Primitive);
    auto ret = foreign_call(impl.name, args, t.typespec.get<TypeKind::Primitive>().type).must();
    scope.push(ret);
}

template<>
void execute(Operation const &op, FunctionReturn const &impl, Scope &scope)
{
    scope.jump(scope.function.ops.size());
}

template<>
void execute(Operation const &op, Intrinsic const &impl, Scope &scope)
{
    if (impl.name == "ptr") {
        auto value = scope.pop();
        assert(TypeRegistry::the()[value.type()].decay().typespec.tag() == TypeKind::Slice);
        auto sv = value.value<std::string_view>();
        scope.push(Value { static_cast<void const *>(sv.data()) });
        return;
    }
    if (impl.name == "len") {
        auto value = scope.pop();
        assert(TypeRegistry::the()[value.type()].decay().typespec.tag() == TypeKind::Slice);
        auto sv = value.value<std::string_view>();
        scope.push(Value { static_cast<u64>(sv.length()) });
        return;
    }
}

template<>
void execute(Operation const &op, Jump const &impl, Scope &scope)
{
    scope.jump(impl.target);
}

template<>
void execute(Operation const &op, JumpF const &impl, Scope &scope)
{
    auto condition = scope.pop();
    if (!condition.as_bool()) {
        scope.jump(impl.target);
    }
}

template<>
void execute(Operation const &op, JumpT const &impl, Scope &scope)
{
    auto condition = scope.pop();
    if (condition.as_bool()) {
        scope.jump(impl.target);
    }
}

template<>
void execute(Operation const &op, MakeArray const &impl, Scope &scope)
{
    scope.push(Value(Value::make_array, impl.type, scope.pop().as_unsigned()));
}

template<>
void execute(Operation const &op, PopArrayElement const &impl, Scope &scope)
{
    auto ident = scope.pop();
    auto ix_size = scope.pop();
    assert(ix_size.as_unsigned() == 1);
    auto ix = scope.pop();
    Value &arr = scope.get(ident.value<std::string_view>());
    arr[ix.as_unsigned()] = scope.pop();
}

template<>
void execute(Operation const &op, PopVariable const &impl, Scope &scope)
{
    auto value = scope.pop();
    scope.set(impl.name, value);
}

template<>
void execute(Operation const &op, PushArrayElement const &impl, Scope &scope)
{
    auto ident = scope.pop();
    auto ix_size = scope.pop();
    assert(ix_size.as_unsigned() == 1);
    auto ix = scope.pop();
    auto &value = scope.get(ident.value<std::string_view>());
    scope.push({value.type(), value[ix.as_unsigned()]});
}

template<>
void execute(Operation const &op, PushBoolean const &impl, Scope &scope)
{
    scope.push(Value { impl.value });
}

template<>
void execute(Operation const &op, PushFloat const &impl, Scope &scope)
{
    scope.push(Value { impl.value });
}

template<>
void execute(Operation const &op, PushInt const &impl, Scope &scope)
{
    scope.push(Value { impl.value });
}

template<>
void execute(Operation const &op, PushNull const &impl, Scope &scope)
{
    scope.push(Value {});
}

template<>
void execute(Operation const &op, PushString const &impl, Scope &scope)
{
    scope.push(Value { impl.value });
}

template<>
void execute(Operation const &op, PushVariableValue const &impl, Scope &scope)
{
    auto &value = scope.variables[impl.name];
    scope.push(value);
}

std::optional<Value> Machine::run(std::vector<Value> const &args)
{
    for (auto &mod : program.modules) {
        if (mod.function_refs.contains("main")) {
            auto &function = mod.functions[mod.function_refs["main"]];
            Scope scope { *this, function, args };
            return scope.execute();
        }
    }
    fatal("No \"main\" function found");
}

Scope::Scope(Machine &machine, Function &function, std::vector<Value> const &args)
    : machine(machine)
    , function(function)
    , log(machine.log)
{
    initialize(args);
}

Scope::Scope(Scope &parent, Function const &function, std::vector<Value> const &args)
    : machine(parent.machine)
    , parent(&parent)
    , function(function)
    , log(parent.log)
{
    initialize(args);
}

void dump_vars(Scope const& scope)
{
    for (auto const &var : scope.variables) {
        std::print(" | {} = {}", var.first, var.second);
    }
    std::println("");
}

std::optional<Value> Scope::execute()
{
    ip = 0;
    while (ip < function.ops.size()) {
        auto const &op = function.ops[ip];
        if (log) std::print("     {} ", op);
        ++ip;
        std::visit(
            [&op, this](auto const &impl) {
                execute<std::decay_t<decltype(impl)>>(op, impl, *this);
            },
            op.op);
        if (log) {
            dump_stack();
            std::println(" -> {}", ip);
        }
    }
    if (!stack.empty()) {
        return stack.back();
    }
    return {};
}

void Scope::jump(size_t target)
{
    ip = target;
}

void Scope::push(Value value)
{
    stack.emplace_back(std::move(value));
}

Value Scope::pop()
{
    auto value = stack.back();
    stack.pop_back();
    return value;
}

void Scope::set(std::string_view name, Value value)
{
    Scope *scope = this;
    while (scope) {
        if (scope->variables.contains(name)) {
            scope->variables[name] = value;
            return;
        }
        scope = scope->parent;
    }
    variables[name] = value;
}

Value &Scope::get(std::string_view name)
{
    Scope *scope = this;
    while (scope) {
        if (scope->variables.contains(name)) {
            return scope->variables[name];
        }
        scope = scope->parent;
    }
    UNREACHABLE();
}

Value const &Scope::get(std::string_view name) const
{
    Scope const *scope = this;
    while (scope) {
        if (scope->variables.contains(name)) {
            return scope->variables.at(name);
        }
        scope = scope->parent;
    }
    UNREACHABLE();
}

void Scope::initialize(std::vector<Value> args)
{
    auto       &binder = machine.program.binder;
    auto const &func_node = binder[function.bound_ref];
    assert(std::holds_alternative<BoundFunction>(func_node.impl));
    auto const &bound_function = std::get<BoundFunction>(func_node.impl);
    assert(args.size() >= bound_function.parameters.size());
    for (size_t i = 0; i < bound_function.parameters.size(); i++) {
        variables[I(BoundParameter, bound_function.parameters[i]).name] = args[i];
    }
}

void Scope::dump_stack()
{
    if (log) {
        for (auto const &value : stack) {
            std::print(" | {}", value);
        }
        // std::println("");
    }
}

}
