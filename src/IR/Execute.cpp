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

#include <Logging.h>
#include <Result.h>
#include <Resolve.h>
#include <Unescape.h>
#include <Binder/Binder.h>
#include <IR/Execute.h>
#include <IR/Foreign.h>
#include <IR/IR.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen::IR {

using namespace Arwen;

template<typename Impl>
void execute(Operation const &op, Impl const &impl, Scope &scope)
{
    std::cerr << "No executor for " << typeid(Impl).name() << "\n";
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
    for (auto const &mod : scope.function.program.modules) {
        auto name = std::string { decl.name };
        if (mod.function_refs.contains(name)) {
            auto ref = mod.function_refs.at(name);
            Scope child { scope, mod.functions.at(ref), args };
            child.execute();
            return;
        }
    }
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
    auto ret_type = *(binder[impl.decl].type);
    auto t = TypeRegistry::the()[ret_type];
    assert(t.typespec.tag() == TypeKind::Primitive);
    auto ret = foreign_call(impl.name, args, t.typespec.get<TypeKind::Primitive>().type).must();
    scope.push(ret);
}

template<>
void execute(Operation const &op, Intrinsic const &impl, Scope &scope)
{
    if (impl.name == "ptr") {
        auto value = scope.pop();
        assert(TypeRegistry::the()[value.type()].decay().typespec.tag() == TypeKind::Slice);
        auto sv = value.value<std::string_view>();
        scope.push(Value { static_cast<const void *>(sv.data()) });
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

void Machine::run(std::vector<Value> const &args)
{
    for (auto &mod : program.modules) {
        if (mod.function_refs.contains("main")) {
            auto &function = mod.functions[mod.function_refs["main"]];
            Scope scope { *this, function, args };
            scope.execute();
        }
    }
}

Scope::Scope(Machine &machine, Function &function, std::vector<Value> const &args)
    : machine(machine)
    , function(function)
    , log(machine.log)
{
    initialize(args);
}

Scope::Scope(Scope &parent, Function const& function, std::vector<Value> const& args)
    : machine(parent.machine)
    , parent(&parent)
    , function(function)
    , log(parent.log)
{
    initialize(args);
}

void Scope::execute()
{
    // std::println("Running {}", function.name);
    for (auto const &op : function.ops) {
        // std::println("{}", op);
        std::visit(
            [&op, this](auto const &impl) {
                execute<std::decay_t<decltype(impl)>>(op, impl, *this);
            },
            op.op);
    }
}

void Scope::push(Value value)
{
    stack.emplace_back(std::move(value));
    dump_stack();
}

Value Scope::pop()
{
    auto value = stack.back();
    stack.pop_back();
    dump_stack();
    return value;
}

void Scope::initialize(std::vector<Value> args)
{
    auto &binder = machine.program.binder;
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
        std::println("");
    }
}

}
