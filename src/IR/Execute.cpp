/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cstddef>
#include <cstring>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <sys/mman.h>
#include <unistd.h>
#include <variant>
#include <vector>

#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/Execute.h>
#include <IR/Foreign.h>
#include <IR/IR.h>
#include <IR/Intrinsics.h>
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
    scope.machine.pop(Address { AddressType::Register , 0 }, impl.lhs_type);
    scope.machine.pop(Address { AddressType::Register , 1 }, impl.rhs_type);
    auto res_type = scope.machine.binary_op(0, impl.lhs_type, 1, impl.rhs_type, 2, impl.op);
    scope.machine.push(Address { AddressType::Register, 2 }, res_type);
}

template<>
void execute(Operation const &op, Call const &impl, Scope &scope)
{
    auto               binder = scope.machine.program.binder;
    auto const        &decl = I(BoundFunction, impl.decl);
    std::vector<Value> args;
    for (auto const &mod : scope.function.program.modules) {
        auto name = std::string { decl.name };
        if (mod.function_refs.contains(name)) {
            auto  ref = mod.function_refs.at(name);
            Scope child { scope, mod.functions.at(ref) };
            if (auto ret = child.execute(); ret) {
                if (scope.ip != scope.EXITED) {
                    scope.push(*ret);
                }
            }
            return;
        }
    }
}

template<>
void execute(Operation const &op, Discard const &impl, Scope &scope)
{
    scope.machine.discard(TypeRegistry::the()[impl.type].size());
}

template<>
void execute(Operation const &op, ForeignCall const &impl, Scope &scope)
{
    auto               binder = scope.machine.program.binder;
    auto const        &decl = I(BoundFunction, impl.decl);
    std::vector<Value> args;
    for (auto p : decl.parameters) {
        scope.machine.pop({AddressType::Register, 0}, *binder[p].type);
        args.emplace_back(*binder[p].type, scope.machine.make_pointer({AddressType::Register, 0}));
    }
    if (scope.log) {
        std::println("{}({})", decl.name, args);
    }
    auto          ret_type = *(binder[impl.decl].type);
    auto          t = TypeRegistry::the()[ret_type];
    PrimitiveType primitive_ret_type = PrimitiveType::Null;
    switch (t.typespec.tag()) {
    case TypeKind::Primitive:
        primitive_ret_type = t.typespec.get<TypeKind::Primitive>().type;
        break;
    case TypeKind::Pointer:
        if (t.typespec.get<TypeKind::Pointer>().element_type == U8Type) {
            primitive_ret_type = PrimitiveType::Ptr;
            break;
        }
        // Fall through
    default:
        fatal("Unsupported return type for foreign function");
        break;
    }
    auto ret = foreign_call(impl.name, args, primitive_ret_type).must();
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
    if (!scope.machine.intrinsics.contains(impl.name)) {
        fatal("Intrinsic {} not found", impl.name);
    }
    scope.machine.intrinsics[impl.name](scope);
}

template<>
void execute(Operation const &op, Jump const &impl, Scope &scope)
{
    scope.jump(impl.target);
}

template<>
void execute(Operation const &op, JumpF const &impl, Scope &scope)
{
    auto condition = scope.pop<bool>();
    if (!condition) {
        scope.jump(impl.target);
    }
}

template<>
void execute(Operation const &op, JumpT const &impl, Scope &scope)
{
    auto condition = scope.pop<bool>();
    if (condition) {
        scope.jump(impl.target);
    }
}

template<>
void execute(Operation const &op, MakeArray const &impl, Scope &scope)
{
    auto t = TypeRegistry::the()[impl.type];
    u64 num_elems = scope.pop<u64>();
    auto block_size = t.size() * num_elems;
    auto ptr = scope.machine.reserve(block_size);
    SliceValue slice { num_elems, scope.machine.stack - scope.machine.bp - ptr };
    scope.push<SliceValue>(slice);
}

template<>
void execute(Operation const &op, PopArrayElement const &impl, Scope &scope)
{
    SliceValue slice = scope.move<SliceValue>(impl.address);
    auto ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto   ix = scope.pop<u64>();
    assert(ix < slice.len);
    auto t = TypeRegistry::the()[impl.type];
    auto ptr = slice.ptr + ix * t.size();
    scope.machine.pop({AddressType::Raw, reinterpret_cast<u64>(ptr) }, t.ref);

}

template<>
void execute(Operation const &op, PopFrame const &impl, Scope &scope)
{
    scope.machine.pop_frame(impl.discard ? 0 : TypeRegistry::the()[impl.type].size());
}

template<>
void execute(Operation const &op, PopVariable const &impl, Scope &scope)
{
    scope.machine.pop(impl.address, impl.type);
}

template<>
void execute(Operation const &op, PushArrayElement const &impl, Scope &scope)
{
    SliceValue slice = scope.move<SliceValue>(impl.address);
    auto ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto   ix = scope.pop<u64>();
    assert(ix < slice.len);
    auto t = TypeRegistry::the()[impl.type];
    auto ptr = slice.ptr + ix * t.size();
    scope.machine.push({AddressType::Raw, reinterpret_cast<u64>(ptr) }, t.ref);
}

template<>
void execute(Operation const &op, PushConstant const &impl, Scope &scope)
{
    scope.push(Value { impl.value });
}

template<>
void execute(Operation const &op, PushFrame const &impl, Scope &scope)
{
    scope.machine.push_frame();
}

template<>
void execute(Operation const &op, PushNullptr const &impl, Scope &scope)
{
    scope.push(Value { PtrType });
}

template<>
void execute(Operation const &op, PushVariableValue const &impl, Scope &scope)
{
    scope.machine.push(impl.address, impl.type);
}

template<>
void execute(Operation const &op, UnaryOperation const &impl, Scope &scope)
{
    scope.machine.pop(Address { AddressType::Register , 0 }, impl.operand_type);
    auto res_type = scope.machine.unary_op(0, impl.operand_type, 1, impl.op);
    scope.machine.push(Address { AddressType::Register, 1 }, res_type);
}

Machine::Machine(Program &program)
    : program(program)
{
    intrinsics["exit"] = exit;
    intrinsics["len"] = len;
    intrinsics["make_string"] = make_string;
    intrinsics["ptr"] = Arwen::IR::ptr;
    stack = static_cast<u8*>(mmap(nullptr, STACK_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    if (stack == MAP_FAILED) {
        fatal("Could not allocate VM Memory: {}", strerror(errno));
    }
    stack += STACK_SIZE;
    bp = sp = 0;
    if (program.depth) {
        data = static_cast<u8*>(mmap(nullptr, program.depth, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
        if (data == MAP_FAILED) {
            munmap(stack - STACK_SIZE, STACK_SIZE);
            fatal("Could not allocate VM Memory: {}", strerror(errno));
        }
        data += program.depth;
    }
}

Machine::~Machine()
{
    munmap(stack - STACK_SIZE, STACK_SIZE);
    if (data) {
        munmap(data - program.depth, program.depth);
    }
}

std::optional<Value> Machine::run(std::vector<Value> const &args)
{
    auto builtin_ix = program.modules.size();
    for (auto &mod : program.modules) {
        if (mod.name == "#builtin") {
            builtin_ix = mod.ref;
            break;
        }
    }
    assert(builtin_ix < program.modules.size());
    Function dummy { program };
    Scope    root { *this, program.modules[builtin_ix].initializer };
    root.execute();
    for (auto &mod : program.modules) {
        if (mod.name == "#builtin")
            continue;
        Scope mod_scope { root, mod.initializer };
        mod_scope.execute();
        if (mod.function_refs.contains("main")) {
            auto &function = mod.functions[mod.function_refs["main"]];
            Scope scope { mod_scope, function };
            return scope.execute();
        }
    }
    fatal("No \"main\" function found");
}

Scope::Scope(Machine &machine, Function &function)
    : machine(machine)
    , function(function)
    , log(machine.log)
{
}

Scope::Scope(Scope &parent, Function const &function)
    : machine(parent.machine)
    , parent(&parent)
    , function(function)
    , log(parent.log)
{
}

std::optional<Value> Scope::execute()
{
    ip = 0;
    while (ip < function.ops.size()) {
        auto const &op = function.ops[ip];
        if (log)
            std::print("     {} ", op);
        ++ip;
        std::visit(
            [&op, this](auto const &impl) {
                execute<std::decay_t<decltype(impl)>>(op, impl, *this);
            },
            op.op);
        if (log) {
            std::println(" -> {}", (ip == EXITED) ? "EXIT" : std::to_string(ip));
        }
        if (ip == EXITED) {
            auto exit_code = pop<i32>();
            if (parent) {
                parent->push(exit_code);
                parent->ip = EXITED;
                return {};
            } else {
                return exit_code;
            }
        }
    }
    if (machine.sp > 0) {
        return pop<u64>();
    }
    return {};
}

void Scope::jump(size_t target)
{
    ip = target;
}

void Scope::exit()
{
    ip = EXITED;
}

}
