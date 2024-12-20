/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <sys/mman.h>
#include <unistd.h>
#include <variant>
#include <vector>

#include <Logging.h>
#include <Resolve.h>
#include <Result.h>
#include <Unescape.h>

#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/Execute.h>
#include <IR/Foreign.h>
#include <IR/IR.h>
#include <IR/Intrinsics.h>
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
void execute(Operation const &op, BinaryOperation const &impl, Scope &scope)
{
    scope.machine.pop(Address { AddressType::Register, 0 }, impl.lhs_type);
    scope.machine.pop(Address { AddressType::Register, 1 }, impl.rhs_type);
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
            if (auto exited = child.execute(); exited) {
                scope.ip = scope.EXITED;
                scope.push(*exited);
            } else if (impl.result_type) {
                scope.machine.push(Address { AddressType::Register, 0}, *impl.result_type);
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
        auto param = binder[p];
        auto param_type = *param.type;
        auto sz = TypeRegistry::the()[param_type].size();
        args.push_back(scope.pop(param_type));
    }
    if (scope.log) {
        std::println("{}({})", decl.name, args);
    }
    auto          ret_type = *(binder[impl.decl].type);
    auto          t = TypeRegistry::the()[ret_type];
    PrimitiveType primitive_ret_type = PrimitiveType::Void;
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
    if (impl.result_type) {
        assert(ret.type() == *impl.result_type);
        scope.machine.push(ret);
    }
}

template<>
void execute(Operation const &op, FunctionReturn const &impl, Scope &scope)
{
    if (impl.return_type) {
        scope.machine.pop(Address { AddressType::Register, 0 }, *impl.return_type);
    }
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
    auto       t = TypeRegistry::the()[impl.type];
    u64        num_elems = scope.pop<u64>();
    auto       block_size = t.size() * num_elems;
    auto       ptr = scope.machine.reserve(block_size);
    SliceValue slice { num_elems, scope.machine.stack - scope.machine.bp - ptr };
    scope.push<SliceValue>(slice);
}

template<>
void execute(Operation const &op, PopArrayElement const &impl, Scope &scope)
{
    SliceValue slice = scope.move<SliceValue>(impl.address);
    auto       ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto ix = scope.pop<u64>();
    assert(ix < slice.len);
    auto t = TypeRegistry::the()[impl.type];
    auto ptr = slice.ptr + ix * t.size();
    scope.machine.pop({ AddressType::Raw, reinterpret_cast<u64>(ptr) }, t.ref);
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
    auto       ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto ix = scope.pop<u64>();
    assert(ix < slice.len);
    auto t = TypeRegistry::the()[impl.type];
    auto ptr = slice.ptr + ix * t.size();
    scope.machine.push({ AddressType::Raw, reinterpret_cast<u64>(ptr) }, t.ref);
}

template<>
void execute(Operation const &op, PushConstant const &impl, Scope &scope)
{
    scope.push(impl.value);
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
    scope.machine.pop(Address { AddressType::Register, 0 }, impl.operand_type);
    auto res_type = scope.machine.unary_op(0, impl.operand_type, 1, impl.op);
    scope.machine.push(Address { AddressType::Register, 1 }, res_type);
}

}
