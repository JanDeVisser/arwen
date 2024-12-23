/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cstring>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <sys/mman.h>
#include <unistd.h>
#include <utility>
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

template<typename... Args>
static void exec_log(Scope const &scope, std::format_string<Args...> fmt, Args &&...args)
{
    if (scope.log) {
        std::print("0x{:02x} ", scope.machine.sp);
        std::println(fmt, args...);
    }
}

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
    if (scope.log) {
        auto lhs_ptr = scope.machine.make_pointer(Address { AddressType::Register, 0 });
        auto rhs_ptr = scope.machine.make_pointer(Address { AddressType::Register, 1 });
        auto res_ptr = scope.machine.make_pointer(Address { AddressType::Register, 2 });
        auto lhs = Value { impl.lhs_type, lhs_ptr };
        auto rhs = Value { impl.rhs_type, rhs_ptr };
        auto res = Value { res_type, res_ptr };
        exec_log(scope, "{} {} {} = {}", lhs, impl.op, rhs, res);
    }
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
            exec_log(scope, "Calling {}", decl.name);
            if (auto exited = child.execute(); exited) {
                scope.ip = scope.EXITED;
                scope.push(*exited);
            } else if (impl.result_type) {
                if (scope.log) {
                    auto res_ptr = scope.machine.make_pointer(Address { AddressType::Register, 0 });
                    auto res = Value { *impl.result_type, res_ptr };
                    std::print("{:66}", "");
                    exec_log(scope, "{}() => {}", decl.name, res);
                }
                scope.machine.push(Address { AddressType::Register, 0 }, *impl.result_type);
            } else if (scope.log) {
                std::print("{:66}", "");
                exec_log(scope, "{}() =|", decl.name);
            }
            return;
        }
    }
}

template<>
void execute(Operation const &op, Discard const &impl, Scope &scope)
{
    auto sz = TypeRegistry::the()[impl.type].size();
    scope.machine.discard(sz);
    exec_log(scope, "Discarded {} bytes", sz);
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
        // assert(ret.type() == *impl.result_type);
        scope.machine.push(ret);
        exec_log(scope, "{}({}) => {}", decl.name, args, ret);
    } else if (scope.log) {
        exec_log(scope, "{}({}) =|", decl.name, args);
    }
}

template<>
void execute(Operation const &op, FunctionReturn const &impl, Scope &scope)
{
    if (impl.return_type) {
        scope.machine.pop(Address { AddressType::Register, 0 }, *impl.return_type);
        auto v = Value {
            *impl.return_type,
            scope.machine.make_pointer(Address { AddressType::Register, 0 })
        };
        exec_log(scope, "{}", v);
    } else if (scope.log) {
        exec_log(scope, "");
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
    exec_log(scope, "{}", impl.name);
}

template<>
void execute(Operation const &op, Jump const &impl, Scope &scope)
{
    scope.jump(impl.target);
    exec_log(scope, "Jumping to {}", impl.target);
}

template<>
void execute(Operation const &op, JumpF const &impl, Scope &scope)
{
    auto condition = scope.pop<bool>();
    if (!condition) {
        scope.jump(impl.target);
        exec_log(scope, "Condition false. Jumping to {}", impl.target);
    } else {
        exec_log(scope, "Condition true. Continuing");
    }
}

template<>
void execute(Operation const &op, JumpT const &impl, Scope &scope)
{
    auto condition = scope.pop<bool>();
    if (condition) {
        scope.jump(impl.target);
        exec_log(scope, "Condition true. Jumping to {}", impl.target);
    } else {
        exec_log(scope, "Condition false. Continuing");
    }
}

template<>
void execute(Operation const &op, MakeArray const &impl, Scope &scope)
{
    auto       t = TypeRegistry::the()[impl.type];
    u64        num_elems = scope.pop<u64>();
    auto       block_size = t.size() * num_elems;
    auto       ptr = scope.machine.reserve(block_size);
    memset(scope.machine.stack - scope.machine.bp - ptr, 0, block_size);
    SliceValue slice { num_elems, scope.machine.stack - scope.machine.bp - ptr };
    scope.push<SliceValue>(slice);
    exec_log(scope, "{}[{}]", t.name, num_elems);
}

template<>
void execute(Operation const &op, PopArrayElement const &impl, Scope &scope)
{
    auto ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto ix = scope.pop<u64>();

    auto type = TypeRegistry::the()[impl.type];
    auto [elem_type, void_p] = std::visit(
        overload {
            [&](Array const &arr) -> std::pair<TypeReference, void *> {
                SliceValue slice_value = scope.move<SliceValue>(impl.address);
                assert(ix < slice_value.len);
                return { arr.element_type, slice_value.ptr };
            },
            [&](Slice const &slice) -> std::pair<TypeReference, void *> {
                SliceValue slice_value = scope.move<SliceValue>(impl.address);
                assert(ix < slice_value.len);
                return { slice.element_type, slice_value.ptr };
            },
            [&](Pointer const &ptr) -> std::pair<TypeReference, void *> {
                return { ptr.element_type, scope.move<void *>(impl.address) };
            },
            [&](auto const &) -> std::pair<TypeReference, void *> {
                UNREACHABLE();
                return { VoidType, nullptr };
            },
        },
        type.typespec.payload());
    auto ptr = reinterpret_cast<u8 *>(void_p);
    auto t = TypeRegistry::the()[elem_type];
    ptr += ix * t.size();
    scope.machine.pop({ AddressType::Raw, reinterpret_cast<u64>(ptr) }, t.ref);
    if (scope.log) {
        auto v = Value { t.ref, ptr };
        exec_log(scope, "{}[{}] = {}", t.name, ix, v);
    }
}

template<>
void execute(Operation const &op, PopVariable const &impl, Scope &scope)
{
    auto t = TypeRegistry::the()[impl.type];
    scope.machine.pop(impl.address, t.ref);
    if (scope.log) {
        auto v = Value { t.ref, scope.machine.make_pointer(impl.address) };
        exec_log(scope, "{} = {}", t.name, v);
    }
}

template<>
void execute(Operation const &op, PushArrayElement const &impl, Scope &scope)
{
    auto ix_size = scope.pop<u64>();
    assert(ix_size == 1);
    auto ix = scope.pop<u64>();

    auto type = TypeRegistry::the()[impl.type];
    auto [elem_type, void_p] = std::visit(
        overload {
            [&](Array const &arr) -> std::pair<TypeReference, void *> {
                SliceValue slice_value = scope.move<SliceValue>(impl.address);
                assert(ix < slice_value.len);
                return { arr.element_type, slice_value.ptr };
            },
            [&](Slice const &slice) -> std::pair<TypeReference, void *> {
                SliceValue slice_value = scope.move<SliceValue>(impl.address);
                assert(ix < slice_value.len);
                return { slice.element_type, slice_value.ptr };
            },
            [&](Pointer const &ptr) -> std::pair<TypeReference, void *> {
                return { ptr.element_type, scope.move<void *>(impl.address) };
            },
            [&](auto const &) -> std::pair<TypeReference, void *> {
                UNREACHABLE();
                return { VoidType, nullptr };
            },
        },
        type.typespec.payload());
    auto ptr = reinterpret_cast<u8 *>(void_p);
    auto t = TypeRegistry::the()[elem_type];
    ptr += ix * t.size();
    scope.machine.push({ AddressType::Raw, reinterpret_cast<u64>(ptr) }, elem_type);
    if (scope.log) {
        auto v = Value { elem_type, ptr };
        exec_log(scope, "{}[{}] = {}", t.name, ix, v);
    }
}

template<>
void execute(Operation const &op, PushConstant const &impl, Scope &scope)
{
    scope.push(impl.value);
    exec_log(scope, "{}", impl.value);
}

template<>
void execute(Operation const &op, PushNullptr const &impl, Scope &scope)
{
    scope.push(Value { PtrType });
    exec_log(scope, "");
}

template<>
void execute(Operation const &op, PushVariableValue const &impl, Scope &scope)
{
    auto t = TypeRegistry::the()[impl.type];
    scope.machine.push(impl.address, t.ref);
    if (scope.log) {
        auto v = Value { t.ref, scope.machine.make_pointer(impl.address) };
        exec_log(scope, "{} = {}", t.name, v);
    }
}

template<>
void execute(Operation const &op, UnaryOperation const &impl, Scope &scope)
{
    scope.machine.pop(Address { AddressType::Register, 0 }, impl.operand_type);
    auto res_type = scope.machine.unary_op(0, impl.operand_type, 1, impl.op);
    scope.machine.push(Address { AddressType::Register, 1 }, res_type);
    if (scope.log) {
        auto operand_ptr = scope.machine.make_pointer(Address { AddressType::Register, 0 });
        auto res_ptr = scope.machine.make_pointer(Address { AddressType::Register, 1 });
        auto operand = Value { impl.operand_type, operand_ptr };
        auto res = Value { res_type, res_ptr };
        exec_log(scope, "{} {} = {}", impl.op, operand, res);
    }
}

}
