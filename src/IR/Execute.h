/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <cstring>
#include <functional>
#include <map>
#include <optional>
#include <string_view>
#include <vector>

#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/IR.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen::IR {

class Scope;

struct Machine {
    using Intrinsic = std::function<void(Scope &)>;
    using Intrinsics = std::map<std::string_view, Intrinsic>;

    Program   &program;
    bool       log { false };
    Intrinsics intrinsics;
    u64        registers[32];
    u8        *stack { nullptr };
    u8        *data { nullptr };
    u64        bp;
    u64        next_bp;
    u64        sp;

    static constexpr u64 STACK_SIZE = 1024 * 1024 * 16;

    Machine(Program &program);
    ~Machine();
    std::optional<Value> run(std::vector<Value> const &args = {});
    void                 trc() const;
    void                 display() const;
    u8                  *make_pointer(Address address);
    void                 move(Address dest, Address src, TypeReference type);
    void                 move(Address dest, Address src, u64 sz, bool sign_extend = false);
    void                 pop(Address dest, TypeReference type);
    void                 pop(Address dest, u64 size);
    void                 push(Address src, TypeReference type);
    Value                pop(TypeReference type);
    void                 push(Value const &value);
    u64                  reserve(u64 bytes);
    u64                  discard(u64 bytes);
    TypeReference        binary_op(u8 lhs_reg, TypeReference lhs_type, u8 rhs_reg, TypeReference rhs_type, u8 result_reg, BinaryOperator op);
    TypeReference        unary_op(u8 operand_reg, TypeReference operand_type, u8 result_reg, UnaryOperator op);
};

class Scope {
public:
    constexpr static size_t EXITED = -1;
    Machine                &machine;
    Scope                  *parent = nullptr;
    Function const         &function;
    size_t                  ip;
    bool                    log { false };

    Scope(Machine &machine, Function &function);
    Scope(Scope &parent, Function const &function);

    std::optional<i32> execute();
    void               jump(size_t target);
    void               exit();
    void               push(Value const &value);
    Value              pop(TypeReference type);

    template<typename T>
    T move(Address src)
    {
        T ret;
        machine.move(Address { AddressType::Raw, reinterpret_cast<u64>(&ret) }, src, type_of<T>());
        return ret;
    }

    template<typename T>
    T pop()
    {
        T ret;
        machine.pop(Address { AddressType::Raw, reinterpret_cast<u64>(&ret) }, type_of<T>());
        return ret;
    }

    template<typename T>
    void push(T const &t)
    {
        machine.push(Address { AddressType::Raw, reinterpret_cast<u64>(&t) }, type_of<T>());
    }
};

}
