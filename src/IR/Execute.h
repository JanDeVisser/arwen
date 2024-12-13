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
    u64        sp;

    static constexpr u64 STACK_SIZE = 1024 * 1024 * 16;

    Machine(Program &program);
    ~Machine();
    std::optional<Value> run(std::vector<Value> const &args = {});

    u8 *make_pointer(Address address)
    {
        switch (address.address_type) {
        case AddressType::Raw:
            return reinterpret_cast<u8*>(address.address);
        case AddressType::Stack:
            return stack - bp - address.address;
        case AddressType::Data:
            return data - address.address;
        case AddressType::Register:
            return reinterpret_cast<u8 *>(registers + address.address);
        }
    }

    void move(Address dest, Address src, TypeReference type)
    {
        auto t = TypeRegistry::the()[type];
        move(dest, src, t.size(), t.is_signed());
    }

    void move(Address dest, Address src, u64 sz, bool sign_extend = false)
    {
        auto src_ptr = make_pointer(src);
        auto dest_ptr = make_pointer(dest);

        if (dest.address_type == AddressType::Register && sz < 8) {
            memset(dest_ptr, 0, sizeof(u64));
        }
        memmove(dest_ptr, src_ptr, sz);
        if (sign_extend && sz < 8) {
            if ((1 << (sz * 8 - 1)) & *(reinterpret_cast<u64 *>(dest_ptr))) {
                memset(dest_ptr + sz, 0xFF, 8 - sz);
            }
        }
    }

    void pop(Address dest, TypeReference type)
    {
        pop(dest, TypeRegistry::the()[type].size());
    }

    void pop(Address dest, u64 size)
    {
        move( dest, Address { AddressType::Stack, sp - bp }, size);
        discard(size);
    }

    void push(Address src, TypeReference type)
    {
        move({ AddressType::Stack, reserve(TypeRegistry::the()[type].size()) }, src, type);
    }

    u64 reserve(u64 bytes)
    {
        sp += align_at(bytes, 8);
        assert(sp < STACK_SIZE);
        return sp - bp;
    }

    u64 discard(u64 bytes)
    {
        sp -= align_at(bytes, 8);
        assert(sp < STACK_SIZE);
        return sp - bp;
    }

    void pop_frame(u64 tetris)
    {
        auto new_bp = (bp > 0) ? *reinterpret_cast<u64 *>(stack - bp) : 0;
        if (tetris > 0) {
            memmove(stack - bp, stack - sp, tetris);
            sp = bp;
        } else {
            sp = (bp > 0) ? bp - 8 : 0;
        }
        bp = new_bp;
    }

    void push_frame()
    {
        sp += 8;
        *reinterpret_cast<u64 *>(stack - sp) = bp;
        bp = sp;
    }

    TypeReference binary_op(u8 lhs_reg, TypeReference lhs_type, u8 rhs_reg, TypeReference rhs_type, u8 result_reg, BinaryOperator op)
    {
        BinaryOperatorMapping m { op };
        auto                  lhs_ptr = registers + lhs_reg;
        auto                  rhs_ptr = registers + rhs_reg;
        auto                  lhs = Value { lhs_type, lhs_ptr };
        auto                  rhs = Value { rhs_type, rhs_ptr };

        TypeReference ret;
        auto          res = m(lhs, rhs)
                       .or_else([]() -> std::optional<Value> {
                           fatal("Could not apply operator");
                           return {};
                       })
                       .and_then([&](Value const &v) -> std::optional<Value> {
                           v.copy_value(reinterpret_cast<u8*>(registers + result_reg));
                           ret = v.type();
                           return v;
                       });
        return ret;
    }

    TypeReference unary_op(u8 operand_reg, TypeReference operand_type, u8 result_reg, UnaryOperator op)
    {
        UnaryOperatorMapping m { op };
        auto                 operand_ptr = registers + operand_reg;
        auto                 operand = Value { operand_type, operand_ptr };

        TypeReference ret;
        auto          res = m(operand)
                       .or_else([]() -> std::optional<Value> {
                           fatal("Could not apply operator");
                           return {};
                       })
                       .and_then([&](Value const &v) -> std::optional<Value> {
                           v.copy_value(reinterpret_cast<u8*>(registers + result_reg));
                           ret = v.type();
                           return v;
                       });
        return ret;
    }
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

    std::optional<Value> execute();
    void                 jump(size_t target);
    void                 exit();

    void push(Value const &value)
    {
        value.copy_value(reinterpret_cast<u8*>(machine.registers + 0));
        machine.push(Address { AddressType::Register, 0 }, value.type());
    }

    Value pop(TypeReference type)
    {
        Value ret { type, machine.stack - machine.sp };
        machine.discard(TypeRegistry::the()[type].size());
        return ret;
    }

    template <typename T>
    T move(Address src)
    {
        T ret;
        machine.move( Address { AddressType::Raw, reinterpret_cast<u64>(&ret) }, src, type_of<T>());
        return ret;
    }

    template <typename T>
    T pop()
    {
        T ret;
        machine.pop(Address {AddressType::Raw, reinterpret_cast<u64>(&ret)}, type_of<T>());
        return ret;
    }

    template <typename T>
    void push(T const &t)
    {
        machine.push(Address {AddressType::Raw, reinterpret_cast<u64>(&t)}, type_of<T>());
    }
};

}
