/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <format>
#include <optional>
#include <print>
#include <type_traits>
#include <variant>

#include <IR/Execute.h>
#include <IR/IR.h>
#include <Type/Type.h>
#include <Type/Value.h>

#include "ExecuteImpl.h"

namespace Arwen::IR {

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

void Scope::push(Value const &value)
{
    machine.push(value);
}

Value Scope::pop(TypeReference type)
{
    return machine.pop(type);
}

std::optional<i32> Scope::execute()
{
    ip = 0;
    auto new_bp = machine.sp - function.parameter_depth;
    machine.sp += function.variable_depth;
    push<u64>(machine.bp);
    auto old_bp_addr = machine.sp;
    machine.bp = new_bp;
    while (ip < function.ops.size()) {
        auto const &op = function.ops[ip];
        if (log) {
            std::print("{:16s}{:50s}", function.name, std::format("{}", op));
        }
        ++ip;
        std::visit(
            [&op, this](auto const &impl) {
                execute<std::decay_t<decltype(impl)>>(op, impl, *this);
            },
            op.op);
        if (ip == EXITED) {
            auto exit_code = pop<i32>();
            if (parent) {
                parent->ip = EXITED;
            }
            return exit_code;
        }
        if (log) {
            machine.trc();
        }
    }
    machine.move(
        Address { AddressType::Raw, reinterpret_cast<u64>(&machine.bp) },
        Address { AddressType::Raw, reinterpret_cast<u64>(machine.stack - old_bp_addr) },
        PtrType);
    machine.sp = new_bp;
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
