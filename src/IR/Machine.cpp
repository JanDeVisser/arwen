/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <ios>
#include <iostream>
#include <optional>
#include <print>
#include <sys/mman.h>
#include <vector>

#include <AST/Operator.h>
#include <IR/Execute.h>
#include <IR/IR.h>
#include <IR/Intrinsics.h>
#include <Logging.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen::IR {

Machine::Machine(Program &program)
    : program(program)
{
    intrinsics["exit"] = exit;
    intrinsics["len"] = len;
    intrinsics["make_string"] = make_string;
    intrinsics["ptr"] = Arwen::IR::ptr;
    stack = static_cast<u8 *>(mmap(nullptr, STACK_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
    if (stack == MAP_FAILED) {
        fatal("Could not allocate VM Memory: {}", strerror(errno));
    }
    stack += STACK_SIZE;
    bp = sp = 0;
    if (program.depth) {
        data = static_cast<u8 *>(mmap(nullptr, program.depth, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
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

u8 *Machine::make_pointer(Address address)
{
    switch (address.address_type) {
    case AddressType::Raw:
        return reinterpret_cast<u8 *>(address.address);
    case AddressType::Stack:
        return stack - bp - address.address;
    case AddressType::Data:
        return data - address.address;
    case AddressType::Register:
        return reinterpret_cast<u8 *>(registers + address.address);
    }
}

void Machine::move(Address dest, Address src, TypeReference type)
{
    auto t = TypeRegistry::the()[type];
    move(dest, src, t.size(), t.is_signed());
}

void Machine::move(Address dest, Address src, u64 sz, bool sign_extend)
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

void Machine::pop(Address dest, TypeReference type)
{
    pop(dest, TypeRegistry::the()[type].size());
}

void Machine::pop(Address dest, u64 size)
{
    move(dest, Address { AddressType::Stack, sp - bp }, size);
    // if (log) {
    //     std::println("  <- {} bytes", size);
    // }
    discard(size);
}

void Machine::push(Address src, TypeReference type)
{
    // if (log) {
    //     std::println("  -> {} bytes", TypeRegistry::the()[type].size());
    // }
    move({ AddressType::Stack, reserve(TypeRegistry::the()[type].size()) }, src, type);
}

Value Machine::pop(TypeReference type)
{
    Value ret { type, stack - sp };
    discard(align_at(TypeRegistry::the()[type].size(), 8));
    // if (log) {
    //     std::println("  <- {}", ret);
    // }
    return ret;
}

void Machine::push(Value const &value)
{
    // if (log) {
    //     std::println("  -> {}", value);
    // }
    reserve(TypeRegistry::the()[value.type()].size());
    value.copy_value(stack - sp);
}

u64 Machine::reserve(u64 bytes)
{
    sp += align_at(bytes, 8);
    assert(sp < STACK_SIZE);
    return sp - bp;
}

u64 Machine::discard(u64 bytes)
{
    sp -= align_at(bytes, 8);
    assert(sp < STACK_SIZE);
    return sp - bp;
}

TypeReference Machine::binary_op(u8 lhs_reg, TypeReference lhs_type, u8 rhs_reg, TypeReference rhs_type, u8 result_reg, BinaryOperator op)
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
                       v.copy_value(reinterpret_cast<u8 *>(registers + result_reg));
                       ret = v.type();
                       return v;
                   });
    return ret;
}

TypeReference Machine::unary_op(u8 operand_reg, TypeReference operand_type, u8 result_reg, UnaryOperator op)
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
                       v.copy_value(reinterpret_cast<u8 *>(registers + result_reg));
                       ret = v.type();
                       return v;
                   });
    return ret;
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

void Machine::display() const
{
    auto display_u64 = [](u64 val) {
        u8 *ptr = reinterpret_cast<u8 *>(&val);
        for (auto ix = 0; ix < 8; ++ix) {
            std::cout << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(ptr[ix]) << ' ';
        }
    };
    // for (auto ix = 0; ix < 4; ++ix) {
    //     std::cout << "R" << ix << ": ";
    //     display_u64(registers[ix]);
    //     std::cout << "\n";
    // }
    std::cout << "SP: " << std::hex << std::setw(4) << std::setfill('0') << sp << '\n';
    std::cout << "BP: " << std::hex << std::setw(4) << std::setfill('0') << bp << '\n';
    for (auto ix = 0; ix < sp; ix += 8) {
        std::cout << std::hex << std::setw(2) << std::setfill('0') << ix << ":";
        if (ix == bp) {
            std::cout << '*';
        } else {
            std::cout << ' ';
        }
        display_u64(reinterpret_cast<u64 *>(stack - ix - 8)[0]);
        std::cout << "\n";
    }
}

void Machine::trc() const
{
    enum class TraceMode {
        SingleStep,
        Run,
        SilentRun,
        VerboseRun,
    };
    static auto run = TraceMode::SingleStep;

    if (run != TraceMode::SingleStep && run != TraceMode::VerboseRun) {
        return;
    }
    display();
    if (run != TraceMode::SingleStep) {
        return;
    }
    auto cmd = 'Q';
    do {
        std::cout << "(R)un/(S)tep/Silen(T)/(V)erbose/(Q)uit: ";
        std::cin >> cmd;
        cmd = std::toupper(cmd);
    } while (strchr("RSTVQ", cmd) == nullptr);
    switch (cmd) {
    case 'R':
        run = TraceMode::Run;
        break;
    case 'T':
        run = TraceMode::SilentRun;
        break;
    case 'V':
        run = TraceMode::VerboseRun;
        break;
    case 'S':
        break;
    case 'Q':
        ::exit(0);
    default:
        UNREACHABLE();
    }
}

}
