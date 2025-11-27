/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <Util/Utf8.h>

#include <App/IR/IR.h>
#include <Arch/AssemblyWriter.h>

namespace Arwen::X86_64 {

using namespace Util;
using namespace Arwen;

#define REGISTERS(S)                 \
    S(rax, L"rax", L"eax", L"ax")    \
    S(rbx, L"rbx", L"ebx", L"bx")    \
    S(rcx, L"rcx", L"ecx", L"cx")    \
    S(rdx, L"rdx", L"edx", L"dx")    \
    S(rsi, L"rsi", L"eax", L"ax")    \
    S(rdi, L"rdi", L"esi", L"si")    \
    S(rsp, L"rsp", L"esp", L"sp")    \
    S(rbp, L"rbp", L"ebp", L"bp")    \
    S(r8, L"r8", L"r8d", L"r8w")     \
    S(r9, L"r9", L"r9d", L"r9w")     \
    S(r10, L"r10", L"r10d", L"r10w") \
    S(r11, L"r11", L"r11d", L"r11w") \
    S(r12, L"r12", L"r12d", L"r12w") \
    S(r13, L"r13", L"r13d", L"r13w") \
    S(r14, L"r14", L"r14d", L"r14w") \
    S(r15, L"r15", L"r15d", L"r15w") \
    S(noreg, L"noreg", L"noreg", L"noreg")

#define REGISTERWIDTHS(S) \
    S(16, 'w')            \
    S(32, 'l')            \
    S(64, 'q')

struct Register {
    enum class Reg {
#undef S
#define S(R, N64, N32, N16) R,
        REGISTERS(S)
#undef S
    };

    enum class RegisterWidth {
#undef S
#define S(WIDTH, SUFFIX) W##WIDTH = WIDTH,
        REGISTERWIDTHS(S)
#undef S
    };

    std::wstring_view name(RegisterWidth w = RegisterWidth::W64)
    {
        switch (w) {
#undef S
#define S(WIDTH, SUFFIX)          \
    case RegisterWidth::W##WIDTH: \
        return name##WIDTH();
            REGISTERWIDTHS(S)
#undef S
        default:
            std::unreachable();
        }
    }

    std::wstring_view name64()
    {
        switch (reg) {
#define S(R, N64, N32, N16) \
    case Reg::R:            \
        return N64;
            REGISTERS(S)
#undef S
        default:
            std::unreachable();
        }
    }

    std::wstring_view name32()
    {
        switch (reg) {
#define S(R, N64, N32, N16) \
    case Reg::R:            \
        return N32;
            REGISTERS(S)
#undef S
        default:
            std::unreachable();
        }
    }

    std::wstring_view name16()
    {
        switch (reg) {
#define S(R, N64, N32, N16) \
    case Reg::R:            \
        return N16;
            REGISTERS(S)
#undef S
        default:
            std::unreachable();
        }
    }

#undef S
#define S(R, N64, N32, N16) static Register R;
    REGISTERS(S)
#undef S

    static Register const &get(Reg r)
    {
        switch (r) {
#undef S
#define S(R, N64, N32, N16) \
    case Reg::R:            \
        return R;
            REGISTERS(S)
#undef S
        default:
            UNREACHABLE();
        }
    }

    static Register const &get(int r)
    {
        assert(r >= 0 && noreg > r);
        return get(static_cast<Reg>(r));
    }

    auto operator<=>(Register const &) const = default;

    Register operator+(int offset)
    {
        Register ret = Register::get(std::to_underlying(reg) + offset);
        assert(ret < noreg);
        return ret;
    }

    Register operator-(int offset)
    {
        Register ret = Register::get(std::to_underlying(reg) - offset);
        assert(ret >= Register::rax);
        return ret;
    }

    operator int() const { return std::to_underlying(reg); }

    constexpr static int num_registers = std::to_underlying(Reg::noreg);

    Register(Reg reg)
        : reg(reg)
    {
    }

    Reg reg;
};

using Registers = std::vector<Register>;
using VarPointer = uint64_t;

struct StackAllocation {
    size_t size;
};

using ValueStackEntry = std::variant<Registers, StackAllocation, VarPointer>;

struct Function {
    constexpr static size_t scope_depth_index = 8;
    constexpr static size_t function_return_pointer_index = 16;
    constexpr static size_t return_value_index = 24;
    constexpr static size_t first_variable_index = 32;

    std::wstring                              name;
    struct Object                            &object;
    IR::pIR                                   function { nullptr };
    bool                                      naked;
    uint64_t                                  stack_depth { return_value_index };
    std::map<std::wstring, uint64_t>          variables;
    AssemblyWriter                            writer;
    std::array<bool, Register::num_registers> regs;
    std::array<bool, Register::num_registers> save_regs;
    std::vector<ValueStackEntry>              stack;

    void            analyze(std::vector<IR::Operation> const &operations);
    void            emit_return();
    void            skeleton();
    ValueStackEntry push_reg(pType const &type);
    ValueStackEntry push_reg(size_t size);
    ValueStackEntry pop_reg(pType const &type);
    ValueStackEntry pop_reg(size_t size);
    void            push(pType const &type);
    void            push(size_t size);
    void            pop(pType const &type);
    void            pop(size_t size);
    void            pop(pType const &type, Register target);
    void            pop(size_t size, Register target);
    void            pop(pType const &type, Registers const &targets);
    void            pop(size_t size, Registers const &targets);
    VarPointer      deref(pType const &type);
    VarPointer      deref(size_t);
    VarPointer      deref(pType const &type, Registers const &targets);
    VarPointer      deref(size_t, Registers const &targets);
    VarPointer      assign(pType const &type);
    VarPointer      assign(size_t);
    void            generate(std::vector<IR::Operation> const &operations);
};

template<typename T>
void pop(Function &function, Register target = Register::noreg)
{
    function.pop(sizeof(T), target);
}

template<typename T>
void push(Function &function)
{
    function.push(sizeof(T));
}

struct Object {
    std::wstring                     file_name;
    IR::pIR                          module { nullptr };
    std::vector<Function>            functions;
    uint64_t                         next_label { 0 };
    std::wstring                     prolog;
    std::wstring                     text;
    std::wstring                     data;
    std::map<std::wstring, uint64_t> strings;
    bool                             has_exports { false };
    bool                             has_main { false };

    void add_directive(std::wstring_view const directive, std::wstring_view const args);
    int  add_string(std::wstring_view const str);
    int  add_string(std::wstring const &str);
    void generate();
    bool save_and_assemble() const;

    template<typename Arg>
    void add_data(std::wstring_view const label, bool global, std::wstring_view type, bool is_static, Arg const &arg)
    {
        if (data.empty()) {
            data = L"\n\n.section __DATA,__data\n";
        }
        if (global) {
            data += format(L"\n.global {}", label);
        }
        data += format(L"\n.align 8\n{}:\n\t{}\t{}", label, type, arg);
        if (is_static) {
            data += L"\n\t.short 0";
        }
    }
};

struct Executable {
    IR::pIR             program;
    std::vector<Object> objects;

    bool generate();
};

void generate_unary(Function &function, pType const &operand, Operator op);
void generate_binop(Function &function, pType const &lhs_type, Operator op, pType const &rhs_type);
bool generate_x86_64(IR::IRNodes const &ir);

std::wostream &operator<<(std::wostream &os, Function const &function);
std::wostream &operator<<(std::wostream &os, Object const &object);
std::wostream &operator<<(std::wostream &os, Executable &executable);

}

namespace std {

using namespace Arwen::X86_64;

template<>
struct formatter<Register, wchar_t> {
    Register::RegisterWidth width = Register::RegisterWidth::W64;

    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end() || *it == '}')
            return it;

        switch (*it) {
#undef S
#define S(WIDTH, SUFFIX)                           \
    case SUFFIX:                                   \
        width = Register::RegisterWidth::W##WIDTH; \
        break;
            REGISTERWIDTHS(S)
#undef S
        default:
            throw std::format_error("Invalid format args for Register");
        }
        ++it;
        if (it != ctx.end() && *it != '}') {
            throw std::format_error("Invalid format args for QuotableString.");
        }
        return it;
    }

    template<class FmtContext>
    FmtContext::iterator format(Register r, FmtContext &ctx) const
    {
        std::wostringstream out;
        out << r.name(width);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

}
