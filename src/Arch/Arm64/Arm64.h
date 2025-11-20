/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <bitset>
#include <expected>
#include <string>

#include <App/IR/IR.h>

namespace Arwen::Arm64 {

using namespace Util;
using namespace Arwen;

enum class ARM64ErrorCode {
    InternalError,
    IOError,
};

struct ARM64Error {
    ARM64ErrorCode code;
    std::string    message;

    std::string const &to_string() const
    {
        return message;
    }
};

struct RegisterAllocation {
    int      reg;
    intptr_t num_regs;
};

using VarPointer = uint64_t;

using ValueStackEntry = std::variant<RegisterAllocation, VarPointer>;

struct Function {
    std::wstring                     name;
    struct Object                   &object;
    IR::pIR                          function { nullptr };
    uint64_t                         stack_depth { 0 };
    std::map<std::wstring, uint64_t> variables;
    std::wstring                     prolog;
    std::wstring                     code;
    std::wstring                     epilog;
    std::wstring                    *active { &code };
    std::bitset<28>                  regs;
    std::bitset<28>                  save_regs;
    std::vector<ValueStackEntry>     stack;

    template<typename... Args>
    void add_instruction(std::wstring_view const mnemonic, std::wstring_view param_fmt, Args &&...args)
    {
        auto const fmt { std::format(L"\t{{}}\t{}\n", param_fmt) };
        *active += std::vformat(fmt, std::make_wformat_args(mnemonic, args...));
    }

    void               add_instruction(std::wstring_view const mnemonic, std::wstring_view const param);
    void               add_instruction(std::wstring_view const mnemonic);
    void               add_text(std::wstring_view const &text);
    void               add_label(std::wstring_view const label);
    void               add_directive(std::wstring_view const directive, std::wstring_view const args);
    void               add_comment(std::wstring_view const comment);
    [[nodiscard]] bool empty() const;
    [[nodiscard]] bool has_text() const;
    void               activate_prolog();
    void               activate_code();
    void               activate_epilog();
    void               analyze(std::vector<IR::Operation> const &operations);
    void               emit_return();
    void               skeleton();
    RegisterAllocation push_reg(pType const &type);
    RegisterAllocation push_reg(size_t size);
    RegisterAllocation pop_reg(pType const &type);
    RegisterAllocation pop_reg(size_t size);
    void               push(pType const &type);
    void               push(size_t size);
    int                pop(pType const &type, int target = 0);
    int                pop(size_t size, int target = 0);
    VarPointer         deref(pType const &type, int target = 0);
    VarPointer         deref(size_t, int target = 0);
    VarPointer         assign(pType const &type);
    VarPointer         assign(size_t);
    void               generate(std::vector<IR::Operation> const &operations);
};

template<typename T>
int pop(Function &function, int target = 0)
{
    return function.pop(sizeof(T), target);
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

    void                            add_directive(std::wstring_view const directive, std::wstring_view const args);
    int                             add_string(std::wstring_view const str);
    int                             add_string(std::wstring const &str);
    void                            generate(IR::pIR const &module);
    std::expected<bool, ARM64Error> save_and_assemble() const;

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

    std::expected<void, ARM64Error> generate();
};

void                            generate_unary(Function &function, pType const &operand, Operator op);
void                            generate_binop(Function &function, pType const &lhs_type, Operator op, pType const &rhs_type);
std::expected<void, ARM64Error> generate_arm64(IR::IRNodes const &ir);

std::wostream &operator<<(std::wostream &os, Function const &function);
std::wostream &operator<<(std::wostream &os, Object const &object);
std::wostream &operator<<(std::wostream &os, Executable &executable);

}
