/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Util/Logging.h"
#include <cstdint>
#include <string>

#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen::IR {

#define IROperationTypes(S)           \
    S(Assign, std::monostate)         \
    S(BinaryOperator, BinaryOp)       \
    S(Call, CallOp)                   \
    S(Cast, pType)                    \
    S(DeclVar, IRVariableDeclaration) \
    S(Discard, std::monostate)        \
    S(Jump, uint64_t)                 \
    S(JumpF, uint64_t)                \
    S(JumpT, uint64_t)                \
    S(Label, uint64_t)                \
    S(NativeCall, CallOp)             \
    S(PushConstant, Value)            \
    S(PushValue, std::wstring)        \
    S(PushVarAddress, VarPath)        \
    S(Return, std::monostate)         \
    S(ScopeBegin, uint64_t)           \
    S(ScopeEnd, std::monostate)       \
    S(Sub, uint64_t)                  \
    S(SubRet, std::monostate)         \
    S(UnaryOperator, UnaryOp)

struct IRVariableDeclaration {
    std::wstring name;
    pType        type;
};

struct VarPath {
    std::wstring          name;
    std::vector<uint64_t> path;
};

struct Operation {
    enum class Type {
#undef S
#define S(T, P) T,
        IROperationTypes(S)
#undef S
    };

    struct CallOp {
        std::wstring                       name;
        std::vector<IRVariableDeclaration> parameters;
        pType                              return_type;
    };
    struct BinaryOp {
        pType    lhs;
        Operator op;
        pType    rhs;
    };
    struct UnaryOp {
        pType    operand;
        Operator op;
    };

#undef S
#define S(T, P)                                 \
    struct T {                                  \
        constexpr static Type type { Type::T }; \
        P                     payload;          \
    };
    IROperationTypes(S)
#undef S

        using Op
        = std::variant<
#undef S
#define S(T, P) T,
            IROperationTypes(S)
#undef S
                std::monostate>;

    Op op;

    [[nodiscard]] Type type() const
    {
        return static_cast<Type>(op.index());
    }

    [[nodiscard]] char const *type_name() const
    {
        switch (type()) {
#undef S
#define S(T, P)   \
    case Type::T: \
        return #T;
            IROperationTypes(S)
#undef S
                default : UNREACHABLE();
        }
    }
};

template<typename Op>
[[nodiscard]] bool is(Operation const &operation)
{
    return std::holds_alternative<Op>(operation.op);
}

template<typename Op>
[[nodiscard]] Op const &get(Operation const &operation)
{
    assert(is<Op>(operation));
    return std::get<Op>(operation.op);
}

struct Function;
struct Module;
struct Program;

using pFunction = std::shared_ptr<Function>;
using pModule = std::shared_ptr<Module>;
using pProgram = std::shared_ptr<Program>;

struct Function {
    std::wstring                       name;
    pSyntaxNode                        syntax_node;
    pModule                            module { nullptr };
    std::vector<IRVariableDeclaration> parameters;
    pType                              return_type;
    std::vector<Operation>             operations;
};

struct Program {
    std::wstring                    name;
    pSyntaxNode                     syntax_node;
    std::map<std::wstring, pModule> modules {};
};

struct Module {
    std::wstring                       name;
    pSyntaxNode                        syntax_node;
    pProgram                           program { nullptr };
    std::vector<IRVariableDeclaration> variables;
    std::map<std::wstring, pFunction>  functions {};
    std::vector<Operation>             operations;
};

using IRNode = std::variant<std::monostate, pFunction, pModule, pProgram>;

struct Context {
    struct LoopDescriptor {
        std::wstring name;
        uint64_t     loop_begin;
        uint64_t     loop_end;
    };
    struct BlockDescriptor {
        std::vector<std::pair<pSyntaxNode, uint64_t>> defer_stmts {};
    };
    using UnwindStackEntry = std::variant<std::monostate, LoopDescriptor, BlockDescriptor>;

    IRNode           ir_node;
    UnwindStackEntry unwind;

    Context(IRNode ir_node, UnwindStackEntry unwind)
        : ir_node(std::move(ir_node))
        , unwind(std::move(unwind))
    {
    }
};

struct Generator {
    std::vector<Context> ctxs;
    void                 generate(pSyntaxNode const &node);
};

IRNode generate_ir(pSyntaxNode const &node);
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::IRVariableDeclaration const &var_decl);
std::wostream &operator<<(std::wostream &os, Arwen::IR::VarPath const &var_path);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::CallOp const &call);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::BinaryOp const &op);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::UnaryOp const &op);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation const &op);
