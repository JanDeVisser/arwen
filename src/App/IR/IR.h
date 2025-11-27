/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

#include <Util/Logging.h>
#include <Util/Ptr.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen::IR {

#define IROperationTypes(S)                           \
    S(AssignFromRef, pType)                           \
    S(AssignValue, pType)                             \
    S(BinaryOperator, BinaryOp)                       \
    S(Break, BreakOp)                                 \
    S(Call, CallOp)                                   \
    S(DeclVar, IRVariableDeclaration)                 \
    S(Dereference, pType)                             \
    S(Discard, pType)                                 \
    S(Jump, uint64_t)                                 \
    S(JumpF, uint64_t)                                \
    S(JumpT, uint64_t)                                \
    S(Label, uint64_t)                                \
    S(NativeCall, CallOp)                             \
    S(Pop, pType)                                     \
    S(PushConstant, Value)                            \
    S(PushValue, VarPath)                             \
    S(PushVarAddress, VarPath)                        \
    S(ScopeBegin, std::vector<IRVariableDeclaration>) \
    S(ScopeEnd, ScopeEndOp)                           \
    S(UnaryOperator, UnaryOp)

// S(Return, pType)                                  \
// S(Sub, uint64_t)                                  \
// S(SubRet, std::monostate)                         \

struct IRVariableDeclaration {
    std::wstring name;
    pType        type;
};

struct VarPath {
    std::wstring name;
    pType        type;
    intptr_t     offset;
};

struct Function;
struct Module;
struct Program;

template<class N>
concept ir_node = std::is_same_v<N, IR::Module>
    || std::is_same_v<N, IR::Function>
    || std::is_same_v<N, IR::Program>;

struct IRNode;

struct IRNodes {
    std::vector<IRNode>  nodes;
    Ptr<IRNode, IRNodes> program { nullptr };
    size_t               size() const;
    bool                 empty() const;
    IRNode const        &operator[](size_t ix) const;
    IRNode              &operator[](size_t ix);
};

using pIR = Ptr<IRNode, IRNodes>;

struct Operation {
    enum class Type {
#undef S
#define S(T, P) T,
        IROperationTypes(S)
#undef S
    };

    struct BreakOp {
        uint64_t scope_end;
        uint64_t depth;
        uint64_t label;
        pType    exit_type;
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
    struct ScopeEndOp {
        uint64_t enclosing_end;
        bool     has_defers;
        pType    exit_type;
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

using Operations = std::vector<Operation>;
using Declarations = std::vector<IRVariableDeclaration>;

struct Function {
    pIR          module { nullptr };
    Declarations parameters;
    pType        return_type { nullptr };

    std::wostream &list(pIR const &ir, std::wostream &os) const;
};

struct Program {
    std::map<std::wstring, pIR> functions {};
    std::map<std::wstring, pIR> modules {};

    std::wostream &list(pIR const &ir, std::wostream &os) const;
};

struct Module {
    pIR                         program { nullptr };
    std::map<std::wstring, pIR> functions {};

    std::wostream &list(pIR const &ir, std::wostream &os) const;
};

struct IRNode {
    using IRNodeVariant = std::variant<struct Function, struct Module, struct Program>;

    std::wstring  name;
    ASTNode       syntax_node;
    pIR           id;
    Operations    operations;
    Declarations  variables;
    IRNodeVariant node;

    template<ir_node N>
    static IRNode make(std::wstring name, ASTNode ast)
    {
        IRNode ret { std::move(name), std::move(ast) };
        ret.node = N {};
        return ret;
    }
};

template<class N>
N *get_if(pIR const &node)
{
    assert(node);
    return std::get_if<N>(&node->node);
}

template<class N>
N &get(pIR const &node)
{
    assert(node);
    return std::get<N>(node->node);
}

template<class N>
bool is(pIR const &node)
{
    assert(node);
    return std::holds_alternative<N>(node->node);
}

template<ir_node N>
pIR make_node(IRNodes &ir, std::wstring name, ASTNode ast)
{
    ir.nodes.push_back(IRNode::make<N>(std::move(name), std::move(ast)));
    pIR ret { &ir };
    ret->id = ret;
    return ret;
}

struct Context {
    struct LoopDescriptor {
        std::wstring name;
        uint64_t     loop_begin;
        uint64_t     loop_end;
    };
    struct BlockDescriptor {
        uint64_t                                  scope_end_label;
        std::vector<std::pair<ASTNode, uint64_t>> defer_stmts {};
    };
    struct FunctionDescriptor {
        uint64_t end_label;
        pType    return_type;
    };
    using UnwindStackEntry = std::variant<std::monostate, FunctionDescriptor, LoopDescriptor, BlockDescriptor>;

    pIR              ir_node;
    UnwindStackEntry unwind;

    Context(pIR ir_node, UnwindStackEntry unwind)
        : ir_node(std::move(ir_node))
        , unwind(std::move(unwind))
    {
    }
};

struct Generator {
    IRNodes             &ir;
    std::vector<Context> ctxs;
    void                 generate(ASTNode const &node);
};

IRNodes       &generate_ir(ASTNode const &node, IRNodes &ir);
std::wostream &list_node(pIR const &ir, std::wostream &os);
std::wostream &list(IRNodes const &ir, std::wostream &os);
bool           save(IRNodes const &ir);

}

std::wostream &operator<<(std::wostream &os, Arwen::IR::IRVariableDeclaration const &var_decl);
std::wostream &operator<<(std::wostream &os, std::vector<Arwen::IR::IRVariableDeclaration> const &var_decls);
std::wostream &operator<<(std::wostream &os, Arwen::IR::VarPath const &var_path);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::CallOp const &call);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::BreakOp const &block_exit);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::BinaryOp const &op);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::ScopeEndOp const &op);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::UnaryOp const &op);
std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation const &op);
