/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <Type/Type.h>
#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <Lib.h>
#include <Result.h>
#include <Type/Value.h>

namespace Arwen::IR {

using namespace Arwen;

using Ref = size_t;

struct BinaryOperation {
    BinaryOperator op;
};

struct Call {
    std::string function;
};

struct Jump {
    Ref target;
};

struct JumpF {
    Ref target;
};

struct JumpT {
    Ref target;
};

struct Label {
    std::string_view name;
};

struct PushBoolean {
    bool value;
};

struct PushInt {
    int64_t value;
};

struct PushString {
    std::string value;
};

using Op = std::variant<
    BinaryOperation,
    Call,
    Jump,
    JumpF,
    JumpT,
    Label,
    PushBoolean,
    PushInt,
    PushString
    >;

struct Operation {
    Ref ref;
    Op op;
};

struct Function {
    Ref ref;
    BoundNodeReference bound_ref;
    std::string_view name;
    std::vector<Operation> ops;

    void generate(Binder &binder);
};

struct Module {
    Ref ref;
    BoundNodeReference bound_ref;
    std::string_view name;
    std::vector<Function> functions;
    std::map<std::string, Ref> function_refs;

    void generate(Binder &binder);
};

struct Program {
    std::vector<Module> modules;
    std::map<std::string, Ref> module_refs;

    Error<bool> generate(Binder &binder);
};

template <class NodeImpl>
inline void generate(NodeImpl const& impl, Binder &, Function &ir)
{
}

template<>
inline void generate(BoundBinaryExpression const& impl, Binder &binder, Function &ir)
{
    generate(binder[impl.right].impl, binder, ir);
    generate(binder[impl.left].impl, binder, ir);
    ir.ops.push_back({ir.ops.size(), BinaryOperation{impl.op}});
}

template<>
inline void generate(BoundBlock const& impl, Binder &binder, Function &ir)
{
    if (impl.label) {
        ir.ops.push_back({ir.ops.size(), Label{*impl.label}});
    }
    for (auto stmt : impl.statements) {
        generate(binder[stmt].impl, binder, ir);
    }
}

}
