/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstddef>
#include <format>
#include <ios>
#include <map>
#include <optional>
#include <ostream>
#include <sstream>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <Type/Type.h>
#include <Type/Value.h>

#include <Lib.h>
#include <Logging.h>
#include <Result.h>
#include <SimpleFormat.h>

namespace Arwen::IR {

using namespace Arwen;

using Ref = size_t;

#define OperationTypes(S) \
    S(BinaryOperation)    \
    S(Break)              \
    S(Call)               \
    S(Discard)            \
    S(ForeignCall)        \
    S(FunctionReturn)     \
    S(Intrinsic)          \
    S(Jump)               \
    S(JumpF)              \
    S(JumpT)              \
    S(Label)              \
    S(MakeArray)          \
    S(PopArrayElement)    \
    S(PopFrame)           \
    S(PopVariable)        \
    S(PushArrayElement)   \
    S(PushConstant)       \
    S(PushFrame)          \
    S(PushNullptr)        \
    S(PushVariableRef)    \
    S(PushVariableValue)  \
    S(UnaryOperation)

enum class OperationType {
#undef S
#define S(T) T,
    OperationTypes(S)
#undef S
};

struct Function;
struct Module;
struct Program;

enum class AddressType {
    Raw,
    Register,
    Stack,
    Data,
};

struct Address {
    AddressType address_type;
    u64         address;
};

struct BinaryOperation {
    TypeReference  lhs_type;
    BinaryOperator op;
    TypeReference  rhs_type;
};

struct Break {
    Ref  target { 0 };
    bool block_is_loop;
};

struct Call {
    std::string_view   name;
    BoundNodeReference decl;
};

struct Discard {
    TypeReference type;
};

struct ForeignCall {
    std::string_view   name;
    BoundNodeReference decl;
};

struct FunctionReturn {
    bool has_result;
};

struct Intrinsic {
    std::string_view name;
};

struct Jump {
    Ref target { 0 };
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

struct MakeArray {
    TypeReference type;
};

struct PopArrayElement {
    Address       address;
    TypeReference type;
};

struct PopFrame {
    TypeReference type;
    bool          discard { false };
};

struct PopVariable {
    Address       address;
    TypeReference type;
};

struct PushArrayElement {
    Address       address;
    TypeReference type;
};

struct PushConstant {
    Value value;
};

struct PushFrame {
};

struct PushNullptr {
};

struct PushVariableRef {
    std::string_view name;
};

struct PushVariableValue {
    Address       address;
    TypeReference type;
};

struct UnaryOperation {
    TypeReference operand_type;
    UnaryOperator op;
};

using Op = std::variant<
    BinaryOperation,
    Break,
    Call,
    Discard,
    ForeignCall,
    FunctionReturn,
    Intrinsic,
    Jump,
    JumpF,
    JumpT,
    Label,
    MakeArray,
    PopArrayElement,
    PopFrame,
    PopVariable,
    PushArrayElement,
    PushConstant,
    PushFrame,
    PushNullptr,
    PushVariableRef,
    PushVariableValue,
    UnaryOperation>;

struct Operation {
    Ref                ref;
    Op                 op;
    std::optional<Ref> target {};
};

template<typename Op>
inline void to_string(std::ostream &out, Op const &op)
{
    out << "Unknown operation";
}

template<>
inline void to_string(std::ostream &out, BinaryOperation const &op)
{
    out << to_string(op.op);
}

template<>
inline void to_string(std::ostream &out, Call const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &, Discard const &)
{
}

template<>
inline void to_string(std::ostream &out, ForeignCall const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, FunctionReturn const &op)
{
    out << std::boolalpha << op.has_result;
}

template<>
inline void to_string(std::ostream &out, Intrinsic const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, Jump const &op)
{
    out << op.target;
}

template<>
inline void to_string(std::ostream &out, JumpF const &op)
{
    out << op.target;
}

template<>
inline void to_string(std::ostream &out, JumpT const &op)
{
    out << op.target;
}

template<>
inline void to_string(std::ostream &out, Label const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, MakeArray const &op)
{
    out << TypeRegistry::the()[op.type].name;
}

template<>
inline void to_string(std::ostream &, PopArrayElement const &)
{
}

template<>
inline void to_string(std::ostream &out, PopVariable const &op)
{
    out << op.address.address;
}

template<>
inline void to_string(std::ostream &, PushArrayElement const &)
{
}

template<>
inline void to_string(std::ostream &out, PushConstant const &op)
{
    if (op.value.type() != StringType) {
        out << std::format("{}", op.value);
    } else {
        out << '"';
        for (auto const &ch : op.value.value<std::string_view>()) {
            switch (ch) {
            case '\n':
                out << "\\n";
                break;
            case '\r':
                out << "\\r";
                break;
            case '\t':
                out << "\\t";
                break;
            default:
                out << ch;
            }
        }
        out << '"';
    }
}

template<>
inline void to_string(std::ostream &, PushNullptr const &)
{
}

template<>
inline void to_string(std::ostream &out, PushVariableRef const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, PushVariableValue const &op)
{
    out << op.address.address;
}

template<>
inline void to_string(std::ostream &out, UnaryOperation const &op)
{
    out << to_string(op.op);
}

struct Function {
    Program                          &program;
    Ref                               module_ref;
    Ref                               ref;
    BoundNodeReference                bound_ref;
    std::string_view                  name;
    std::map<BoundNodeReference, Ref> scopes;
    std::vector<Operation>            ops;
    AddressType                       address_type { AddressType::Stack };
    u64                               depth { 0 };
    u64                               bp;
    std::map<BoundNodeReference, u64> variables;

    template <typename Impl, typename... Args>
    void add_op(Args&&... args)
    {
        ops.push_back({
            ops.size(),
            Impl { args... },
        });
    }


    void list() const;
};

struct Module {
    Program                          &program;
    Ref                               ref;
    BoundNodeReference                bound_ref;
    Function                          initializer;
    std::string_view                  name;
    std::vector<Function>             functions;
    std::map<std::string_view, Ref>   function_refs;
    u64                               depth { 0 };
    std::map<BoundNodeReference, u64> variables;

    void list(Binder &binder) const;
};

struct Program {
    Binder                           &binder;
    std::vector<Module>               modules;
    std::map<std::string_view, Ref>   module_refs;
    u64                               depth { 0 };
    std::map<BoundNodeReference, u64> variables;

    Error<bool> generate();
    void        list() const;
};

}

template<>
inline std::string_view Arwen::to_string(Arwen::IR::OperationType const &type)
{
    switch (type) {
#undef S
#define S(T)                          \
    case Arwen::IR::OperationType::T: \
        return #T;
        OperationTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

template<>
struct std::formatter<Arwen::IR::Operation, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::IR::Operation const &op, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("{:5}. {:20}", op.ref, Arwen::to_string(static_cast<Arwen::IR::OperationType>(op.op.index())));
        std::visit(
            [&out](auto const &op) {
                to_string<std::decay_t<decltype(op)>>(out, op);
            },
            op.op);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
