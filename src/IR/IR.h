/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <ios>
#include <map>
#include <ostream>
#include <sstream>
#include <string>
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
    S(Call)               \
    S(ForeignCall)        \
    S(Jump)               \
    S(JumpF)              \
    S(JumpT)              \
    S(Label)              \
    S(PushBoolean)        \
    S(PushFloat)          \
    S(PushInt)            \
    S(PushString)         \
    S(PushVariableRef)    \
    S(PushVariableValue)

enum class OperationType {
#undef S
#define S(T) T,
    OperationTypes(S)
#undef S
};

struct BinaryOperation {
    BinaryOperator op;
};

struct Call {
    std::string function;
};

struct ForeignCall {
    std::string foreign_function;
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

struct PushFloat {
    double value;
};

struct PushInt {
    int64_t value;
};

struct PushString {
    std::string value;
};

struct PushVariableRef {
    std::string name;
};

struct PushVariableValue {
    std::string name;
};

using Op = std::variant<
    BinaryOperation,
    Call,
    ForeignCall,
    Jump,
    JumpF,
    JumpT,
    Label,
    PushBoolean,
    PushFloat,
    PushInt,
    PushString,
    PushVariableRef,
    PushVariableValue>;

struct Operation {
    Ref ref;
    Op  op;
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
    out << op.function;
}

template<>
inline void to_string(std::ostream &out, Label const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, ForeignCall const &op)
{
    out << op.foreign_function;
}

template<>
inline void to_string(std::ostream &out, PushBoolean const &op)
{
    out << std::boolalpha << op.value;
}

template<>
inline void to_string(std::ostream &out, PushFloat const &op)
{
    out << op.value;
}

template<>
inline void to_string(std::ostream &out, PushInt const &op)
{
    out << op.value;
}

template<>
inline void to_string(std::ostream &out, PushString const &op)
{
    for (auto const &ch : op.value) {
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
}

template<>
inline void to_string(std::ostream &out, PushVariableRef const &op)
{
    out << op.name;
}

template<>
inline void to_string(std::ostream &out, PushVariableValue const &op)
{
    out << op.name;
}

struct Function {
    Ref                    ref;
    BoundNodeReference     bound_ref;
    std::string_view       name;
    std::vector<Operation> ops;

    void list() const;
};

struct Module {
    Ref                        ref;
    BoundNodeReference         bound_ref;
    std::string_view           name;
    std::vector<Function>      functions;
    std::map<std::string, Ref> function_refs;

    void list(Binder& binder) const;
};

struct Program {
    Binder                    &binder;
    std::vector<Module>        modules;
    std::map<std::string, Ref> module_refs;

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
