/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Util/Logging.h"
#include "Util/TokenLocation.h"
#include <concepts>
#include <cstdint>
#include <limits>
#include <memory>
#include <string>
#include <type_traits>
#include <vector>

#include <App/Operator.h>
#include <App/Type.h>

namespace Arwen {

using namespace Util;

#define SyntaxNodeTypes(S) \
    S(BinaryExpression)    \
    S(Block)               \
    S(BoolConstant)        \
    S(Break)               \
    S(Continue)            \
    S(Decimal)             \
    S(DeferStatement)      \
    S(DoubleQuotedString)  \
    S(Dummy)               \
    S(Embed)               \
    S(Enum)                \
    S(EnumValue)           \
    S(Error)               \
    S(ExpressionList)      \
    S(ExternLink)          \
    S(ForStatement)        \
    S(FunctionDeclaration) \
    S(FunctionDefinition)  \
    S(Identifier)          \
    S(IfStatement)         \
    S(Include)             \
    S(Integer)             \
    S(LoopStatement)       \
    S(Module)              \
    S(Number)              \
    S(Parameter)           \
    S(QuotedString)        \
    S(Return)              \
    S(SignedInteger)       \
    S(SingleQuotedString)  \
    S(Struct)              \
    S(StructMember)        \
    S(TypeSpecification)   \
    S(UnaryExpression)     \
    S(VariableDeclaration) \
    S(Void)                \
    S(WhileStatement)      \
    S(Yield)

enum class SyntaxNodeType {
#undef S
#define S(T) T,
    SyntaxNodeTypes(S)
#undef S
};

extern char const *SyntaxNodeType_name(SyntaxNodeType type);
extern void        print_indent(int indent);

using pSyntaxNode = std::shared_ptr<struct SyntaxNode>;
using SyntaxNodes = std::vector<pSyntaxNode>;
using pBoundNode = std::shared_ptr<struct BoundNode>;
using Label = std::optional<std::wstring>;
using pTypeSpecification = std::shared_ptr<struct TypeSpecification>;
using TypeSpecifications = std::vector<pTypeSpecification>;

struct Parser;

struct SyntaxNode : std::enable_shared_from_this<SyntaxNode> {
    TokenLocation  location;
    SyntaxNodeType type;
    pType          bound_type;

    virtual ~SyntaxNode() = default;
    SyntaxNode() = delete;
    SyntaxNode(SyntaxNodeType type);

    void         dump(int indent = 0);
    virtual void header();
    virtual void dump_node(int indent);

    virtual pSyntaxNode normalize(Parser &parser);
    virtual pType       bind(Parser &parser) = 0;
    virtual pSyntaxNode coerce(pType const &target, Parser &parser);
};

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(TokenLocation location, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = std::move(location);
    // std::cout << "[" << SyntaxNodeType_name(ret->type) << "] (" << ret->location.line + 1 << "," << ret->location.column + 1 << ")" << std::endl;
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(pSyntaxNode const &child, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = child->location;
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(std::array<pSyntaxNode, 2> const &children, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = children[0]->location + children[1]->location;
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(std::vector<pSyntaxNode> const &children, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = children[0]->location + children.back()->location;
    return ret;
}

template<class Node, class Normalized = Node>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> normalize_node(std::shared_ptr<Node> node, Parser &parser)
{
    return (node) ? std::dynamic_pointer_cast<Normalized>(node->normalize(parser)) : nullptr;
}

template<class Node>
    requires std::derived_from<Node, SyntaxNode>
static std::vector<std::shared_ptr<Node>> normalize_nodes(std::vector<std::shared_ptr<Node>> nodes, Parser &parser)
{
    std::vector<std::shared_ptr<Node>> normalized;
    for (auto const &n : nodes) {
        normalized.emplace_back(std::dynamic_pointer_cast<Node>(n->normalize(parser)));
    }
    return normalized;
}

struct BinaryExpression : SyntaxNode {
    pSyntaxNode lhs;
    Operator    op;
    pSyntaxNode rhs;

    BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs);

    pType       bind(Parser &parser) override;
    pSyntaxNode normalize(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

using pConstantExpression = std::shared_ptr<struct ConstantExpression>;

struct ConstantExpression : SyntaxNode {
    ConstantExpression(SyntaxNodeType type);
    pSyntaxNode evaluate(Operator op, pConstantExpression const &rhs = nullptr);

#undef S
#undef S
#define S(O) virtual pSyntaxNode evaluate_##O(pConstantExpression const &rhs = nullptr);
    Operators(S)
#undef S
};

struct Block : SyntaxNode {
    SyntaxNodes statements;

    Block(SyntaxNodes statements);

    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct BoolConstant : ConstantExpression {
    bool value;

    BoolConstant(bool value);
    pType       bind(Parser &parser) override;
    void        header() override;
    pSyntaxNode evaluate_LogicalInvert(pConstantExpression const &) override;
};

struct Break : SyntaxNode {
    Label label;

    Break(Label label);

    pType bind(Parser &parser) override;
    void  header() override;
};

struct Continue : SyntaxNode {
    Label label;

    Continue(Label label);

    pType bind(Parser &parser) override;
    void  header() override;
};

struct DeferStatement : SyntaxNode {
    pSyntaxNode stmt;

    DeferStatement(pSyntaxNode stmt);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct DoubleQuotedString : ConstantExpression {
    std::wstring string;

    DoubleQuotedString(std::wstring_view str, bool strip_quotes);
    pType       bind(Parser &parser) override;
    void        header() override;
    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override;
};

struct Dummy : SyntaxNode {
    Dummy();
    pType bind(Parser &parser) override;
};

struct Embed : SyntaxNode {
    std::wstring file_name;

    Embed(std::wstring_view file_name);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
};

struct EnumValue : SyntaxNode {
    std::wstring        label;
    pConstantExpression value;
    pTypeSpecification  payload;

    EnumValue(std::wstring label, pConstantExpression value, pTypeSpecification payload);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
};

using pEnumValue = std::shared_ptr<EnumValue>;
using EnumValues = std::vector<pEnumValue>;

struct Enum : SyntaxNode {
    std::wstring       name;
    pTypeSpecification underlying_type;
    EnumValues         values;

    Enum(std::wstring name, pTypeSpecification underlying_type, EnumValues values);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
    void        header() override;
};

struct Error : SyntaxNode {
    pSyntaxNode expression;

    Error(pSyntaxNode expression);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct ExpressionList : SyntaxNode {
    SyntaxNodes expressions;

    ExpressionList(SyntaxNodes expressions);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct ExternLink : SyntaxNode {
    std::wstring link_name;

    ExternLink(std::wstring link_name);
    pType bind(Parser &parser) override;
    void  header() override;
};

struct ForStatement : SyntaxNode {
    std::wstring range_variable;
    pSyntaxNode  range_expr;
    pSyntaxNode  statement;

    ForStatement(std::wstring var, pSyntaxNode expr, pSyntaxNode stmt);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

using pParameter = std::shared_ptr<struct Parameter>;

struct FunctionDeclaration : SyntaxNode {
    std::wstring            name;
    std::vector<pParameter> parameters;
    pTypeSpecification      return_type;

    FunctionDeclaration(std::wstring name, std::vector<pParameter> parameters, pTypeSpecification return_type);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

using pFunctionDeclaration = std::shared_ptr<FunctionDeclaration>;

struct FunctionDefinition : SyntaxNode {
    pFunctionDeclaration declaration;
    pSyntaxNode          implementation;

    FunctionDefinition(pFunctionDeclaration declaration, pSyntaxNode implementation);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct Identifier : SyntaxNode {
    std::wstring identifier;

    Identifier(std::wstring_view identifier);
    pType bind(Parser &parser) override;
    void  header() override;
};

struct IfStatement : SyntaxNode {
    pSyntaxNode condition;
    pSyntaxNode if_branch;
    pSyntaxNode else_branch;

    IfStatement(pSyntaxNode condition, pSyntaxNode if_branch, pSyntaxNode else_branch);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct Include : SyntaxNode {
    std::wstring file_name;

    Include(std::wstring_view file_name);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
};

template<typename T, SyntaxNodeType NodeType>
struct Num : ConstantExpression {
    using Integer = Num<uint64_t, SyntaxNodeType::Integer>;
    using SignedInteger = Num<int64_t, SyntaxNodeType::SignedInteger>;
    using Decimal = Num<double, SyntaxNodeType::Decimal>;

    T value;

    Num(std::wstring_view number)
        : ConstantExpression(NodeType)
    {
        if constexpr (std::is_integral_v<T>) {
            this->value = string_to_integer<T>(number)
                              .or_else([number]() -> std::optional<T> {
                                  std::wcerr << "Could not convert string '" << number << "' to integer. This is unexpected" << std::endl;
                                  abort();
                                  return { 0 };
                              })
                              .value();
        }
        if constexpr (std::is_floating_point_v<T>) {
            char *end_ptr;
            auto  narrow_string = as_utf8(number);
            value = static_cast<T>(strtod(narrow_string.data(), &end_ptr));
            assert(end_ptr != narrow_string.data());
        }
    }

    Num(T number)
        : ConstantExpression(NodeType)
        , value(number)
    {
    }

    template<typename Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        fatal("Unexpected return type in evaluate_numeric_op");
    }

    template<std::unsigned_integral Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<uint64_t, SyntaxNodeType::Integer>>(std::move(location), result);
    }

    template<std::signed_integral Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<int64_t, SyntaxNodeType::SignedInteger>>(std::move(location), result);
    }

    template<std::floating_point Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<double, SyntaxNodeType::Decimal>>(std::move(location), result);
    }

    template<typename Func>
    pSyntaxNode evaluate_numeric_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
    {
        switch (rhs->type) {
        case SyntaxNodeType::Integer: {
            auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::Decimal: {
            auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_decimal->value));
        }
        default:
            return nullptr;
        }
    }

    template<typename Func>
    pSyntaxNode evaluate_binary_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
    {
        switch (rhs->type) {
        case SyntaxNodeType::Integer: {
            auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        default:
            return nullptr;
        }
    }

    template<typename Func>
    pSyntaxNode evaluate_comparison_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
    {
        switch (rhs->type) {
        case SyntaxNodeType::Integer: {
            auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
            bool z = func(lhs->value, rhs_integer->value);
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            bool z = func(lhs->value, rhs_integer->value);
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        case SyntaxNodeType::Decimal: {
            auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
            bool z = func(lhs->value, rhs_decimal->value);
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        default:
            return nullptr;
        }
    }

    pType bind(Parser &parser) override
    {
        if (value > IntType::u32.max_value) {
            return TypeRegistry::u64;
        }
        return TypeRegistry::u32;
    }

    template<typename V>
    pSyntaxNode coerce_value(pType const &target, Parser &parser)
    {
        return ConstantExpression::coerce(target, parser);
    }

    template<std::unsigned_integral V>
    pSyntaxNode coerce_value(pType const &target, Parser &)
    {
        if (!std::holds_alternative<IntType>(target->description)) {
            return nullptr;
        }
        if (value <= std::get<IntType>(target->description).max_value) {
            auto ret = make_node<Integer>(location, value);
            ret->bound_type = target;
            return ret;
        }
        return nullptr;
    }

    template<std::signed_integral V>
    pSyntaxNode coerce_value(pType const &target, Parser &)
    {
        if (!std::holds_alternative<IntType>(target->description)) {
            return nullptr;
        }
        if (value >= std::get<IntType>(target->description).min_value
            && value <= std::get<IntType>(target->description).max_value) {
            auto ret = make_node<Integer>(location, value);
            ret->bound_type = target;
            return ret;
        }
        return nullptr;
    }

    pSyntaxNode coerce(pType const &target, Parser &parser) override
    {
        if (bound_type == target) {
            return shared_from_this();
        }
        return coerce_value<T>(target, parser);
    }

    void header() override
    {
        std::wcout << value;
    }

    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override
    {
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x + y; });
    }

    pSyntaxNode evaluate_Subtract(pConstantExpression const &rhs) override
    {
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x - y; });
    }

    pSyntaxNode evaluate_Multiply(pConstantExpression const &rhs) override
    {
        if (auto rhs_string = std::dynamic_pointer_cast<DoubleQuotedString>(rhs); rhs_string != nullptr) {
            std::wstring s;
            for (auto ix = 0; ix < value; ++value) {
                s += rhs_string->string;
            }
            return make_node<DoubleQuotedString>(location + rhs->location, s, false);
        }
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x * y; });
    }

    pSyntaxNode evaluate_Divide(pConstantExpression const &rhs) override
    {
        if (value == 0) {
            std::wcerr << "Division by zero" << std::endl;
            return nullptr;
        }
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x / y; });
    }

    pSyntaxNode evaluate_Modulo(pConstantExpression const &rhs) override
    {
        if (value == 0) {
            std::wcerr << "Division by zero" << std::endl;
            return nullptr;
        }
        if constexpr (std::is_integral_v<T>) {
            switch (rhs->type) {
            case SyntaxNodeType::Integer: {
                auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
                return make_node<Integer>(location + rhs->location, value % rhs_integer->value);
            }
            case SyntaxNodeType::SignedInteger: {
                auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
                return make_node<SignedInteger>(location + rhs->location, value % rhs_integer->value);
            }
            case SyntaxNodeType::Decimal: {
                auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
                return make_node<Decimal>(location + rhs->location, fmod(value, rhs_decimal->value));
            }
            default:
                return nullptr;
            }
        } else {
            return evaluate_numeric_op(
                this, rhs,
                [](auto x, auto y) { return fmod(x, y); });
        }
    }

    pSyntaxNode evaluate_Equals(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x == y; });
    }

    pSyntaxNode evaluate_NotEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x != y; });
    }

    pSyntaxNode evaluate_Less(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x < y; });
    }

    pSyntaxNode evaluate_LessEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x <= y; });
    }

    pSyntaxNode evaluate_Greater(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x > y; });
    }

    pSyntaxNode evaluate_GreaterEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x >= y; });
    }

    pSyntaxNode evaluate_BinaryAnd(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryAnd(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x & y; });
        }
    }

    pSyntaxNode evaluate_BinaryOr(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryOr(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x | y; });
        }
    }

    pSyntaxNode evaluate_BinaryXor(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryXor(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x ^ y; });
        }
    }

    pSyntaxNode evaluate_ShiftLeft(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_ShiftLeft(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x << y; });
        }
    }

    pSyntaxNode evaluate_ShiftRight(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_ShiftRight(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x >> y; });
        }
    }

    pSyntaxNode evaluate_Idempotent(pConstantExpression const &) override
    {
        return make_result(location, value);
    }

    pSyntaxNode evaluate_Negate(pConstantExpression const &) override
    {
        if constexpr (std::is_integral_v<T> && std::is_unsigned_v<T>) {
            if (value > std::numeric_limits<int64_t>::max()) {
                fatal("Cannot invert integer larger than int64_t::max");
            }
        }
        return make_result(location, -value);
    }

    pSyntaxNode evaluate_BinaryInvert(pConstantExpression const &) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryInvert();
        } else {
            return make_result(location, ~value);
        }
    }
};

using Integer = Num<uint64_t, SyntaxNodeType::Integer>;
using SignedInteger = Num<int64_t, SyntaxNodeType::SignedInteger>;
using Decimal = Num<double, SyntaxNodeType::Decimal>;

struct LoopStatement : SyntaxNode {
    Label       label;
    pSyntaxNode statement;

    LoopStatement(Label label, pSyntaxNode statement);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Module : SyntaxNode {
    std::string       name;
    std::wstring_view source;
    pSyntaxNode       statements;

    Module(std::string_view name, std::wstring_view source, SyntaxNodes statements);
    Module(std::string_view name, std::wstring_view source, pSyntaxNode statement);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Number : ConstantExpression {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
};

using pNumber = std::shared_ptr<Number>;

struct Parameter : SyntaxNode {
    std::wstring       name;
    pTypeSpecification type_name;

    Parameter(std::wstring name, pTypeSpecification type_name);
    pType bind(Parser &parser) override;
    void  header() override;
};

struct QuotedString : SyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type);
    pType       bind(Parser &parser) override;
    void        header() override;
    pSyntaxNode normalize(Parser &parser) override;
};

struct Return : SyntaxNode {
    pSyntaxNode expression;

    Return(pSyntaxNode expression);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct SingleQuotedString : ConstantExpression {
    std::wstring string;

    SingleQuotedString(std::wstring_view str, bool strip_quotes);
    pType bind(Parser &parser) override;
    void  header() override;
};

struct StructMember : SyntaxNode {
    std::wstring       label;
    pTypeSpecification type;

    StructMember(std::wstring label, pTypeSpecification payload);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
};

using pStructMember = std::shared_ptr<StructMember>;
using StructMembers = std::vector<pStructMember>;

struct Struct : SyntaxNode {
    std::wstring  name;
    StructMembers members;

    Struct(std::wstring name, StructMembers members);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        dump_node(int indent) override;
    void        header() override;
};

struct TypeDescriptionNode {
    std::wstring       name;
    TypeSpecifications arguments {};
};

struct SliceDescriptionNode {
    pTypeSpecification slice_of;
};

struct ZeroTerminatedArrayDescriptionNode {
    pTypeSpecification array_of;
};

struct ArrayDescriptionNode {
    pTypeSpecification array_of;
    size_t             size;
};

struct OptionalDescriptionNode {
    pTypeSpecification optional_of;
};

struct ErrorDescriptionNode {
    pTypeSpecification success;
    pTypeSpecification error;
};

using TypeSpecificationDescription = std::variant<
    TypeDescriptionNode,
    SliceDescriptionNode,
    ZeroTerminatedArrayDescriptionNode,
    ArrayDescriptionNode,
    OptionalDescriptionNode,
    ErrorDescriptionNode>;

struct TypeSpecification : SyntaxNode {

    TypeSpecificationDescription description;

    TypeSpecification(TypeSpecificationDescription description);
    TypeSpecification(TypeDescriptionNode type);
    TypeSpecification(SliceDescriptionNode slice);
    TypeSpecification(ZeroTerminatedArrayDescriptionNode array);
    TypeSpecification(ArrayDescriptionNode array);
    TypeSpecification(OptionalDescriptionNode optional);
    TypeSpecification(ErrorDescriptionNode error);

    pSyntaxNode  normalize(Parser &parser) override;
    pType        bind(Parser &parser) override;
    void         header() override;
    std::wstring to_string();
    pType        resolve();
};

struct UnaryExpression : SyntaxNode {
    Operator    op;
    pSyntaxNode operand;

    UnaryExpression(Operator op, pSyntaxNode operand);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct VariableDeclaration : SyntaxNode {
    std::wstring       name;
    pTypeSpecification type_name {};
    pSyntaxNode        initializer;
    bool               is_const;

    VariableDeclaration(std::wstring name, pTypeSpecification type_name, pSyntaxNode initializer, bool is_const);
    pType       bind(Parser &parser) override;
    pSyntaxNode normalize(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Void : ConstantExpression {
    Void();
    pType bind(Parser &parser) override;
};

struct WhileStatement : SyntaxNode {
    Label       label;
    pSyntaxNode condition;
    pSyntaxNode statement;

    WhileStatement(Label label, pSyntaxNode condition, pSyntaxNode statement);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Yield : SyntaxNode {
    Label       label;
    pSyntaxNode statement;

    Yield(Label label, pSyntaxNode statement);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    void        header() override;
    void        dump_node(int indent) override;
};

pType bind_node(pSyntaxNode node, Parser &parser);

}
