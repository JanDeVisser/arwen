/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <memory>
#include <string>
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
using pInteger = std::shared_ptr<struct Integer>;
using pNumber = std::shared_ptr<struct Number>;
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
    pSyntaxNode evaluate_binop(Operator op, pConstantExpression const &rhs);

#undef S
#undef S
#define S(O)                                                         \
    virtual pSyntaxNode evaluate_##O(pConstantExpression const &rhs) \
    {                                                                \
        return make_node<BinaryExpression>(                          \
            this->location + rhs->location,                          \
            this->shared_from_this(), Operator::O, rhs);             \
    }
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
    pType bind(Parser &parser) override;
    void  header() override;
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

struct Decimal : ConstantExpression {
    double value;

    Decimal(std::wstring const &number);
    Decimal(double number);
    pType bind(Parser &parser) override;
    void  header() override;

    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Subtract(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Multiply(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Divide(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Modulo(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Equals(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_NotEqual(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Less(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_LessEqual(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Greater(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_GreaterEqual(pConstantExpression const &rhs) override;
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

struct Integer : ConstantExpression {
    uint64_t value;

    Integer(std::wstring_view number);
    Integer(uint64_t number);
    pType bind(Parser &parser) override;
    void  header() override;

    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Subtract(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Multiply(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Divide(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Modulo(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Equals(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_NotEqual(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Less(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_LessEqual(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_Greater(pConstantExpression const &rhs) override;
    pSyntaxNode evaluate_GreaterEqual(pConstantExpression const &rhs) override;
};

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
