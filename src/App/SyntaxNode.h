/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <App/Operator.h>

namespace Arwen {

using namespace Util;

#define SyntaxNodeTypes(S) \
    S(BinaryExpression)    \
    S(Block)               \
    S(BoolConstant)        \
    S(Break)               \
    S(Continue)            \
    S(Decimal)             \
    S(DoubleQuotedString)  \
    S(Dummy)               \
    S(Embed)               \
    S(ExpressionList)      \
    S(Identifier)          \
    S(IfStatement)         \
    S(Include)             \
    S(Integer)             \
    S(LoopStatement)       \
    S(Module)              \
    S(Number)              \
    S(QuotedString)        \
    S(SingleQuotedString)  \
    S(UnaryExpression)     \
    S(WhileStatement)

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

struct SyntaxNode : std::enable_shared_from_this<SyntaxNode> {
    TokenLocation  location;
    SyntaxNodeType type;

    virtual ~SyntaxNode() = default;
    SyntaxNode() = delete;
    SyntaxNode(SyntaxNodeType type);

    void         dump(int indent = 0);
    virtual void header();
    virtual void dump_node(int indent);

    virtual pSyntaxNode normalize();
    virtual pBoundNode  bind() = 0;
};

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(Args &&...args)
{
    return std::make_shared<Node>(args...);
}

struct BinaryExpression : SyntaxNode {
    pSyntaxNode lhs;
    Operator    op;
    pSyntaxNode rhs;

    BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs);

    pBoundNode  bind() override;
    pSyntaxNode normalize() override;
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
            this->shared_from_this(), Operator::O, rhs);             \
    }
    Operators(S)
#undef S
};

struct Block : SyntaxNode {
    SyntaxNodes statements;

    Block(SyntaxNodes statements);

    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        dump_node(int indent) override;
};

struct BoolConstant : ConstantExpression {
    bool value;

    BoolConstant(bool value);
    pBoundNode bind() override;
    void       header() override;
};

struct Break : SyntaxNode {
    Label label;

    Break(Label label);

    pBoundNode bind() override;
    void       header() override;
};

struct Continue : SyntaxNode {
    Label label;

    Continue(Label label);

    pBoundNode bind() override;
    void       header() override;
};

struct Decimal : ConstantExpression {
    double value;

    Decimal(std::wstring const &number);
    Decimal(double number);
    pBoundNode bind() override;
    void       header() override;

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

struct DoubleQuotedString : ConstantExpression {
    std::wstring string;

    DoubleQuotedString(std::wstring_view str, bool strip_quotes);
    pBoundNode  bind() override;
    void        header() override;
    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override;
};

struct Dummy : SyntaxNode {
    Dummy();
    pBoundNode bind() override;
};

struct Embed : SyntaxNode {
    std::wstring file_name;

    Embed(std::wstring_view file_name);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
};

struct ExpressionList : SyntaxNode {
    SyntaxNodes expressions;

    ExpressionList(SyntaxNodes expressions);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        dump_node(int indent) override;
};

struct Identifier : SyntaxNode {
    std::wstring identifier;

    Identifier(std::wstring_view identifier);
    pBoundNode bind() override;
    void       header() override;
};

struct IfStatement : SyntaxNode {
    pSyntaxNode condition;
    pSyntaxNode if_branch;
    pSyntaxNode else_branch;

    IfStatement(pSyntaxNode condition, pSyntaxNode if_branch, pSyntaxNode else_branch);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        dump_node(int indent) override;
};

struct Include : SyntaxNode {
    std::wstring file_name;

    Include(std::wstring_view file_name);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
};

struct Integer : ConstantExpression {
    uint64_t value;

    Integer(std::wstring_view number);
    Integer(uint64_t number);
    pBoundNode bind() override;
    void       header() override;

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
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Module : SyntaxNode {
    std::string       name;
    std::wstring_view source;
    pSyntaxNode       statements;

    Module(std::string_view name, std::wstring_view source, SyntaxNodes statements);
    Module(std::string_view name, std::wstring_view source, pSyntaxNode statement);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Number : SyntaxNode {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
};

struct QuotedString : SyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type);
    pBoundNode  bind() override;
    void        header() override;
    pSyntaxNode normalize() override;
};

struct SingleQuotedString : ConstantExpression {
    std::wstring string;

    SingleQuotedString(std::wstring_view str, bool strip_quotes);
    pBoundNode bind() override;
    void       header() override;
};

struct UnaryExpression : SyntaxNode {
    Operator    op;
    pSyntaxNode operand;

    UnaryExpression(Operator op, pSyntaxNode operand);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct WhileStatement : SyntaxNode {
    Label       label;
    pSyntaxNode condition;
    pSyntaxNode statement;

    WhileStatement(Label label, pSyntaxNode condition, pSyntaxNode statement);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
    void        dump_node(int indent) override;
};

}
