/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include <App/Operator.h>
#include <Util/Token.h>

namespace Arwen {

using namespace Util;

#define SyntaxNodeTypes(S) \
    S(BinaryExpression)    \
    S(Block)               \
    S(Break)               \
    S(Continue)            \
    S(Dummy)               \
    S(ExpressionList)      \
    S(Identifier)          \
    S(IfStatement)         \
    S(LoopStatement)       \
    S(Module)              \
    S(Number)              \
    S(QuotedString)        \
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

struct Block : SyntaxNode {
    SyntaxNodes statements;

    Block(SyntaxNodes statements);

    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        dump_node(int indent) override;
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

struct Dummy : SyntaxNode {
    Dummy();
    pBoundNode bind() override;
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
    std::string name;
    pSyntaxNode statements;

    Module(std::string_view name, SyntaxNodes statements);
    Module(std::string_view name, pSyntaxNode statement);
    pSyntaxNode normalize() override;
    pBoundNode  bind() override;
    void        header() override;
    void        dump_node(int indent) override;
};

struct Number : SyntaxNode {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type);
    pBoundNode bind() override;
    void       header() override;
};

struct QuotedString : SyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type);
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
