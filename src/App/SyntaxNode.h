/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <concepts>
#include <cstddef>
#include <map>
#include <memory>
#include <ranges>
#include <string>
#include <vector>

#include <Util/Logging.h>
#include <Util/TokenLocation.h>

#include <App/Operator.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

using namespace Util;

#define SyntaxNodeTypes(S) \
    S(BinaryExpression)    \
    S(Block)               \
    S(BoolConstant)        \
    S(Break)               \
    S(Call)                \
    S(Constant)            \
    S(Continue)            \
    S(DeferStatement)      \
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
    S(Import)              \
    S(Insert)              \
    S(LoopStatement)       \
    S(Module)              \
    S(Nullptr)             \
    S(Number)              \
    S(Parameter)           \
    S(Program)             \
    S(PublicDeclaration)   \
    S(QuotedString)        \
    S(Return)              \
    S(StampedIdentifier)   \
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
using pExpressionList = std::shared_ptr<struct ExpressionList>;
using pFunctionDefinition = std::shared_ptr<struct FunctionDefinition>;

struct Parser;

using pNamespace = std::shared_ptr<struct Namespace>;

struct Namespace : std::enable_shared_from_this<Namespace> {
    using VariableMap = std::map<std::wstring, pSyntaxNode>;
    using TypeMap = std::map<std::wstring, pType>;
    using FunctionMap = std::multimap<std::wstring, pFunctionDefinition>;
    using FunctionIter = FunctionMap::iterator;
    using FunctionConstIter = FunctionMap::const_iterator;

    TypeMap     types {};
    FunctionMap functions {};
    VariableMap variables {};
    pSyntaxNode node { nullptr };
    pNamespace  parent { nullptr };

    explicit Namespace(pNamespace parent = nullptr);
    bool                             is_registered(std::wstring const &name) const;
    pType                            find_type(std::wstring const &name) const;
    bool                             has_type(std::wstring const &name) const;
    void                             register_type(std::wstring name, pType type);
    void                             register_function(std::wstring name, pFunctionDefinition fnc);
    bool                             has_function(std::wstring const &name) const;
    void                             unregister_function(std::wstring name, pFunctionDefinition fnc);
    pFunctionDefinition              find_function(std::wstring const &name, pType const &type) const;
    pFunctionDefinition              find_function_by_arg_list(std::wstring const &name, pType const &type) const;
    FunctionConstIter                find_function_here(std::wstring name, pType const &type) const;
    std::vector<pFunctionDefinition> find_overloads(std::wstring const &name, TypeSpecifications const &type_args) const;
    bool                             has_variable(std::wstring const &name) const;
    pSyntaxNode                      find_variable(std::wstring const &name) const;
    pType                            type_of(std::wstring const &name) const;
    void                             register_variable(std::wstring name, pSyntaxNode node);
};

struct SyntaxNode : std::enable_shared_from_this<SyntaxNode> {
    enum class Status {
        Initialized,
        Normalized,
        Bound,
        Undetermined,
        Ambiguous,
        BindErrors
    };

    TokenLocation  location;
    SyntaxNodeType type;
    Status         status { Status::Initialized };
    pType          bound_type;
    pNamespace     ns { nullptr };

    virtual ~SyntaxNode() = default;
    SyntaxNode() = delete;
    explicit SyntaxNode(SyntaxNodeType type, pNamespace ns = nullptr);

    void                   dump(int indent = 0);
    std::wostream         &header_line(std::wostream &os);
    virtual std::wostream &header(std::wostream &os);
    virtual void           dump_node(int indent);

    virtual pSyntaxNode normalize(Parser &parser);
    virtual pType       bind(Parser &parser);
    virtual pSyntaxNode stamp(Parser &parser);
    virtual pSyntaxNode coerce(pType const &target, Parser &parser);
};

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(TokenLocation location, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = std::move(location);
    // trace("[{}] ({},{})", SyntaxNodeType_name(ret->type), ret->location.line + 1, ret->location.column + 1);
    if (ret->ns != nullptr) {
        ret->ns->node = ret;
    }
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(pSyntaxNode const &child, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = child->location;
    if (ret->ns != nullptr) {
        ret->ns->node = ret;
    }
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(std::array<pSyntaxNode, 2> const &children, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = children[0]->location + children[1]->location;
    if (ret->ns != nullptr) {
        ret->ns->node = ret;
    }
    return ret;
}

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(std::vector<pSyntaxNode> const &children, Args &&...args)
{
    auto ret = std::make_shared<Node>(args...);
    ret->location = children[0]->location + children.back()->location;
    if (ret->ns != nullptr) {
        ret->ns->node = ret;
    }
    return ret;
}

pSyntaxNode              normalize_node_(pSyntaxNode const &node, Parser &parser);
std::vector<pSyntaxNode> normalize_nodes_(std::vector<pSyntaxNode> const &nodes, Parser &parser);
pSyntaxNode              stamp_node_(pSyntaxNode const &node, Parser &parser);
std::vector<pSyntaxNode> stamp_nodes_(std::vector<pSyntaxNode> const &nodes, Parser &parser);
pType                    bind_node(pSyntaxNode const &node, Parser &parser);

template<class Node, class Normalized = Node>
    requires std::derived_from<Node, SyntaxNode> && std::derived_from<Normalized, SyntaxNode>
static std::shared_ptr<Normalized> normalize_node(std::shared_ptr<Node> node, Parser &parser)
{
    auto ret = std::dynamic_pointer_cast<Normalized>(normalize_node_(node, parser));
    assert(ret != nullptr);
    return ret;
}

template<class Node>
    requires std::derived_from<Node, SyntaxNode>
static std::vector<std::shared_ptr<Node>> normalize_nodes(std::vector<std::shared_ptr<Node>> nodes, Parser &parser)
{
    auto normalized = normalize_nodes_(
        { std::from_range, nodes | std::views::transform([](auto const &n) {
             return std::dynamic_pointer_cast<SyntaxNode>(n);
         }) },
        parser);
    std::vector<std::shared_ptr<Node>> ret {
        std::from_range, normalized | std::views::transform([](auto const &n) {
            return std::dynamic_pointer_cast<Node>(n);
        })
    };
    return ret;
}

template<class Node, class Normalized = Node>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> stamp_node(std::shared_ptr<Node> node, Parser &parser)
{
    return node ? std::dynamic_pointer_cast<Normalized>(stamp_node_(node, parser)) : nullptr;
}

template<class Node>
    requires std::derived_from<Node, SyntaxNode>
static std::vector<std::shared_ptr<Node>> stamp_nodes(std::vector<std::shared_ptr<Node>> nodes, Parser &parser)
{
    auto stamped = stamp_nodes_(
        { std::from_range, nodes | std::views::transform([](auto const &n) {
             return std::dynamic_pointer_cast<SyntaxNode>(n);
         }) },
        parser);
    std::vector<std::shared_ptr<Node>> ret {
        std::from_range, stamped | std::views::transform([](auto const &n) {
            return std::dynamic_pointer_cast<Node>(n);
        })
    };
    return ret;
}

struct BinaryExpression final : SyntaxNode {
    pSyntaxNode lhs;
    Operator    op;
    pSyntaxNode rhs;

    BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs);

    pType          bind(Parser &parser) override;
    pSyntaxNode    normalize(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct Block final : SyntaxNode {
    SyntaxNodes statements;

    Block(SyntaxNodes statements, pNamespace const &ns);

    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

using pBlock = std::shared_ptr<Block>;

struct BoolConstant final : SyntaxNode {
    bool value;

    explicit BoolConstant(bool value);
    pSyntaxNode    normalize(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct Break final : SyntaxNode {
    Label label;

    explicit Break(Label label);

    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    pSyntaxNode    stamp(Parser &parser) override;
};

struct Call final : SyntaxNode {
    pSyntaxNode         callable;
    pExpressionList     arguments;
    pFunctionDefinition function;

    Call(pSyntaxNode callable, pExpressionList arguments);
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    void           dump_node(int indent) override;
    std::wostream &header(std::wostream &os) override;
};

using pConstant = std::shared_ptr<struct Constant>;

struct Constant final : SyntaxNode {
    std::optional<Value> bound_value {};

    explicit Constant(Value value);
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct Continue final : SyntaxNode {
    Label label;

    Continue(Label label);

    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct DeferStatement final : SyntaxNode {
    pSyntaxNode stmt;

    DeferStatement(pSyntaxNode stmt);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct Dummy final : SyntaxNode {
    Dummy();
    pType bind(Parser &parser) override;
};

struct Embed final : SyntaxNode {
    std::wstring file_name;

    Embed(std::wstring_view file_name);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct EnumValue final : SyntaxNode {
    std::wstring       label;
    pSyntaxNode        value;
    pTypeSpecification payload;

    EnumValue(std::wstring label, pSyntaxNode value, pTypeSpecification payload);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

using pEnumValue = std::shared_ptr<EnumValue>;
using EnumValues = std::vector<pEnumValue>;

struct Enum final : SyntaxNode {
    std::wstring       name;
    pTypeSpecification underlying_type;
    EnumValues         values;

    Enum(std::wstring name, pTypeSpecification underlying_type, EnumValues values);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    void           dump_node(int indent) override;
    std::wostream &header(std::wostream &os) override;
};

struct Error final : SyntaxNode {
    pSyntaxNode expression;

    Error(pSyntaxNode expression);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct ExpressionList final : SyntaxNode {
    SyntaxNodes expressions;

    explicit ExpressionList(SyntaxNodes expressions);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

using pIdentifier = std::shared_ptr<struct Identifier>;
using Identifiers = std::vector<pIdentifier>;

struct ExternLink final : SyntaxNode {
    std::wstring link_name;

    ExternLink(std::wstring link_name);
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct ForStatement final : SyntaxNode {
    std::wstring range_variable;
    pSyntaxNode  range_expr;
    pSyntaxNode  statement;

    ForStatement(std::wstring var, pSyntaxNode expr, pSyntaxNode stmt, pNamespace ns);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

using pParameter = std::shared_ptr<struct Parameter>;
using Parameters = std::vector<pParameter>;

struct FunctionDeclaration final : SyntaxNode {
    std::wstring       name;
    Identifiers        generics;
    Parameters         parameters;
    pTypeSpecification return_type;

    FunctionDeclaration(std::wstring name, Identifiers generics, Parameters parameters, pTypeSpecification return_type);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

using pFunctionDeclaration = std::shared_ptr<FunctionDeclaration>;

struct FunctionDefinition final : SyntaxNode {
    std::wstring         name;
    pFunctionDeclaration declaration;
    pSyntaxNode          implementation;

    FunctionDefinition(std::wstring name, pFunctionDeclaration declaration, pSyntaxNode implementation, pNamespace ns);
    pSyntaxNode         normalize(Parser &parser) override;
    pType               bind(Parser &parser) override;
    pSyntaxNode         stamp(Parser &parser) override;
    void                dump_node(int indent) override;
    pFunctionDefinition instantiate(Parser &parser, std::vector<pType> const &generic_args) const;
    pFunctionDefinition instantiate(Parser &parser, std::map<std::wstring, pType> const &generic_args) const;
};

struct Identifier final : SyntaxNode {
    std::wstring identifier;

    explicit Identifier(std::wstring_view identifier);
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct IfStatement final : SyntaxNode {
    pSyntaxNode condition;
    pSyntaxNode if_branch;
    pSyntaxNode else_branch;

    IfStatement(pSyntaxNode condition, pSyntaxNode if_branch, pSyntaxNode else_branch);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct Import final : SyntaxNode {
    std::wstring name;

    explicit Import(std::wstring name);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct Include final : SyntaxNode {
    std::wstring file_name;

    explicit Include(std::wstring_view file_name);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct Insert final : SyntaxNode {
    std::wstring script_text;

    explicit Insert(std::wstring_view script_text);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
};

struct LoopStatement final : SyntaxNode {
    Label       label;
    pSyntaxNode statement;

    LoopStatement(Label label, pSyntaxNode statement);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct Module final : SyntaxNode {
    std::wstring name;
    std::wstring source;
    SyntaxNodes  statements;

    Module(std::wstring name, std::wstring source, SyntaxNodes const &statements, pNamespace const &ns);
    // Module(std::wstring name, std::wstring source, pBlock statements);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

using pModule = std::shared_ptr<Module>;

struct Nullptr final : SyntaxNode {
    Nullptr();
    std::wostream &header(std::wostream &os) override;
    pSyntaxNode    normalize(Parser &) override;
};

struct Number final : SyntaxNode {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type);
    pSyntaxNode    normalize(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

using pNumber = std::shared_ptr<Number>;

struct Parameter final : SyntaxNode {
    std::wstring       name;
    pTypeSpecification type_name;

    Parameter(std::wstring name, pTypeSpecification type_name);
    pType          bind(Parser &parser) override;
    pSyntaxNode    normalize(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct Program final : SyntaxNode {
    std::wstring                    name;
    std::map<std::wstring, pModule> modules {};
    std::wstring                    source;
    SyntaxNodes                     statements;

    Program(std::wstring name, pNamespace ns);
    Program(std::wstring name, std::map<std::wstring, pModule> modules, SyntaxNodes statements, pNamespace ns);
    Program(std::wstring name, std::wstring source, SyntaxNodes statements, pNamespace ns);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct PublicDeclaration final : SyntaxNode {
    std::wstring name;
    pSyntaxNode  declaration;

    PublicDeclaration(std::wstring name, pSyntaxNode declaration);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct QuotedString final : SyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type);
    std::wostream &header(std::wostream &os) override;
    pSyntaxNode    normalize(Parser &parser) override;
};

struct Return final : SyntaxNode {
    pSyntaxNode expression;

    explicit Return(pSyntaxNode expression);
    pSyntaxNode normalize(Parser &parser) override;
    pType       bind(Parser &parser) override;
    pSyntaxNode stamp(Parser &parser) override;
    void        dump_node(int indent) override;
};

struct StampedIdentifier final : SyntaxNode {
    std::wstring       identifier;
    TypeSpecifications arguments;

    explicit StampedIdentifier(std::wstring_view identifier, TypeSpecifications arguments);
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

struct StructMember final : SyntaxNode {
    std::wstring       label;
    pTypeSpecification member_type;

    StructMember(std::wstring label, pTypeSpecification payload);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
};

using pStructMember = std::shared_ptr<StructMember>;
using StructMembers = std::vector<pStructMember>;

struct Struct final : SyntaxNode {
    std::wstring  name;
    StructMembers members;

    Struct(std::wstring name, StructMembers members);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    void           dump_node(int indent) override;
    std::wostream &header(std::wostream &os) override;
};

struct TypeNameNode {
    std::wstring       name;
    TypeSpecifications arguments {};
};

struct ReferenceDescriptionNode {
    pTypeSpecification referencing;
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

struct DynArrayDescriptionNode {
    pTypeSpecification array_of;
};

struct OptionalDescriptionNode {
    pTypeSpecification optional_of;
};

struct ErrorDescriptionNode {
    pTypeSpecification success;
    pTypeSpecification error;
};

using TypeSpecificationDescription = std::variant<
    TypeNameNode,
    ReferenceDescriptionNode,
    SliceDescriptionNode,
    ZeroTerminatedArrayDescriptionNode,
    ArrayDescriptionNode,
    DynArrayDescriptionNode,
    OptionalDescriptionNode,
    ErrorDescriptionNode>;

struct TypeSpecification final : SyntaxNode {

    TypeSpecificationDescription description;

    TypeSpecification(TypeSpecificationDescription description);
    TypeSpecification(TypeNameNode type);
    TypeSpecification(ReferenceDescriptionNode slice);
    TypeSpecification(SliceDescriptionNode slice);
    TypeSpecification(ZeroTerminatedArrayDescriptionNode array);
    TypeSpecification(ArrayDescriptionNode array);
    TypeSpecification(DynArrayDescriptionNode array);
    TypeSpecification(OptionalDescriptionNode optional);
    TypeSpecification(ErrorDescriptionNode error);

    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    std::wstring   to_string();
    pType          resolve(Parser &parser);
};

struct UnaryExpression final : SyntaxNode {
    Operator    op;
    pSyntaxNode operand;

    UnaryExpression(Operator op, pSyntaxNode operand);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct VariableDeclaration final : SyntaxNode {
    std::wstring       name;
    pTypeSpecification type_name {};
    pSyntaxNode        initializer;
    bool               is_const;

    VariableDeclaration(std::wstring name, pTypeSpecification type_name, pSyntaxNode initializer, bool is_const);
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    pSyntaxNode    normalize(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct Void final : SyntaxNode {
    Void();
    pType bind(Parser &parser) override;
};

struct WhileStatement final : SyntaxNode {
    Label       label;
    pSyntaxNode condition;
    pSyntaxNode statement;

    WhileStatement(Label label, pSyntaxNode condition, pSyntaxNode statement);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

struct Yield final : SyntaxNode {
    Label       label;
    pSyntaxNode statement;

    Yield(Label label, pSyntaxNode statement);
    pSyntaxNode    normalize(Parser &parser) override;
    pType          bind(Parser &parser) override;
    pSyntaxNode    stamp(Parser &parser) override;
    std::wostream &header(std::wostream &os) override;
    void           dump_node(int indent) override;
};

}

std::wostream &operator<<(std::wostream &os, Arwen::SyntaxNode const &node);
