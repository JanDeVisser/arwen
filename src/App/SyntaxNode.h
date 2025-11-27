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
#include <ostream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <Util/Ptr.h>
#include <Util/TokenLocation.h>

#include <App/Operator.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

using namespace Util;

#define SyntaxNodeTypes(S) \
    S(Dummy)               \
    S(BinaryExpression)    \
    S(Block)               \
    S(BoolConstant)        \
    S(Break)               \
    S(Call)                \
    S(Constant)            \
    S(Continue)            \
    S(DeferStatement)      \
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

using Label = std::optional<std::wstring>;

struct Parser;

struct ASTNode : public Ptr<struct ASTNodeImpl, Parser> {
    ASTNode(Parser *parser, size_t id)
        : Ptr(parser, id)
    {
    }

    ASTNode(Parser *parser)
        : Ptr(parser)
    {
    }

    ASTNode() = default;

    ASTNode(std::nullptr_t const &n)
        : Ptr(n)
    {
    }

    TokenLocation operator+(ASTNode const &other);
    TokenLocation operator+(TokenLocation const &other);
    void          error(char const *message) const { error(std::string(message)); }
    void          error(wchar_t const *message) const { error(std::wstring(message)); }
    void          error(std::wstring const &message) const;
    void          error(std::string const &message) const;

    template<typename... Args>
    void error(std::format_string<Args...> const message, Args &&...args) const
    {
        error(std::vformat(message.get(), std::make_format_args(args...)));
    }

    template<typename... Args>
    void error(std::wformat_string<Args...> const message, Args &&...args) const
    {
        error(std::vformat(message.get(), std::make_wformat_args(args...)));
    }

    pType bind_error(std::wstring const &message) const;
    pType bind_error(wchar_t const *message) const { return bind_error(std::wstring(message)); }

    template<typename... Args>
    pType bind_error(std::wformat_string<Args...> const message, Args &&...args) const
    {
        return bind_error(std::vformat(message.get(), std::make_wformat_args(args...)));
    }
};

using ASTNodes = std::vector<ASTNode>;

struct AbstractSyntaxNode {
    pType          bind(ASTNode const &n);
    ASTNode        normalize(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &, std::wostream &os);
    void           dump_node(ASTNode const &, int);
    ASTNode        coerce(ASTNode const &n, pType const &target);
};

struct Namespace : std::enable_shared_from_this<Namespace> {
    using VariableMap = std::map<std::wstring, ASTNode>;
    using TypeMap = std::map<std::wstring, pType>;
    using FunctionMap = std::multimap<std::wstring, ASTNode>;
    using FunctionIter = FunctionMap::iterator;
    using FunctionConstIter = FunctionMap::const_iterator;

    TypeMap     types {};
    FunctionMap functions {};
    VariableMap variables {};
    ASTNode     parent { nullptr };

    explicit Namespace(ASTNode parent = nullptr);
    bool              is_registered(std::wstring const &name) const;
    pType             find_type(std::wstring const &name) const;
    bool              has_type(std::wstring const &name) const;
    void              register_type(std::wstring name, pType type);
    void              register_function(std::wstring name, ASTNode fnc);
    bool              has_function(std::wstring const &name) const;
    void              unregister_function(std::wstring name, ASTNode fnc);
    ASTNode           find_function(std::wstring const &name, pType const &type) const;
    ASTNode           find_function_by_arg_list(std::wstring const &name, pType const &type) const;
    FunctionConstIter find_function_here(std::wstring name, pType const &type) const;
    ASTNodes          find_overloads(std::wstring const &name, ASTNodes const &type_args) const;
    bool              has_variable(std::wstring const &name) const;
    ASTNode           find_variable(std::wstring const &name) const;
    pType             type_of(std::wstring const &name) const;
    void              register_variable(std::wstring name, ASTNode node);
};

struct BinaryExpression final : AbstractSyntaxNode {
    ASTNode  lhs;
    Operator op;
    ASTNode  rhs;

    BinaryExpression(ASTNode lhs, Operator op, ASTNode rhs);

    pType          bind(ASTNode const &n);
    ASTNode        normalize(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct Block final : AbstractSyntaxNode {
    ASTNodes statements;

    Block() = default;
    Block(ASTNodes statements);

    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct BoolConstant final : AbstractSyntaxNode {
    bool value;

    explicit BoolConstant(bool value);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Break final : AbstractSyntaxNode {
    Label label;

    explicit Break(Label label);

    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Call final : AbstractSyntaxNode {
    ASTNode callable;
    ASTNode arguments;
    ASTNode function;

    Call(ASTNode callable, ASTNode arguments);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    void           dump_node(ASTNode const &n, int indent);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Constant final : AbstractSyntaxNode {
    std::optional<Value> bound_value {};

    explicit Constant(Value value);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Continue final : AbstractSyntaxNode {
    Label label;

    Continue(Label label);

    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct DeferStatement final : AbstractSyntaxNode {
    ASTNode stmt;

    DeferStatement(ASTNode stmt);
    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct Dummy final : AbstractSyntaxNode {
    Dummy() = default;
    pType bind(ASTNode const &n);
};

struct Embed final : AbstractSyntaxNode {
    std::wstring file_name;

    Embed(std::wstring_view file_name);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct EnumValue final : AbstractSyntaxNode {
    std::wstring label;
    ASTNode      value;
    ASTNode      payload;

    EnumValue(std::wstring label, ASTNode value, ASTNode payload);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Enum final : AbstractSyntaxNode {
    std::wstring name;
    ASTNode      underlying_type;
    ASTNodes     values;

    Enum(std::wstring name, ASTNode underlying_type, ASTNodes values);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    void           dump_node(ASTNode const &n, int indent);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Error final : AbstractSyntaxNode {
    ASTNode expression;

    Error(ASTNode expression);
    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct ExpressionList final : AbstractSyntaxNode {
    ASTNodes expressions;

    explicit ExpressionList(ASTNodes expressions);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct ExternLink final : AbstractSyntaxNode {
    std::wstring link_name;

    ExternLink(std::wstring link_name);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct ForStatement final : AbstractSyntaxNode {
    std::wstring range_variable;
    ASTNode      range_expr;
    ASTNode      statement;

    ForStatement(std::wstring var, ASTNode expr, ASTNode stmt);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct FunctionDeclaration final : AbstractSyntaxNode {
    std::wstring name;
    ASTNodes     generics;
    ASTNodes     parameters;
    ASTNode      return_type;

    FunctionDeclaration(std::wstring name, ASTNodes generics, ASTNodes parameters, ASTNode return_type);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct FunctionDefinition final : AbstractSyntaxNode {
    std::wstring name;
    ASTNode      declaration;
    ASTNode      implementation;

    FunctionDefinition(std::wstring name, ASTNode declaration, ASTNode implementation);
    FunctionDefinition(std::wstring name);

    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
    ASTNode instantiate(ASTNode const &n, std::vector<pType> const &generic_args) const;
    ASTNode instantiate(ASTNode const &n, std::map<std::wstring, pType> const &generic_args) const;
};

struct Identifier final : AbstractSyntaxNode {
    std::wstring identifier;

    explicit Identifier(std::wstring_view identifier);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct IfStatement final : AbstractSyntaxNode {
    ASTNode condition;
    ASTNode if_branch;
    ASTNode else_branch;

    IfStatement(ASTNode condition, ASTNode if_branch, ASTNode else_branch);
    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct Import final : AbstractSyntaxNode {
    std::wstring name;

    explicit Import(std::wstring name);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Include final : AbstractSyntaxNode {
    std::wstring file_name;

    explicit Include(std::wstring_view file_name);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Insert final : AbstractSyntaxNode {
    std::wstring script_text;

    explicit Insert(std::wstring_view script_text);
    ASTNode normalize(ASTNode const &n);
};

struct LoopStatement final : AbstractSyntaxNode {
    Label   label;
    ASTNode statement;

    LoopStatement(Label label, ASTNode statement);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct Module final : AbstractSyntaxNode {
    std::wstring name;
    std::wstring source;
    ASTNodes     statements;

    Module(std::wstring name, std::wstring source);
    Module(std::wstring name, std::wstring source, ASTNodes const &statements);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct Nullptr final : AbstractSyntaxNode {
    Nullptr();
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Number final : AbstractSyntaxNode {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Parameter final : AbstractSyntaxNode {
    std::wstring name;
    ASTNode      type_name;

    Parameter(std::wstring name, ASTNode type_name);
    pType          bind(ASTNode const &n);
    ASTNode        normalize(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Program final : AbstractSyntaxNode {
    std::wstring                    name;
    std::map<std::wstring, ASTNode> modules {};
    std::wstring                    source;
    ASTNodes                        statements;

    Program(std::wstring name, std::wstring source);
    Program(std::wstring name, std::map<std::wstring, ASTNode> modules, ASTNodes statements);
    Program(std::wstring name, std::wstring source, ASTNodes statements);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct PublicDeclaration final : AbstractSyntaxNode {
    std::wstring name;
    ASTNode      declaration;

    PublicDeclaration(std::wstring name, ASTNode declaration);
    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct QuotedString final : AbstractSyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    ASTNode        normalize(ASTNode const &n);
};

struct Return final : AbstractSyntaxNode {
    ASTNode expression;

    explicit Return(ASTNode expression);
    ASTNode normalize(ASTNode const &n);
    pType   bind(ASTNode const &n);
    ASTNode stamp(ASTNode const &n);
    void    dump_node(ASTNode const &n, int indent);
};

struct StampedIdentifier final : AbstractSyntaxNode {
    std::wstring identifier;
    ASTNodes     arguments;

    explicit StampedIdentifier(std::wstring_view identifier, ASTNodes arguments);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct StructMember final : AbstractSyntaxNode {
    std::wstring label;
    ASTNode      member_type;

    StructMember(std::wstring label, ASTNode payload);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct Struct final : AbstractSyntaxNode {
    std::wstring name;
    ASTNodes     members;

    Struct(std::wstring name, ASTNodes members);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    void           dump_node(ASTNode const &n, int indent);
    std::wostream &header(ASTNode const &n, std::wostream &os);
};

struct TypeNameNode {
    std::wstring name;
    ASTNodes     arguments {};
};

struct ReferenceDescriptionNode {
    ASTNode referencing;
};

struct SliceDescriptionNode {
    ASTNode slice_of;
};

struct ZeroTerminatedArrayDescriptionNode {
    ASTNode array_of;
};

struct ArrayDescriptionNode {
    ASTNode array_of;
    size_t  size;
};

struct DynArrayDescriptionNode {
    ASTNode array_of;
};

struct OptionalDescriptionNode {
    ASTNode optional_of;
};

struct ErrorDescriptionNode {
    ASTNode success;
    ASTNode error;
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

template<class S>
concept is_type_specification = std::is_same_v<S, TypeNameNode>
    || std::is_same_v<S, ReferenceDescriptionNode>
    || std::is_same_v<S, SliceDescriptionNode>
    || std::is_same_v<S, ZeroTerminatedArrayDescriptionNode>
    || std::is_same_v<S, ArrayDescriptionNode>
    || std::is_same_v<S, DynArrayDescriptionNode>
    || std::is_same_v<S, OptionalDescriptionNode>
    || std::is_same_v<S, ErrorDescriptionNode>;

struct TypeSpecification final : AbstractSyntaxNode {

    TypeSpecificationDescription description;

    TypeSpecification(TypeSpecificationDescription description);

    template<class S>
        requires is_type_specification<S>
    TypeSpecification(S specification)
        : description(specification)
    {
    }

    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    std::wstring   to_string();
    pType          resolve(ASTNode const &n);
};

struct UnaryExpression final : AbstractSyntaxNode {
    Operator op;
    ASTNode  operand;

    UnaryExpression(Operator op, ASTNode operand);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct VariableDeclaration final : AbstractSyntaxNode {
    std::wstring name;
    ASTNode      type_name {};
    ASTNode      initializer;
    bool         is_const;

    VariableDeclaration(std::wstring name, ASTNode type_name, ASTNode initializer, bool is_const);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    ASTNode        normalize(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct Void final : AbstractSyntaxNode {
    Void();
    pType bind(ASTNode const &n);
};

struct WhileStatement final : AbstractSyntaxNode {
    Label   label;
    ASTNode condition;
    ASTNode statement;

    WhileStatement(Label label, ASTNode condition, ASTNode statement);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

struct Yield final : AbstractSyntaxNode {
    Label   label;
    ASTNode statement;

    Yield(Label label, ASTNode statement);
    ASTNode        normalize(ASTNode const &n);
    pType          bind(ASTNode const &n);
    ASTNode        stamp(ASTNode const &n);
    std::wostream &header(ASTNode const &n, std::wostream &os);
    void           dump_node(ASTNode const &n, int indent);
};

template<class N>
concept is_component = std::is_same_v<N, Program> || std::is_same_v<N, Module>;

template<class N>
concept is_identifier = std::is_same_v<N, Identifier> || std::is_same_v<N, StampedIdentifier>;

template<class N>
concept has_name = std::is_same_v<N, Module>
    || std::is_same_v<N, FunctionDeclaration>
    || std::is_same_v<N, VariableDeclaration>;

using SyntaxNode = std::variant<
    Dummy,
    BinaryExpression,
    Block,
    BoolConstant,
    Break,
    Call,
    Constant,
    Continue,
    DeferStatement,
    Embed,
    Enum,
    EnumValue,
    Error,
    ExpressionList,
    ExternLink,
    ForStatement,
    FunctionDeclaration,
    FunctionDefinition,
    Identifier,
    IfStatement,
    Include,
    Import,
    Insert,
    LoopStatement,
    Module,
    Nullptr,
    Number,
    Parameter,
    Program,
    PublicDeclaration,
    QuotedString,
    Return,
    StampedIdentifier,
    Struct,
    StructMember,
    TypeSpecification,
    UnaryExpression,
    VariableDeclaration,
    Void,
    WhileStatement,
    Yield>;

struct ASTNodeImpl {
    enum class Status {
        Initialized,
        Normalized,
        Bound,
        Undetermined,
        Ambiguous,
        BindErrors
    };

    TokenLocation            location {};
    Status                   status { Status::Initialized };
    SyntaxNode               node {};
    pType                    bound_type {};
    std::optional<Namespace> ns {};
    ASTNode                  id;

    template<class N, typename... Args>
        requires std::derived_from<N, AbstractSyntaxNode>
    static ASTNodeImpl make(TokenLocation const &loc, Args... args)
    {
        ASTNodeImpl ret = make<N>(loc, args...);
        ret.location = loc;
        return ret;
    }

    template<class N, typename... Args>
        requires std::derived_from<N, AbstractSyntaxNode>
    static ASTNodeImpl make(Args... args)
    {
        ASTNodeImpl ret;
        ret.node = N { args... };
        return ret;
    }

    [[nodiscard]] SyntaxNodeType type() const { return static_cast<SyntaxNodeType>(node.index()); }
    void                         init_namespace();
    void                         dump(int indent = 0);
    std::wostream               &header_line(std::wostream &os);
    std::wostream               &header(std::wostream &os);

    ASTNode clone();
    ASTNode normalize();
    pType   bind();
    ASTNode stamp();
    ASTNode coerce(pType const &target);

private:
    ASTNodeImpl() = default;
};

template<class N>
N *get_if(ASTNode const &node)
{
    assert(node);
    return std::get_if<N>(&node->node);
}

template<class N>
N &get(ASTNode const &node)
{
    assert(node);
    return std::get<N>(node->node);
}

template<class N>
bool is(ASTNode const &node)
{
    assert(node);
    return std::holds_alternative<N>(node->node);
}

static inline std::wstring_view identifier(ASTNode const &n)
{
    return std::visit(
        overloads {
            [](is_identifier auto const &node) -> std::wstring_view {
                return std::wstring_view(node.identifier);
            },
            [](auto const &node) -> std::wstring_view {
                UNREACHABLE();
            } },
        n->node);
}

static inline std::wstring_view name(ASTNode const &n)
{
    return std::visit(
        overloads {
            [](has_name auto const &node) -> std::wstring_view {
                return std::wstring_view(node.name);
            },
            [](auto const &node) -> std::wstring_view {
                UNREACHABLE();
            } },
        n->node);
}

void     normalize_nodes(ASTNodes &nodes);
ASTNodes stamp_nodes(ASTNodes const &nodes);
pType    bind_nodes(ASTNodes const &nodes);

}

std::wostream &operator<<(std::wostream &os, Arwen::SyntaxNode const &node);
