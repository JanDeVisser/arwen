/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <typeindex>
#include <variant>
#include <vector>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Lexer/Lexer.h>
#include <Type/Type.h>

#include <Lib.h>
#include <Logging.h>
#include <Result.h>
#include <ScopeGuard.h>
#include <TaggedUnion.h>
#include <Unescape.h>

namespace Arwen {

using BoundNodeReference = NodeReference;
using BoundNodeReferences = std::vector<BoundNodeReference>;

struct TypeAlternatives {
    TypeReference              preferred;
    std::vector<TypeReference> alternatives;
};

struct BindError {
    BoundNodeReference node;
    std::string        message;
};

struct BoundArrayType {
    TypeReference      element_type;
    BoundNodeReference size;
};

struct BoundAssignmentExpression {
    BoundNodeReference left;
    BinaryOperator     op { BinaryOperator::None };
    BoundNodeReference right;
};

struct BoundBinaryExpression {
    BoundNodeReference left;
    BinaryOperator     op { BinaryOperator::None };
    BoundNodeReference right;
};

struct BoundBlock {
    std::optional<std::string_view> label;
    BoundNodeReferences             statements;
};

struct BoundCoercion {
    BoundNodeReference expression;
};

struct BoundConstantDeclaration {
    std::string_view                  name;
    std::optional<BoundNodeReference> type;
    BoundNodeReference                initializer;
};

struct BoundForeignFunction {
    std::string_view foreign_name;
};

struct BoundFunction {
    std::string_view                  name;
    BoundNodeReferences               parameters;
    std::optional<BoundNodeReference> return_type;
    BoundNodeReference                implementation;
};

struct BoundFunctionCall {
    std::string_view                  name;
    bool                              discard_result { false };
    std::optional<BoundNodeReference> function = {};
    BoundNodeReferences               arguments;
};

struct BoundFunctionImplementation {
    std::string_view   name;
    BoundNodeReference implementation;
};

struct BoundIdentifier {
    std::string_view                  name;
    std::optional<BoundNodeReference> declaration = {};
};

struct BoundIf {
    BoundNodeReference                condition;
    BoundNodeReference                true_branch;
    std::optional<BoundNodeReference> false_branch;
};

struct BoundIntrinsic {
    std::string_view name;
};

struct BoundLoop {
    BoundNodeReference body;
};

struct BoundMember {
    std::string_view name;
};

struct BoundModule {
    std::string_view    name;
    BoundNodeReferences names;
};

struct BoundParameter {
    std::string_view   name;
    BoundNodeReference type;
};

struct BoundPointerType {
    BoundNodeReference element_type;
};

struct BoundProgram {
    BoundNodeReferences modules;
};

struct BoundReturn {
    std::optional<BoundNodeReference> expression;
};

struct BoundSubscript {
    BoundNodeReferences subscripts;
};

struct BoundUnaryExpression {
    UnaryOperator      op { UnaryOperator::None };
    BoundNodeReference operand;
};

struct BoundVariableDeclaration {
    std::string_view                  name;
    std::optional<BoundNodeReference> type;
    std::optional<BoundNodeReference> initializer;
};

struct BoundWhile {
    BoundNodeReference condition;
    BoundNodeReference body;
};

#define BoundNodeImpls(S)          \
    S(BoundArrayType)              \
    S(BoundAssignmentExpression)   \
    S(BasicTypeNode)               \
    S(BoundBinaryExpression)       \
    S(BoundBlock)                  \
    S(BoundCoercion)               \
    S(BoolConstant)                \
    S(BoundConstantDeclaration)    \
    S(FloatConstant)               \
    S(BoundForeignFunction)        \
    S(BoundFunction)               \
    S(BoundFunctionCall)           \
    S(BoundFunctionImplementation) \
    S(BoundIdentifier)             \
    S(BoundIf)                     \
    S(BoundIntrinsic)              \
    S(IntConstant)                 \
    S(BoundLoop)                   \
    S(BoundMember)                 \
    S(BoundModule)                 \
    S(Nullptr)                     \
    S(BoundParameter)              \
    S(BoundPointerType)            \
    S(BoundProgram)                \
    S(BoundReturn)                 \
    S(StringConstant)              \
    S(BoundSubscript)              \
    S(BoundUnaryExpression)        \
    S(BoundVariableDeclaration)    \
    S(BoundWhile)

#undef S
#define S(T) T,
enum class BoundNodeType {
    BoundNodeImpls(S)
};

using BoundNodeImpl = std::variant<
    BoundNodeImpls(S)
#undef S
        BindError>;

struct BoundNode {
    std::optional<TypeReference>                  type {};
    BoundNodeReference                            ref { 0 };
    BoundNodeReference                            parent { 0 };
    NodeReference                                 ast_ref;
    Location                                      location;
    std::map<std::string_view, NodeReference>     names;
    BoundNodeImpl                                 impl;
    static std::map<std::type_index, std::string> type_names;

    [[nodiscard]] std::string_view type_name() const;

    template<typename Impl>
    static BoundNode make(NodeReference parent, NodeReference ast_ref, Location location)
    {
        auto ret = BoundNode { parent, ast_ref, location };
        ret.impl = { Impl {} };
        return ret;
    };

    template<typename Impl>
    Impl &implementation()
    {
        return std::get<Impl>(impl);
    };

private:
    BoundNode(BoundNodeReference parent, NodeReference ast_ref, Location location)
        : parent(parent)
        , ast_ref(ast_ref)
        , location(location)
    {
    }
};

struct Binder {
    TypeRegistry                &registry;
    std::vector<ASTNode> const  &ast;
    std::vector<BoundNode>       bound_nodes;
    BoundNodeReferences          namespaces;
    int                          pass { 0 };
    BoundNodeReferences          errors;
    size_t                       unbound { 0 };
    BoundNodeReference           entrypoint;
    bool                         log { false };
    std::set<BoundNodeReference> visited;
    std::set<BoundNodeReference> still_unbound;

    explicit Binder(std::vector<ASTNode> const &ast)
        : registry(TypeRegistry::the())
        , ast(ast)
    {
    }

    BoundNode &operator[](BoundNodeReference ref)
    {
        return bound_nodes[ref];
    }

    template<typename Impl>
    Impl &impl(BoundNodeReference ref)
    {
        return bound_nodes[ref].implementation<Impl>();
    };

    TypeAlternatives                 alternatives(NodeReference ref, TypeReference hint);
    BoundNodeReference               accept(NodeReference ref, TypeReference type);
    BoundNodeReference               bind_node(NodeReference ast_ref, BoundNodeReference parent = 0);
    BoundNodeReference               rebind_node(BoundNodeReference ref);
    Result<BoundNodeReference, bool> bind(NodeReference ast_entrypoint);
    BoundNodeType                    type_of(NodeReference ref) const;
    void                             push_namespace(BoundNodeReference ref);
    void                             set_name(std::string_view name, BoundNodeReference ref);
    void                             pop_namespace();
    std::optional<NodeReference>     resolve(std::string_view name);
    void                             to_string(std::ostream &out, BoundNodeReference ref);
    void                             dump(std::ostream &out, BoundNodeReference ref, std::string_view caption, int indent = 0);
    void                             dump(std::ostream &out, std::optional<BoundNodeReference> ref, std::string_view caption, int indent);
    void                             dump(std::ostream &out, BoundNodeReferences refs, std::string_view caption, int indent);
    void                             list(std::ostream &out = std::cout);
};

#define I(Cls, Ref) (binder.impl<Cls>(Ref))
#define IMPL I(STRUCT, ref) //(binder.impl<STRUCT>(ref))

}
