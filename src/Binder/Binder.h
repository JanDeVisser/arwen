/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <format>
#include <ios>
#include <iostream>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
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
#include <Unescape.h>

namespace Arwen {

using BoundNodeReference = NodeReference;
using BoundNodeReferences = std::vector<BoundNodeReference>;

struct BindError {
    BoundNodeReference node;
    std::string        message;
};

struct BoundArrayType {
    TypeReference element_type;
    size_t        size;
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

struct BoundConstantDeclaration {
    std::string_view                  name;
    std::optional<BoundNodeReference> type;
    BoundNodeReference                initializer;
};

struct BoundForeignFunction {
    BoundNodeReference declaration;
    BoundNodeReference foreign_function;
};

struct BoundFunction {
    BoundNodeReference declaration;
    BoundNodeReference implementation;
};

struct BoundFunctionCall {
    std::string_view                  name;
    std::optional<BoundNodeReference> function;
    BoundNodeReferences               arguments;
};

struct BoundFunctionDecl {
    std::string_view                  name;
    BoundNodeReferences               parameters;
    std::optional<BoundNodeReference> return_type;
};

struct BoundIdentifier {
    std::string_view   name;
    BoundNodeReference declaration;
};

struct BoundIf {
    BoundNodeReference                condition;
    BoundNodeReference                true_branch;
    std::optional<BoundNodeReference> false_branch;
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
    TypeReference element_type;
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

#define BoundNodeImpls(S)        \
    S(BoundArrayType)            \
    S(BoundAssignmentExpression) \
    S(BasicTypeNode)             \
    S(BoundBinaryExpression)     \
    S(BoundBlock)                \
    S(BoolConstant)              \
    S(BoundConstantDeclaration)  \
    S(FloatConstant)             \
    S(BoundForeignFunction)      \
    S(BoundFunction)             \
    S(BoundFunctionCall)         \
    S(BoundFunctionDecl)         \
    S(BoundIdentifier)           \
    S(BoundIf)                   \
    S(IntConstant)               \
    S(BoundLoop)                 \
    S(BoundMember)               \
    S(BoundModule)               \
    S(BoundParameter)            \
    S(BoundPointerType)          \
    S(BoundProgram)              \
    S(BoundReturn)               \
    S(StringConstant)            \
    S(BoundSubscript)            \
    S(BoundUnaryExpression)      \
    S(BoundVariableDeclaration)

using BoundNodeImpl = std::variant<
#undef S
#define S(T) T,
    BoundNodeImpls(S)
#undef S
        BindError>;

struct BoundNode {
    std::optional<TypeReference>                  type {};
    BoundNodeReference                            ref { 0 };
    NodeReference                                 ast_ref;
    Location                                      location;
    std::map<std::string_view, NodeReference>     names;
    BoundNodeImpl                                 impl;
    static std::map<std::type_index, std::string> type_names;

    [[nodiscard]] std::string_view type_name() const
    {
        if (type_names.empty()) {
#undef S
#define S(T) type_names[std::type_index(typeid(T))] = #T;
            BoundNodeImpls(S)
#undef S
                type_names[std::type_index(typeid(BindError))]
                = "BindError";
        }
        return std::visit([](auto impl) -> std::string_view {
            using T = std::decay_t<decltype(impl)>;
            return type_names[std::type_index(typeid(T))];
        },
            impl);
    }

    template<typename Impl>
    static BoundNode make(NodeReference ast_ref, Location location)
    {
        auto ret = BoundNode { ast_ref, location };
        ret.impl = { Impl {} };
        return ret;
    };

    template<typename Impl>
    Impl &implementation()
    {
        return std::get<Impl>(impl);
    };

private:
    BoundNode(NodeReference ast_ref, Location location)
        : ast_ref(ast_ref)
        , location(location)
    {
    }
};

struct Binder {
    TypeRegistry                registry;
    std::vector<ASTNode> const &ast;
    std::vector<BoundNode>      bound_nodes;
    BoundNodeReferences         namespaces;
    int                         pass { 0 };
    BoundNodeReferences         errors;
    size_t                      unbound { 0 };
    BoundNodeReference          entrypoint;

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

    BoundNodeReference               bind_node(NodeReference ast_ref);
    BoundNodeReference               rebind_node(BoundNodeReference ref);
    Result<BoundNodeReference, bool> bind(NodeReference ast_entrypoint);
    void                             push_namespace(BoundNodeReference ref);
    void                             set_name(std::string_view name, BoundNodeReference ref);
    void                             pop_namespace();
    std::optional<NodeReference>     resolve(std::string_view name);
    BoundNodeReference               add_error(BoundNodeReference ref, std::string const &message);
    void                             dump(BoundNodeReference ref, std::string_view caption, int indent = 0);
    void                             dump(std::optional<BoundNodeReference> ref, std::string_view caption, int indent);
    void                             dump(BoundNodeReferences refs, std::string_view caption, int indent);
    void                             list();

    template<class Impl>
    BoundNodeReference add_node(NodeReference ref, Location location)
    {
        bound_nodes.push_back(BoundNode::make<Impl>(ref, location));
        bound_nodes.back().ref = bound_nodes.size() - 1;
        return bound_nodes.back().ref;
    }
};

#define I(Cls, Ref) (binder.impl<Cls>(Ref))
#define IMPL I(STRUCT, ref) //(binder.impl<STRUCT>(ref))

}
