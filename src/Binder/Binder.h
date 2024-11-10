/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <optional>
#include <typeindex>
#include <typeinfo>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Binder/Type.h>
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

struct BoundParameter {
    std::string_view   name;
    BoundNodeReference type;
};

struct BoundPointerType {
    TypeReference element_type;
};

struct BoundProgram {
    BoundNodeReferences declarations;
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
    S(BasicType)                 \
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
    Result<BoundNodeReference, bool> bind(NodeReference entrypoint);
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

template<typename AstImpl>
inline BoundNodeReference bind([[maybe_unused]] Binder &, [[maybe_unused]] NodeReference)
{
    UNREACHABLE();
}

template<typename T>
inline BoundNodeReference rebind([[maybe_unused]] Binder &, BoundNodeReference ref)
{
    return ref;
}

template<typename T>
inline void dump(Binder &, T const &, int)
{
}

template<>
inline void dump(Binder &binder, BindError const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << "ERROR: " << impl.message << "\n";
    binder.dump(impl.node, "Node", indent + 2);
}

#define I(Cls, Ref) (binder.impl<Cls>(Ref))
#define IMPL I(STRUCT, ref) //(binder.impl<STRUCT>(ref))

#undef STRUCT
#define STRUCT BoundAssignmentExpression

template<>
inline BoundNodeReference bind<AssignmentExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<AssignmentExpression>(ast_node.impl);
    BoundNodeReference ref = binder.add_node<BoundAssignmentExpression>(ast_node.ref, ast_node.location);
    IMPL.left = binder.bind_node(ast_impl.left);
    IMPL.right = binder.bind_node(ast_impl.right);
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundAssignmentExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (binder[IMPL.left].type && binder[IMPL.right].type) {
        binder[ref].type = binder[IMPL.left].type;
    }
    return ref;
}

template<>
inline void dump(Binder &binder, BoundAssignmentExpression const &impl, int indent)
{
    binder.dump(impl.left, "Left", indent + 2);
    std::cout << std::string(indent + 2, ' ') << to_string(impl.op) << "\n";
    binder.dump(impl.right, "Right", indent + 2);
}

#undef STRUCT
#define STRUCT BasicType

template<>
inline BoundNodeReference rebind<BasicType>(Binder &binder, BoundNodeReference ref)
{
    binder[ref].type = binder.registry.find(IMPL.name);
    return ref;
}

template<>
inline BoundNodeReference bind<BasicType>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BasicType>(ast_node.impl);
    BoundNodeReference ref = binder.add_node<BasicType>(ast_node.ref, ast_node.location);
    binder[ref].impl = ast_impl;
    binder[ref].type = binder.registry.find(IMPL.name);
    return ref;
}

#undef STRUCT
#define STRUCT BoundBinaryExpression

template<>
inline BoundNodeReference bind<BinaryExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BinaryExpression>(ast_node.impl);
    BoundNodeReference ref = binder.add_node<BoundBinaryExpression>(ast_node.ref, ast_node.location);
    IMPL.left = binder.bind_node(ast_impl.left);
    IMPL.op = ast_impl.op;
    IMPL.right = binder.bind_node(ast_impl.right);
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundBinaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (binder[IMPL.left].type && binder[IMPL.right].type) {
        switch (IMPL.op) {
        case BinaryOperator::MemberAccess: {
            auto left_type = binder.registry[*binder[IMPL.left].type];
            if (!std::holds_alternative<Object>(left_type.typespec)) {
                return binder.add_error(ref, "Member access requires an object LHS value");
            }
            auto const& obj = std::get<Object>(left_type.typespec);
            if (!std::holds_alternative<BoundMember>(binder[IMPL.right].impl)) {
                return binder.add_error(ref, "Member access requires an identifier RHS value");
            }
            std::string_view n = std::get<BoundMember>(binder[IMPL.right].impl).name;
            binder[ref].type = {};
            for (auto const &fld : obj.fields) {
                if (fld.first == n) {
                    binder[ref].type = fld.second;
                    break;
                }
            }
            if (!binder[ref].type) {
                return binder.add_error(ref, std::format("Unknown member '{}'", n));
            }
        } break;
        default: {
            binder[ref].type = binder[IMPL.left].type;
        } break;
        }
    }
    return ref;
}

template<>
inline void dump(Binder &binder, BoundBinaryExpression const &impl, int indent)
{
    binder.dump(impl.left, "Left", indent + 2);
    std::cout << std::string(indent + 2, ' ') << to_string(impl.op) << "\n";
    binder.dump(impl.right, "Right", indent + 2);
}

#undef STRUCT
#define STRUCT BoundBlock

template<>
inline BoundNodeReference rebind<BoundBlock>(Binder &binder, BoundNodeReference ref)
{
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };
    bool all_bound = true;
    for (size_t ix = 0; ix < IMPL.statements.size(); ++ix) {
        auto bound_stmt_ref = IMPL.statements[ix];
        auto new_ref = binder.rebind_node(bound_stmt_ref);
        if (new_ref != bound_stmt_ref) {
            IMPL.statements[ix] = new_ref;
        }
        all_bound &= binder[bound_stmt_ref].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = (IMPL.statements.empty())
            ? binder.registry[BuiltinType::Void].ref
            : binder[IMPL.statements.back()].type;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<Block>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<Block>(ast_node.impl);
    BoundNodeReference ref = binder.add_node<BoundBlock>(ast_node.ref, ast_node.location);
    IMPL.label = ast_impl.label;
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto const &stmt : ast_impl.statements) {
            auto bound_stmt = binder.bind_node(stmt);
            IMPL.statements.push_back(bound_stmt);
        }
    }
    return binder.rebind_node(ref);
}

template<>
inline void dump(Binder &binder, BoundBlock const &impl, int indent)
{
    if (impl.label) {
        std::cout << std::string(indent + 2, ' ') << '#' << *impl.label << "\n";
    }
    binder.dump(impl.statements, "Statement", indent + 2);
}

#undef STRUCT
#define STRUCT BoolConstant

template<>
inline BoundNodeReference bind<BoolConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto        ref = binder.add_node<BoolConstant>(ast_node.ref, ast_node.location);
    binder[ref].type = binder.registry[BuiltinType::Bool].ref;
    return ref;
}

template<>
inline void dump(Binder &binder, BoolConstant const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << std::boolalpha << impl.value << "\n";
}

#undef STRUCT
#define STRUCT BoundConstantDeclaration

template<>
inline BoundNodeReference bind<ConstantDeclaration>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ConstantDeclaration>(ast_node.impl);
    auto        ref = binder.add_node<BoundConstantDeclaration>(ast_node.ref, ast_node.location);

    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type);
    }
    IMPL.initializer = binder.bind_node(ast_impl.initializer);
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundConstantDeclaration>(Binder &binder, BoundNodeReference ref)
{
    IMPL.initializer = binder.rebind_node(IMPL.initializer);
    if (!IMPL.type) {
        binder[ref].type = binder[IMPL.initializer].type;
    } else {
        auto type = binder[*IMPL.type].type;
        if (type) {
            // TODO: Type compatibility
            // TODO: Error message
            if (binder[IMPL.initializer].type && *binder[IMPL.initializer].type != *type) {
                return binder.add_error(ref, "Type mismatch between declared type and initialization");
            }
        }
        binder[ref].type = type;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

template<>
inline void dump(Binder &binder, BoundConstantDeclaration const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << impl.name << "\n";
    binder.dump(impl.type, "Type", indent + 2);
    binder.dump(impl.initializer, "Initializer", indent + 2);
}

template<>
inline BoundNodeReference bind<FloatConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &node = binder.ast[ast_ref];
    auto        ref = binder.add_node<FloatConstant>(node.ref, node.location);
    binder[ref].type = binder.registry[BuiltinType::Float].ref;
    return ref;
}

template<>
inline void dump(Binder &, FloatConstant const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << impl.value << "\n";
}

#undef STRUCT
#define STRUCT BoundForeignFunction

template<>
inline BoundNodeReference rebind<BoundForeignFunction>(Binder &binder, BoundNodeReference ref)
{
    IMPL.declaration = binder.rebind_node(IMPL.declaration);
    IMPL.foreign_function = binder.rebind_node(IMPL.foreign_function);
    if (binder[IMPL.declaration].type && binder[IMPL.foreign_function].type) {
        binder[ref].type = binder.registry[BuiltinType::Function].ref;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<ForeignFunction>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ForeignFunction>(ast_node.impl);
    auto        ref = binder.add_node<BoundForeignFunction>(ast_node.ref, ast_node.location);

    IMPL.declaration = binder.bind_node(ast_impl.declaration);
    IMPL.foreign_function = binder.bind_node(ast_impl.foreign_function);
    return binder.rebind_node(ref);
}

template<>
inline void dump(Binder &binder, BoundForeignFunction const &impl, int indent)
{
    binder.dump(impl.declaration, "Declaration", indent + 2);
    binder.dump(impl.foreign_function, "Foreign function", indent + 2);
}

#undef STRUCT
#define STRUCT BoundFunction

template<>
inline BoundNodeReference rebind<BoundFunction>(Binder &binder, BoundNodeReference ref)
{
    IMPL.declaration = binder.rebind_node(IMPL.declaration);
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        auto const &func_decl = I(BoundFunctionDecl, IMPL.declaration);
        for (auto param_ref : func_decl.parameters) {
            auto const& param = I(BoundParameter, param_ref);
            binder.set_name(param.name, param_ref);
        }
        IMPL.implementation = binder.rebind_node(IMPL.implementation);
    }
    if (binder[IMPL.declaration].type && binder[IMPL.implementation].type) {
        binder[ref].type = binder.registry[BuiltinType::Function].ref;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<Function>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Function>(ast_node.impl);
    auto        ref = binder.add_node<BoundFunction>(ast_node.ref, ast_node.location);

    IMPL.declaration = binder.bind_node(ast_impl.declaration);
    IMPL.implementation = binder.bind_node(ast_impl.implementation);
    return binder.rebind_node(ref);
}

template<>
inline void dump(Binder &binder, BoundFunction const &impl, int indent)
{
    binder.dump(impl.declaration, "Declaration", indent + 2);
    binder.dump(impl.implementation, "Implementation", indent + 2);
}

#undef STRUCT
#define STRUCT BoundFunctionCall

template<>
inline BoundNodeReference rebind<BoundFunctionCall>(Binder &binder, BoundNodeReference ref)
{
    bool all_bound { true };
    for (auto ix = 0; ix < IMPL.arguments.size(); ++ix) {
        auto arg = IMPL.arguments[ix];
        if (auto new_arg = binder.rebind_node(arg); new_arg != arg) {
            IMPL.arguments[ix] = new_arg;
            arg = new_arg;
        }
        all_bound &= binder[arg].type.has_value();
    }
    if (!IMPL.function) {
        auto n = IMPL.name;
        IMPL.function = binder.resolve(n);
        if (!IMPL.function && binder.pass > 0) {
            return binder.add_error(ref, "Undefined function");
        }
    }
    if (IMPL.function) {
        auto &func_decl = binder.impl<BoundFunctionDecl>(*IMPL.function);
        if (func_decl.parameters.size() != IMPL.arguments.size()) {
            return binder.add_error(ref, "Parameter/argument count mismatch");
        }
        size_t ix = 0;
        for (auto param : func_decl.parameters) {
            auto arg = IMPL.arguments[ix++];
            if (binder[param].type && binder[arg].type && *binder[param].type != *binder[arg].type) {
                return binder.add_error(arg, "Argument type mismatch");
            }
        }
        if (all_bound) {
            binder[ref].type = binder[*IMPL.function].type;
        }
    }
    return ref;
}

template<>
inline BoundNodeReference bind<FunctionCall>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionCall>(ast_node.impl);
    auto        ref = binder.add_node<BoundFunctionCall>(ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    for (auto arg : ast_impl.arguments) {
        IMPL.arguments.push_back(binder.bind_node(arg));
    }
    return binder.rebind_node(ref);
}

template<>
inline void dump(Binder &binder, BoundFunctionCall const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << "Name: " << impl.name << '\n';
    binder.dump(impl.function, "Function", indent + 2);
    binder.dump(impl.arguments, "Argument", indent + 2);
}

#undef STRUCT
#define STRUCT BoundFunctionDecl

template<>
inline BoundNodeReference bind<FunctionDecl>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionDecl>(ast_node.impl);
    auto        ref = binder.add_node<BoundFunctionDecl>(ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    if (ast_impl.return_type) {
        IMPL.return_type = binder.bind_node(*ast_impl.return_type);
    }
    for (auto param : ast_impl.parameters) {
        auto param_ref = binder.bind_node(param);
        IMPL.parameters.push_back(param_ref);
    }
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundFunctionDecl>(Binder &binder, BoundNodeReference ref)
{
    auto all_bound { true };
    for (auto ix = 0; ix < IMPL.parameters.size(); ++ix) {
        auto param = IMPL.parameters[ix];
        if (auto new_param = binder.rebind_node(param); new_param != param) {
            IMPL.parameters[ix] = new_param;
            param = new_param;
        }
        all_bound &= binder[param].type.has_value();
    }
    TypeReference type_ref;
    if (IMPL.return_type) {
        IMPL.return_type = binder.rebind_node(*IMPL.return_type);
        all_bound &= binder[*IMPL.return_type].type.has_value();
        type_ref = *binder[*IMPL.return_type].type;
    } else {
        type_ref = binder.registry[BuiltinType::Void].ref;
    }
    if (all_bound) {
        binder[ref].type = type_ref;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

template<>
inline void dump(Binder &binder, BoundFunctionDecl const &impl, int indent)
{
    std::cout << std::string(indent, ' ');
    std::cout << impl.name << '(';
    auto first { true };
    for (auto param_ref : impl.parameters) {
        if (!first) {
            std::cout << ", ";
        }
        auto const &param = binder[param_ref];
        std::cout << I(BoundParameter, param_ref).name;
        if (param.type) {
            std::cout << ": " << binder.registry[*param.type].name;
        }
        first = false;
    }
    std::cout << ')';
    if (impl.return_type && binder[*impl.return_type].type) {
        std::cout << ' ' << binder.registry[*binder[*impl.return_type].type].name;
    }
    std::cout << '\n';
}

#undef STRUCT
#define STRUCT BoundIdentifier

template<>
inline BoundNodeReference bind<Identifier>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Identifier>(ast_node.impl);
    auto        ref = binder.add_node<BoundIdentifier>(ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.text;
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundIdentifier>(Binder &binder, BoundNodeReference ref)
{
    auto variable = binder.resolve(IMPL.name);
    if (variable) {
        auto &var_node = binder[*variable];
        IMPL.declaration = *variable;
        binder[ref].type = var_node.type;
    } else if (binder.pass > 0) {
        return binder.add_error(ref, std::format("Undefined variable '{}'", IMPL.name));
    }
    return ref;
}

template<>
inline void dump(Binder &, BoundIdentifier const &impl, int indent)
{
    std::cout << std::string(indent, ' ') << impl.name << '\n';
}

#undef STRUCT
#define STRUCT BoundIf

template<>
inline BoundNodeReference rebind<BoundIf>(Binder &binder, BoundNodeReference ref)
{
    IMPL.condition = binder.rebind_node(IMPL.condition);
    IMPL.true_branch = binder.rebind_node(IMPL.true_branch);
    if (IMPL.false_branch) {
        IMPL.false_branch = binder.rebind_node(*IMPL.false_branch);
    }
    if (binder[IMPL.condition].type && binder.registry[BuiltinType::Bool].ref != *binder[IMPL.condition].type) {
        return binder.add_error(ref, "If condition is not a boolean");
    }
    if (binder[IMPL.true_branch].type) {
        auto type = *binder[IMPL.true_branch].type;
        // TODO: Type compatibility
        if (IMPL.false_branch) {
            if (!binder[*IMPL.false_branch].type) {
                return ref;
            }
            if (*binder[*IMPL.false_branch].type != type) {
                // TODO: Error message
                return binder.add_error(ref, "'If' and 'Else' branches have different types");
            }
        }
        binder[ref].type = type;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<If>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<If>(ast_node.impl);
    auto        ref = binder.add_node<BoundIf>(ast_node.ref, ast_node.location);

    IMPL.condition = binder.bind_node(ast_impl.condition);
    IMPL.true_branch = binder.bind_node(ast_impl.true_branch);
    if (ast_impl.false_branch) {
        IMPL.false_branch = binder.bind_node(*ast_impl.false_branch);
    }
    return binder.rebind_node(ref);
}

#undef STRUCT
#define STRUCT IntConstant

template<>
inline BoundNodeReference bind<IntConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto        ref = binder.add_node<IntConstant>(ast_node.ref, ast_node.location);
    binder[ref].type = binder.registry[BuiltinType::Int].ref;
    return ref;
}

template<>
inline void dump(Binder &, IntConstant const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << impl.value << "\n";
}

#undef STRUCT
#define STRUCT BoundLoop

template<>
inline BoundNodeReference bind<Loop>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Loop>(ast_node.impl);
    auto        ref = binder.add_node<BoundLoop>(ast_node.ref, ast_node.location);

    IMPL.body = binder.bind_node(ast_impl.body);
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundLoop>(Binder &binder, BoundNodeReference ref)
{
    binder.rebind_node(IMPL.body);
    binder[ref].type = binder[IMPL.body].type;
    return ref;
}

#undef STRUCT
#define STRUCT BoundMember

template<>
inline BoundNodeReference bind<Member>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Member>(ast_node.impl);
    auto        ref = binder.add_node<BoundMember>(ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    binder[ref].type = binder.registry[BuiltinType::Void].ref;
    return ref;
}

template<>
inline void dump(Binder &, BoundMember const &impl, int indent)
{
    std::cout << std::string(indent, ' ') << impl.name << '\n';
}

#undef STRUCT
#define STRUCT BoundParameter

template<>
inline BoundNodeReference rebind<BoundParameter>(Binder &binder, BoundNodeReference ref)
{
    IMPL.type = binder.rebind_node(IMPL.type);
    binder[ref].type = binder[IMPL.type].type;
    return ref;
}

template<>
inline BoundNodeReference bind<Parameter>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Parameter>(ast_node.impl);
    auto        ref = binder.add_node<BoundParameter>(ast_node.ref, ast_node.location);

    IMPL.name = ast_impl.name;
    IMPL.type = binder.bind_node(ast_impl.type);
    return binder.rebind_node(ref);
}

#undef STRUCT
#define STRUCT BoundPointerType

template<>
inline BoundNodeReference bind<PointerType>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<PointerType>(ast_node.impl);
    auto        ref = binder.add_node<BoundPointerType>(ast_node.ref, ast_node.location);
    IMPL.element_type = binder.bind_node(ast_impl.element_type);
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundPointerType>(Binder &binder, BoundNodeReference ref)
{
    IMPL.element_type = binder.rebind_node(IMPL.element_type);
    if (binder[IMPL.element_type].type) {
        binder[ref].type = binder.registry.resolve_pointer(*binder[IMPL.element_type].type);
    }
    return ref;
}

template<>
inline void dump(Binder &binder, BoundPointerType const &impl, int indent)
{
    binder.dump(impl.element_type, "Element Type", indent + 2);
}

#undef STRUCT
#define STRUCT BoundProgram

template<>
inline BoundNodeReference rebind<BoundProgram>(Binder &binder, BoundNodeReference ref)
{
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };
    auto all_bound { true };
    for (auto ix = 0; ix < IMPL.declarations.size(); ++ix) {
        auto decl_ref = IMPL.declarations[ix];
        if (auto new_decl = binder.rebind_node(decl_ref); new_decl != decl_ref) {
            IMPL.declarations[ix] = new_decl;
            decl_ref = new_decl;
        }
        all_bound &= binder[decl_ref].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = binder.registry[BuiltinType::Void].ref;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<Program>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Program>(ast_node.impl);
    auto        ref = binder.add_node<BoundProgram>(ast_node.ref, ast_node.location);

    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto decl_ref : ast_impl.declarations) {
            auto bound_decl = binder.bind_node(decl_ref);
            IMPL.declarations.push_back(bound_decl);
        }
    }
    return binder.rebind_node(ref);
}

template<>
inline void dump(Binder &binder, BoundProgram const &impl, int indent)
{
    binder.dump(impl.declarations, "Declaration", indent + 2);
}

#undef STRUCT
#define STRUCT BoundReturn

template<>
inline BoundNodeReference rebind<BoundReturn>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.expression) {
        IMPL.expression = binder.rebind_node(*IMPL.expression);
        binder[ref].type = binder[*IMPL.expression].type;
    } else {
        binder[ref].type = binder.registry[BuiltinType::Void].ref;
    }
    return ref;
}

template<>
inline BoundNodeReference bind<Return>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Return>(ast_node.impl);
    auto        ref = binder.add_node<BoundReturn>(ast_node.ref, ast_node.location);

    if (ast_impl.expression) {
        IMPL.expression = binder.bind_node(*ast_impl.expression);
    }
    return binder.rebind_node(ref);
}

#undef STRUCT
#define STRUCT StringConstant

template<>
inline BoundNodeReference bind<StringConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<StringConstant>(ast_node.impl);
    auto        ref = binder.add_node<StringConstant>(ast_node.ref, ast_node.location);
    IMPL.value  = ast_impl.value.substr(1, ast_impl.value.length() - 2);
    binder[ref].type = binder.registry["string"].ref;
    return ref;
}

template<>
inline void dump(Binder &, StringConstant const &impl, int indent)
{
    std::cout << std::string(indent + 2, ' ') << impl.value << "\n";
}

#undef STRUCT
#define STRUCT BoundSubscript

template<>
inline BoundNodeReference bind<Subscript>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Subscript>(ast_node.impl);
    auto        ref = binder.add_node<BoundSubscript>(ast_node.ref, ast_node.location);
    for (auto arg : ast_impl.subscripts) {
        IMPL.subscripts.push_back(binder.bind_node(arg));
    }
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundSubscript>(Binder &binder, BoundNodeReference ref)
{
    bool all_bound { true };
    for (auto ix = 0; ix < IMPL.subscripts.size(); ++ix) {
        auto arg = IMPL.subscripts[ix];
        if (auto new_arg = binder.rebind_node(arg); new_arg != arg) {
            IMPL.subscripts[ix] = new_arg;
            arg = new_arg;
        }
        all_bound &= binder[arg].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = binder.registry[BuiltinType::Void].ref;
    }
    return ref;
}

#undef STRUCT
#define STRUCT BoundUnaryExpression

template<>
inline BoundNodeReference bind<UnaryExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<UnaryExpression>(ast_node.impl);
    auto        ref = binder.add_node<BoundUnaryExpression>(ast_node.ref, ast_node.location);
    IMPL.op = ast_impl.op;
    IMPL.operand = binder.bind_node(ast_impl.operand);
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundUnaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.operand = binder.rebind_node(IMPL.operand);
    binder[ref].type = binder[IMPL.operand].type;
    return ref;
}

#undef STRUCT
#define STRUCT BoundVariableDeclaration

template<>
inline BoundNodeReference bind<VariableDeclaration>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<VariableDeclaration>(ast_node.impl);
    auto        ref = binder.add_node<BoundVariableDeclaration>(ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type);
    }
    if (ast_impl.initializer) {
        IMPL.initializer = binder.bind_node(*ast_impl.initializer);
    }
    return binder.rebind_node(ref);
}

template<>
inline BoundNodeReference rebind<BoundVariableDeclaration>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.initializer) {
        IMPL.initializer = binder.rebind_node(*IMPL.initializer);
    }
    if (!IMPL.type) {
        if (IMPL.initializer) {
            binder[ref].type = binder[*IMPL.initializer].type;
        } else {
            return binder.add_error(ref, "Uninitialized variable must have type");
        }
    } else {
        auto type = binder[*IMPL.type].type;
        if (type && IMPL.initializer) {
            // TODO: Type compatibility
            // TODO: Error message
            if (binder[*IMPL.initializer].type && *binder[*IMPL.initializer].type != *type) {
                return binder.add_error(ref, "Type mismatch between declared type and initialization");
            }
        }
        binder[ref].type = type;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

}
