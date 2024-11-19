/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <iostream>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeindex>
#include <variant>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Logging.h>
#include <Result.h>

namespace Arwen {

template<class Impl>
BoundNodeReference add_node(Binder &binder, NodeReference ref, Location location)
{
    binder.bound_nodes.push_back(BoundNode::make<Impl>(ref, location));
    binder.bound_nodes.back().ref = binder.bound_nodes.size() - 1;
    return binder.bound_nodes.back().ref;
}

template<typename... Args>
BoundNodeReference add_error(Binder &binder, BoundNodeReference ref, std::format_string<Args...> message, Args &&...args)
{
    BoundNodeReference err = add_node<BindError>(binder, binder.bound_nodes[ref].ast_ref, binder.bound_nodes[ref].location);
    auto              &impl = std::get<BindError>(binder.bound_nodes[err].impl);
    impl.node = ref;
    impl.message = std::format(message, std::forward<Args>(args)...);
    binder.errors.emplace_back(err);
    return err;
}

template<typename AstImpl>
BoundNodeReference bind([[maybe_unused]] Binder &, [[maybe_unused]] NodeReference)
{
    UNREACHABLE();
}

template<typename T>
BoundNodeReference rebind([[maybe_unused]] Binder &, BoundNodeReference ref)
{
    return ref;
}

template<typename T>
void dump(std::ostream &out, Binder &, T const &, int)
{
}

template<typename T>
void to_string(std::ostream &out, Binder &, T const &)
{
}

template<>
void to_string(std::ostream &out, Binder &binder, BindError const &impl)
{
    out << "ERROR: " << impl.message;
}

template<>
void dump(std::ostream &out, Binder &binder, BindError const &impl, int indent)
{
    binder.dump(out, impl.node, "Node", indent);
}

#undef STRUCT
#define STRUCT BoundAssignmentExpression

template<>
BoundNodeReference bind<AssignmentExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<AssignmentExpression>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundAssignmentExpression>(binder, ast_node.ref, ast_node.location);
    IMPL.left = binder.bind_node(ast_impl.left);
    IMPL.right = binder.bind_node(ast_impl.right);
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundAssignmentExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (binder[IMPL.left].type && binder[IMPL.right].type) {
        if (std::holds_alternative<BoundBinaryExpression>(binder[IMPL.left].impl)) {
            auto left = std::get<BoundBinaryExpression>(binder[IMPL.left].impl);
            assert(left.op == BinaryOperator::MemberAccess);
        } else if (!std::holds_alternative<BoundIdentifier>(binder[IMPL.left].impl)) {
            return add_error(binder, ref, "Assignment requires identifier or member access LHS value");
        }
        auto right_type = binder.registry[*binder[IMPL.right].type];
        auto left_type = binder.registry[*binder[IMPL.right].type];
        if (!right_type.is_assignable_to(left_type.ref)) {
            return add_error(binder, ref, "Cannot assign expression of type '{}' to variable of type '{}'", right_type.name, left_type.name);
        }
        binder[ref].type = binder[IMPL.left].type;
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundAssignmentExpression const &impl)
{
    if (impl.op != BinaryOperator::Equal) {
        out << "lhs = lhs " << to_string(impl.op) << " rhs";
    } else {
        out << "lhs " << to_string(impl.op) << " rhs";
    }
}

template<>
void dump(std::ostream &out, Binder &binder, BoundAssignmentExpression const &impl, int indent)
{
    binder.dump(out, impl.left, "lhs", indent);
    binder.dump(out, impl.right, "rhs", indent);
}

#undef STRUCT
#define STRUCT BasicTypeNode

template<>
BoundNodeReference rebind<BasicTypeNode>(Binder &binder, BoundNodeReference ref)
{
    binder[ref].type = binder.registry.find(IMPL.name);
    return ref;
}

template<>
BoundNodeReference bind<BasicTypeNode>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BasicTypeNode>(ast_node.impl);
    BoundNodeReference ref = add_node<BasicTypeNode>(binder, ast_node.ref, ast_node.location);
    binder[ref].impl = ast_impl;
    binder[ref].type = binder.registry.find(IMPL.name);
    return ref;
}

#undef STRUCT
#define STRUCT BoundBinaryExpression

template<>
BoundNodeReference bind<BinaryExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BinaryExpression>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundBinaryExpression>(binder, ast_node.ref, ast_node.location);
    IMPL.left = binder.bind_node(ast_impl.left);
    IMPL.op = ast_impl.op;
    IMPL.right = binder.bind_node(ast_impl.right);
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundBinaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (binder[IMPL.left].type && binder[IMPL.right].type) {
        auto left_type = binder.registry[*binder[IMPL.left].type];
        auto right_type = binder.registry[*binder[IMPL.right].type];
        switch (IMPL.op) {
        case BinaryOperator::MemberAccess: {
            if (left_type.typespec.tag() != TypeKind::Object) {
                return add_error(binder, ref, "Member access requires an object LHS value");
            }
            auto const &obj = left_type.typespec.get<TypeKind::Object>();
            if (!std::holds_alternative<BoundMember>(binder[IMPL.right].impl)) {
                return add_error(binder, ref, "Member access requires an identifier RHS value");
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
                return add_error(binder, ref, "Unknown member '{}'", n);
            }
        } break;
        default: {
            BinaryOperatorMapping m { IMPL.op };
            if (auto c = m.compatible(static_cast<PrimitiveType>(left_type.ref), static_cast<PrimitiveType>(right_type.ref)); c) {
                binder[ref].type = static_cast<TypeReference>(*c);
            } else {
                add_error(binder, ref, "Cannot use operator '{}' with LHS '{}' and RHS '{}'", to_string(m.op), left_type.name, right_type.name);
            }
        } break;
        }
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundBinaryExpression const &impl)
{
    out << "lhs " << to_string(impl.op) << " rhs";
}

template<>
void dump(std::ostream &out, Binder &binder, BoundBinaryExpression const &impl, int indent)
{
    binder.dump(out, impl.left, "lhs", indent);
    binder.dump(out, impl.right, "rhs", indent);
}

#undef STRUCT
#define STRUCT BoundBlock

template<>
BoundNodeReference rebind<BoundBlock>(Binder &binder, BoundNodeReference ref)
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
            ? binder.registry[BasicType::Void].ref
            : binder[IMPL.statements.back()].type;
    }
    return ref;
}

template<>
BoundNodeReference bind<Block>(Binder &binder, NodeReference ast_ref)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<Block>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundBlock>(binder, ast_node.ref, ast_node.location);
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
void to_string(std::ostream &out, Binder &binder, BoundBlock const &impl)
{
    if (impl.label) {
        out << '#' << *impl.label << ' ';
    }
    out << "{";
}

template<>
void dump(std::ostream &out, Binder &binder, BoundBlock const &impl, int indent)
{
    binder.dump(out, impl.statements, "statements", indent);
}

#undef STRUCT
#define STRUCT BoolConstant

template<>
BoundNodeReference bind<BoolConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto        ref = add_node<BoolConstant>(binder, ast_node.ref, ast_node.location);
    binder[ref].type = binder.registry[PrimitiveType::Bool].ref;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, BoolConstant const &impl)
{
    std::cout << std::boolalpha << impl.value;
}

#undef STRUCT
#define STRUCT BoundConstantDeclaration

template<>
BoundNodeReference bind<ConstantDeclaration>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ConstantDeclaration>(ast_node.impl);
    auto        ref = add_node<BoundConstantDeclaration>(binder, ast_node.ref, ast_node.location);

    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type);
    }
    IMPL.initializer = binder.bind_node(ast_impl.initializer);
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundConstantDeclaration>(Binder &binder, BoundNodeReference ref)
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
                return add_error(binder, ref, "Type mismatch between declared type and initialization");
            }
        }
        binder[ref].type = type;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundConstantDeclaration const &impl)
{
    out << impl.name;
    if (impl.type) {
        out << ": ";
        binder.to_string(out, *impl.type);
    }
}

template<>
void dump(std::ostream &out, Binder &binder, BoundConstantDeclaration const &impl, int indent)
{
    binder.dump(out, impl.initializer, "Initializer", indent);
}

template<>
BoundNodeReference bind<FloatConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &node = binder.ast[ast_ref];
    auto        ref = add_node<FloatConstant>(binder, node.ref, node.location);
    binder[ref].type = binder.registry[PrimitiveType::Float].ref;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, FloatConstant const &impl)
{
    out << impl.value;
}

#undef STRUCT
#define STRUCT BoundForeignFunction

template<>
BoundNodeReference rebind<BoundForeignFunction>(Binder &binder, BoundNodeReference ref)
{
    IMPL.declaration = binder.rebind_node(IMPL.declaration);
    IMPL.foreign_function = binder.rebind_node(IMPL.foreign_function);
    if (binder[IMPL.declaration].type && binder[IMPL.foreign_function].type) {
        binder[ref].type = binder.registry[PseudoType::Function].ref;
    }
    return ref;
}

template<>
BoundNodeReference bind<ForeignFunction>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ForeignFunction>(ast_node.impl);
    auto        ref = add_node<BoundForeignFunction>(binder, ast_node.ref, ast_node.location);

    IMPL.declaration = binder.bind_node(ast_impl.declaration);
    IMPL.foreign_function = binder.bind_node(ast_impl.foreign_function);
    return binder.rebind_node(ref);
}

template<>
void dump(std::ostream &out, Binder &binder, BoundForeignFunction const &impl, int indent)
{
    binder.dump(out, impl.declaration, "Declaration", indent);
    binder.dump(out, impl.foreign_function, "Foreign function", indent);
}

#undef STRUCT
#define STRUCT BoundFunction

template<>
BoundNodeReference rebind<BoundFunction>(Binder &binder, BoundNodeReference ref)
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
            auto const &param = I(BoundParameter, param_ref);
            binder.set_name(param.name, param_ref);
        }
        IMPL.implementation = binder.rebind_node(IMPL.implementation);
    }
    if (binder[IMPL.declaration].type && binder[IMPL.implementation].type) {
        binder[ref].type = binder.registry[PseudoType::Function].ref;
    }
    return ref;
}

template<>
BoundNodeReference bind<Function>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Function>(ast_node.impl);
    auto        ref = add_node<BoundFunction>(binder, ast_node.ref, ast_node.location);

    IMPL.declaration = binder.bind_node(ast_impl.declaration);
    IMPL.implementation = binder.bind_node(ast_impl.implementation);
    return binder.rebind_node(ref);
}

template<>
void dump(std::ostream &out, Binder &binder, BoundFunction const &impl, int indent)
{
    binder.dump(out, impl.declaration, "Declaration", indent);
    binder.dump(out, impl.implementation, "Implementation", indent);
}

#undef STRUCT
#define STRUCT BoundFunctionCall

template<>
BoundNodeReference rebind<BoundFunctionCall>(Binder &binder, BoundNodeReference ref)
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
            return add_error(binder, ref, "Undefined function");
        }
    }
    if (IMPL.function) {
        auto &func_decl = I(BoundFunctionDecl, *IMPL.function);
        if (func_decl.parameters.size() != IMPL.arguments.size()) {
            return add_error(binder, ref, "In call to '{}': Expected {} arguments, got {}", func_decl.name, func_decl.parameters.size(), IMPL.arguments.size());
        }
        size_t ix = 0;
        for (auto param : func_decl.parameters) {
            auto arg = IMPL.arguments[ix++];
            if (binder[param].type && binder[arg].type && *binder[param].type != *binder[arg].type) {
                return add_error(binder, arg, "Argument type mismatch");
            }
        }
        if (all_bound) {
            binder[ref].type = binder[*IMPL.function].type;
        }
    }
    return ref;
}

template<>
BoundNodeReference bind<FunctionCall>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionCall>(ast_node.impl);
    auto        ref = add_node<BoundFunctionCall>(binder, ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    for (auto arg : ast_impl.arguments) {
        auto arg_ref = binder.bind_node(arg);
        IMPL.arguments.push_back(arg_ref);
    }
    return binder.rebind_node(ref);
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundFunctionCall const &impl)
{
    out << impl.name;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundFunctionCall const &impl, int indent)
{
    binder.dump(out, impl.function, "Function", indent);
    binder.dump(out, impl.arguments, "Argument", indent);
}

#undef STRUCT
#define STRUCT BoundFunctionDecl

template<>
BoundNodeReference bind<FunctionDecl>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionDecl>(ast_node.impl);
    auto        ref = add_node<BoundFunctionDecl>(binder, ast_node.ref, ast_node.location);
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
BoundNodeReference rebind<BoundFunctionDecl>(Binder &binder, BoundNodeReference ref)
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
        type_ref = binder.registry[BasicType::Void].ref;
    }
    if (all_bound) {
        binder[ref].type = type_ref;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundFunctionDecl const &impl)
{
    out << impl.name << '(';
    auto first { true };
    for (auto param_ref : impl.parameters) {
        if (!first) {
            out << ", ";
        }
        auto const &param = binder[param_ref];
        out << I(BoundParameter, param_ref).name;
        if (param.type) {
            out << ": " << binder.registry[*param.type].name;
        }
        first = false;
    }
    out << ')';
    if (impl.return_type && binder[*impl.return_type].type) {
        out << ' ' << binder.registry[*binder[*impl.return_type].type].name;
    }
    out << '\n';
}

#undef STRUCT
#define STRUCT BoundIdentifier

template<>
BoundNodeReference bind<Identifier>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Identifier>(ast_node.impl);
    auto        ref = add_node<BoundIdentifier>(binder, ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.text;
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundIdentifier>(Binder &binder, BoundNodeReference ref)
{
    auto variable = binder.resolve(IMPL.name);
    if (variable) {
        auto &var_node = binder[*variable];
        IMPL.declaration = *variable;
        binder[ref].type = var_node.type;
    } else if (binder.pass > 0) {
        return add_error(binder, ref, "Undefined variable '{}'", IMPL.name);
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, BoundIdentifier const &impl)
{
    out << impl.name;
}

#undef STRUCT
#define STRUCT BoundIf

template<>
BoundNodeReference rebind<BoundIf>(Binder &binder, BoundNodeReference ref)
{
    IMPL.condition = binder.rebind_node(IMPL.condition);
    IMPL.true_branch = binder.rebind_node(IMPL.true_branch);
    if (IMPL.false_branch) {
        IMPL.false_branch = binder.rebind_node(*IMPL.false_branch);
    }
    if (binder[IMPL.condition].type && binder.registry[PrimitiveType::Bool].ref != *binder[IMPL.condition].type) {
        return add_error(binder, ref, "If condition is not a boolean");
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
                return add_error(binder, ref, "'If' and 'Else' branches have different types");
            }
        }
        binder[ref].type = type;
    }
    return ref;
}

template<>
BoundNodeReference bind<If>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<If>(ast_node.impl);
    auto        ref = add_node<BoundIf>(binder, ast_node.ref, ast_node.location);

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
BoundNodeReference bind<IntConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto        ref = add_node<IntConstant>(binder, ast_node.ref, ast_node.location);
    binder[ref].type = binder.registry[PrimitiveType::Int].ref;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, IntConstant const &impl)
{
    out << impl.value;
}

#undef STRUCT
#define STRUCT BoundLoop

template<>
BoundNodeReference bind<Loop>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Loop>(ast_node.impl);
    auto        ref = add_node<BoundLoop>(binder, ast_node.ref, ast_node.location);

    IMPL.body = binder.bind_node(ast_impl.body);
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundLoop>(Binder &binder, BoundNodeReference ref)
{
    binder.rebind_node(IMPL.body);
    binder[ref].type = binder[IMPL.body].type;
    return ref;
}

#undef STRUCT
#define STRUCT BoundMember

template<>
BoundNodeReference bind<Member>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Member>(ast_node.impl);
    auto        ref = add_node<BoundMember>(binder, ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    binder[ref].type = binder.registry[BasicType::Void].ref;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, BoundMember const &impl)
{
    out << impl.name;
}

#undef STRUCT
#define STRUCT BoundModule

template<>
BoundNodeReference rebind<BoundModule>(Binder &binder, BoundNodeReference ref)
{
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };
    auto all_bound { true };
    for (auto ix = 0; ix < IMPL.names.size(); ++ix) {
        auto decl_ref = IMPL.names[ix];
        if (auto new_decl = binder.rebind_node(decl_ref); new_decl != decl_ref) {
            IMPL.names[ix] = new_decl;
            decl_ref = new_decl;
        }
        all_bound &= binder[decl_ref].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = binder.registry[PseudoType::Void].ref;
    }
    return ref;
}

template<>
BoundNodeReference bind<Module>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Module>(ast_node.impl);
    auto        ref = add_node<BoundModule>(binder, ast_node.ref, ast_node.location);

    IMPL.name = ast_impl.name;
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto decl_ref : ast_impl.names) {
            auto bound_decl = binder.bind_node(decl_ref);
            IMPL.names.push_back(bound_decl);
        }
    }
    return binder.rebind_node(ref);
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundModule const &impl)
{
    out << impl.name;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundModule const &impl, int indent)
{
    binder.dump(out, impl.names, "Name", indent);
}

#undef STRUCT
#define STRUCT BoundParameter

template<>
BoundNodeReference rebind<BoundParameter>(Binder &binder, BoundNodeReference ref)
{
    IMPL.type = binder.rebind_node(IMPL.type);
    binder[ref].type = binder[IMPL.type].type;
    return ref;
}

template<>
BoundNodeReference bind<Parameter>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Parameter>(ast_node.impl);
    auto        ref = add_node<BoundParameter>(binder, ast_node.ref, ast_node.location);

    IMPL.name = ast_impl.name;
    IMPL.type = binder.bind_node(ast_impl.type);
    return binder.rebind_node(ref);
}

#undef STRUCT
#define STRUCT BoundPointerType

template<>
BoundNodeReference bind<PointerType>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<PointerType>(ast_node.impl);
    auto        ref = add_node<BoundPointerType>(binder, ast_node.ref, ast_node.location);
    IMPL.element_type = binder.bind_node(ast_impl.element_type);
    return ref;
}

template<>
BoundNodeReference rebind<BoundPointerType>(Binder &binder, BoundNodeReference ref)
{
    IMPL.element_type = binder.rebind_node(IMPL.element_type);
    if (binder[IMPL.element_type].type) {
        binder[ref].type = binder.registry.resolve_pointer(*binder[IMPL.element_type].type);
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundPointerType const &impl, int indent)
{
    binder.dump(out, impl.element_type, "Element Type", indent);
}

#undef STRUCT
#define STRUCT BoundProgram

template<>
BoundNodeReference rebind<BoundProgram>(Binder &binder, BoundNodeReference ref)
{
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };
    auto all_bound { true };
    for (auto ix = 0; ix < IMPL.modules.size(); ++ix) {
        auto mod_ref = IMPL.modules[ix];
        if (auto new_mod = binder.rebind_node(mod_ref); new_mod != mod_ref) {
            IMPL.modules[ix] = new_mod;
            mod_ref = new_mod;
        }
        all_bound &= binder[mod_ref].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = binder.registry[PseudoType::Void].ref;
    }
    return ref;
}

template<>
BoundNodeReference bind<Program>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Program>(ast_node.impl);
    auto        ref = add_node<BoundProgram>(binder, ast_node.ref, ast_node.location);

    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto mod_ref : ast_impl.modules) {
            auto bound_decl = binder.bind_node(mod_ref);
            IMPL.modules.push_back(bound_decl);
        }
    }
    return binder.rebind_node(ref);
}

template<>
void dump(std::ostream &out, Binder &binder, BoundProgram const &impl, int indent)
{
    binder.dump(out, impl.modules, "Module", indent);
}

#undef STRUCT
#define STRUCT BoundReturn

template<>
BoundNodeReference rebind<BoundReturn>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.expression) {
        IMPL.expression = binder.rebind_node(*IMPL.expression);
        binder[ref].type = binder[*IMPL.expression].type;
    } else {
        binder[ref].type = binder.registry[PseudoType::Void].ref;
    }
    return ref;
}

template<>
BoundNodeReference bind<Return>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Return>(ast_node.impl);
    auto        ref = add_node<BoundReturn>(binder, ast_node.ref, ast_node.location);

    if (ast_impl.expression) {
        IMPL.expression = binder.bind_node(*ast_impl.expression);
    }
    return binder.rebind_node(ref);
}

#undef STRUCT
#define STRUCT StringConstant

template<>
BoundNodeReference bind<StringConstant>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<StringConstant>(ast_node.impl);
    auto        ref = add_node<StringConstant>(binder, ast_node.ref, ast_node.location);
    IMPL.value = ast_impl.value.substr(1, ast_impl.value.length() - 2);
    binder[ref].type = binder.registry["string"].ref;
    return ref;
}

template<>
void dump(std::ostream &out, Binder &, StringConstant const &impl, int indent)
{
    out << std::string(indent + 2, ' ') << impl.value << "\n";
}

#undef STRUCT
#define STRUCT BoundSubscript

template<>
BoundNodeReference bind<Subscript>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Subscript>(ast_node.impl);
    auto        ref = add_node<BoundSubscript>(binder, ast_node.ref, ast_node.location);
    for (auto arg : ast_impl.subscripts) {
        IMPL.subscripts.push_back(binder.bind_node(arg));
    }
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundSubscript>(Binder &binder, BoundNodeReference ref)
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
        binder[ref].type = binder.registry[PseudoType::Void].ref;
    }
    return ref;
}

#undef STRUCT
#define STRUCT BoundUnaryExpression

template<>
BoundNodeReference bind<UnaryExpression>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<UnaryExpression>(ast_node.impl);
    auto        ref = add_node<BoundUnaryExpression>(binder, ast_node.ref, ast_node.location);
    IMPL.op = ast_impl.op;
    IMPL.operand = binder.bind_node(ast_impl.operand);
    return binder.rebind_node(ref);
}

template<>
BoundNodeReference rebind<BoundUnaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.operand = binder.rebind_node(IMPL.operand);
    binder[ref].type = binder[IMPL.operand].type;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundUnaryExpression const &impl)
{
    out << to_string(impl.op) << "operand";
}

template<>
void dump(std::ostream &out, Binder &binder, BoundUnaryExpression const &impl, int indent)
{
    binder.dump(out, impl.operand, "Operand", indent);
}

#undef STRUCT
#define STRUCT BoundVariableDeclaration

template<>
BoundNodeReference bind<VariableDeclaration>(Binder &binder, NodeReference ast_ref)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<VariableDeclaration>(ast_node.impl);
    auto        ref = add_node<BoundVariableDeclaration>(binder, ast_node.ref, ast_node.location);
    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type);
    }
    if (ast_impl.initializer) {
        IMPL.initializer = binder.bind_node(*ast_impl.initializer);
    }
    return binder.rebind_node(ref);
}

std::map<std::type_index, std::string> BoundNode::type_names {};

std::string_view BoundNode::type_name() const
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

template<>
BoundNodeReference rebind<BoundVariableDeclaration>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.initializer) {
        IMPL.initializer = binder.rebind_node(*IMPL.initializer);
    }
    if (!IMPL.type) {
        if (IMPL.initializer) {
            binder[ref].type = binder[*IMPL.initializer].type;
        } else {
            return add_error(binder, ref, "Uninitialized variable must have type");
        }
    } else {
        auto type = binder[*IMPL.type].type;
        if (type && IMPL.initializer) {
            // TODO: Type compatibility
            // TODO: Error message
            if (binder[*IMPL.initializer].type && *binder[*IMPL.initializer].type != *type) {
                return add_error(binder, ref, "Type mismatch between declared type and initialization");
            }
        }
        binder[ref].type = type;
    }
    binder.set_name(IMPL.name, ref);
    return ref;
}

BoundNodeReference Binder::bind_node(NodeReference ast_ref)
{
    auto const &ast_node = ast[ast_ref];
    auto        ret = std::visit(
        [this, ast_node](auto &impl) -> BoundNodeReference {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::bind<T>(*this, ast_node.ref);
        },
        ast_node.impl);
    if (!bound_nodes[ret].type) {
        ++unbound;
    }
    return ret;
}

BoundNodeReference Binder::rebind_node(BoundNodeReference ref)
{
    auto const &node = bound_nodes[ref];
    auto        ret = ref;
    if (!node.type) {
        ret = std::visit(
            [this, node](auto &impl) -> BoundNodeReference {
                using T = std::decay_t<decltype(impl)>;
                return Arwen::rebind<T>(*this, node.ref);
            },
            node.impl);
    }
    if (!bound_nodes[ret].type) {
        ++unbound;
    }
    return ret;
}

Result<BoundNodeReference, bool> Binder::bind(NodeReference ast_entrypoint)
{
    errors.clear();
    unbound = 0;
    pass = 0;
    entrypoint = bind_node(ast_entrypoint);
    std::cout << "Pass 0 - AST Transformation\n\n";
    dump(std::cout, entrypoint, "Program");
    list();
    std::cout << "\n";
    if (!errors.empty()) {
        return false;
    }
    for (auto t = bound_nodes[entrypoint].type; !t; t = bound_nodes[entrypoint].type) {
        errors.clear();
        auto prev = unbound;
        unbound = 0;
        ++pass;
        rebind_node(entrypoint);
        std::cout << "\nPass " << pass << " - rebind\n\n";
        dump(std::cout, entrypoint, "Program");
        std::cout << "\n";
        list();
        std::cout << "\n";
        if (prev <= unbound) {
            entrypoint = add_error(*this, entrypoint, "Infinite loop in bind stage");
        }
        if (!errors.empty()) {
            return false;
        }
    }
    return entrypoint;
}

BoundNodeType Binder::type_of(NodeReference ref) const
{
    return static_cast<BoundNodeType>(bound_nodes[ref].impl.index());
}

void Binder::push_namespace(BoundNodeReference ref)
{
    namespaces.emplace_back(ref);
}

void Binder::set_name(std::string_view name, NodeReference ref)
{
    assert(!namespaces.empty());
    auto &ns = bound_nodes[namespaces.back()];
    ns.names.emplace(name, ref);
}

void Binder::pop_namespace()
{
    assert(!namespaces.empty());
    namespaces.pop_back();
}

std::optional<NodeReference> Binder::resolve(std::string_view name)
{
    assert(!namespaces.empty());
    for (ssize_t depth = static_cast<ssize_t>(namespaces.size()) - 1; depth >= 0; --depth) {
        auto &ns = bound_nodes[namespaces[depth]];
        if (auto it = ns.names.find(name); it != ns.names.end()) {
            return it->second;
        }
    }
    return {};
}

void Binder::to_string(std::ostream &out, BoundNodeReference ref)
{
    return std::visit(
        [&out, this](auto const &impl) {
            to_string<std::decay_t<decltype(impl)>>(out, *this, impl);
        },
        bound_nodes[ref].impl);
}

void Binder::list(std::ostream &out)
{
    for (auto const &node : bound_nodes) {
        out << node.ref << ". " << node.type_name() << " ";
        to_string(out, node.ref);
        if (node.type) {
            out << " " << registry[*node.type].name;
        }
        out << "\n";
    }
}

void Binder::dump(std::ostream &out, BoundNodeReference ref, std::string_view caption, int indent)
{
    if (indent == 0 && !errors.empty()) { // Hack
        out << "Errors:\n";
        for (auto const &err : errors) {
            auto const &n = bound_nodes[err];
            out << std::format("{}", n.location) << std::get<BindError>(n.impl).message << "\n";
        }
    }
    BoundNode &node = bound_nodes[ref];
    out << std::string(indent, ' ') << caption << ": " << ref << ". " << node.type_name();

    if (node.type) {
        out << " " << registry[*node.type].name;
    }
    out << "\n"
        << std::string(indent + 2, ' ');
    to_string(out, ref);
    out << "\n";
    return std::visit(
        [&out, this, indent](auto const &impl) {
            dump<std::decay_t<decltype(impl)>>(out, *this, impl, indent + 2);
        },
        node.impl);
}

void Binder::dump(std::ostream &out, std::optional<BoundNodeReference> ref, std::string_view caption, int indent)
{
    if (ref) {
        dump(out, *ref, caption, indent);
    } else {
        out << std::string(indent, ' ') << caption << ": [empty]" << '\n';
    }
}

void Binder::dump(std::ostream &out, BoundNodeReferences refs, std::string_view caption, int indent)
{
    if (refs.empty()) {
        out << std::string(indent, ' ') << caption << ": [empty]" << '\n';
    } else {
        for (auto ref : refs) {
            dump(out, ref, caption, indent);
        }
    }
}

}
