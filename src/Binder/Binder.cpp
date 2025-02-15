/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iostream>
#include <map>
#include <optional>
#include <ostream>
#include <print>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeindex>
#include <variant>
#include <vector>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <Lexer/Lexer.h>
#include <Type/Type.h>
#include <Type/Value.h>

#include <Lib.h>
#include <Logging.h>
#include <Result.h>
#include <ScopeGuard.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BindError

template<>
BoundNodeReference rebind<BindError>(Binder &binder, BoundNodeReference ref)
{
    if (binder.pass == IMPL.pass) {
        return ref;
    }
    auto ret = binder.rebind_node(IMPL.node);
    // std::println("rebind<BindError> {}. {}: \"{}\" -> {} {}",
    //     ref,
    //     IMPL.node,
    //     IMPL.message,
    //     ret,
    //     binder[ret].type_name()
    // );
    return ret;
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

template<>
std::string_view to_string<BindError>(BindError const &node)
{
    return "BindError";
}

}

#include "ArrayType.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundAssignmentExpression

template<>
BoundNodeReference bind<AssignmentExpression>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<AssignmentExpression>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundAssignmentExpression>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.left = binder.bind_node(ast_impl.left, ref);
    IMPL.right = binder.bind_node(ast_impl.right, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundAssignmentExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.left = binder.rebind_node(IMPL.left);
    IMPL.right = binder.rebind_node(IMPL.right);
    if (!binder[IMPL.left].type || !binder[IMPL.right].type) {
        return ref;
    }
    if (std::holds_alternative<BoundBinaryExpression>(binder[IMPL.left].impl)) {
        auto left = std::get<BoundBinaryExpression>(binder[IMPL.left].impl);
        if (left.op != BinaryOperator::MemberAccess && left.op != BinaryOperator::Subscript) {
            return add_error(binder, ref, "Assignment requires identifier, subscript, or member access LHS value");
        }
    } else if (!std::holds_alternative<BoundIdentifier>(binder[IMPL.left].impl)) {
        return add_error(binder, ref, "Assignment requires identifier, subscript, or member access LHS value");
    }
    auto left_type = binder.registry[*binder[IMPL.left].type];
    auto right_types = binder.alternatives(IMPL.left, left_type.ref);
    if (right_types.preferred == left_type.ref) {
        binder[ref].type = left_type.ref;
        return ref;
    } else {
        for (auto type : right_types.alternatives) {
            if (type == left_type.ref) {
                binder[ref].type = binder[IMPL.left].type;
                IMPL.right = binder.accept(IMPL.right, type);
                return ref;
            }
        }
        return add_error(binder, ref, "Cannot assign expression of type '{}' to variable of type '{}'", binder.registry[*binder[IMPL.right].type].name, left_type.name);
    }
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

template<>
std::string_view to_string<BoundAssignmentExpression>(BoundAssignmentExpression const &node)
{
    return "BoundAssignmentExpression";
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
BoundNodeReference bind<BasicTypeNode>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<BasicTypeNode>(ast_node.impl);
    BoundNodeReference ref = add_node<BasicTypeNode>(binder, ast_node.ref, ast_node.location, parent);
    binder[ref].impl = ast_impl;
    binder[ref].type = binder.registry.find(IMPL.name);
    return ref;
}

}

#include "BinaryExpression.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundBlock

template<>
BoundNodeReference rebind<BoundBlock>(Binder &binder, BoundNodeReference ref)
{
    auto print_node = [&binder, ref]() {
        for (size_t ix = 0; ix < IMPL.statements.size(); ++ix) {
            std::println("{}. {} ({}) -> {}",
                ix,
                binder.bound_nodes[IMPL.statements[ix]].type_name(),
                IMPL.statements[ix],
                binder[IMPL.statements[ix]].type.has_value() ? "bound" : "unbound");
        }
    };
    // print_node();
    binder.push_namespace(ref);
    ScopeGuard sg {
        [&print_node, &binder]() {
            // print_node();
            binder.pop_namespace();
        }
    };
    bool all_bound = true;
    for (size_t ix = 0; ix < IMPL.statements.size(); ++ix) {
        IMPL.statements[ix] = binder.rebind_node(IMPL.statements[ix]);
        all_bound &= binder[IMPL.statements[ix]].type.has_value();
    }
    if (all_bound && !binder[ref].type) {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
BoundNodeReference bind<Block>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const        &ast_node = binder.ast[ast_ref];
    auto const        &ast_impl = std::get<Block>(ast_node.impl);
    BoundNodeReference ref = add_node<BoundBlock>(binder, ast_node.ref, ast_node.location, parent);
    if (ast_impl.label) {
        IMPL.label = std::get<Label>(binder.ast[*ast_impl.label].impl).label;
    }
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto const &stmt : ast_impl.statements) {
            auto bound_stmt = binder.bind_node(stmt, ref);
            IMPL.statements.push_back(bound_stmt);
        }
    }
    return ref;
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
#define STRUCT BoundCoercion

template<>
void dump(std::ostream &out, Binder &binder, BoundCoercion const &impl, int indent)
{
    binder.dump(out, impl.expression, "Expression", indent);
}

}

#include "Break.h"
#include "Constant.h"
#include "ConstantDeclaration.h"
#include "Continue.h"
#include "For.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundForeignFunction

template<>
BoundNodeReference bind<ForeignFunction>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<ForeignFunction>(ast_node.impl);
    auto        ref = add_node<BoundForeignFunction>(binder, ast_node.ref, ast_node.location, parent);

    binder[ref].type = VoidType;
    IMPL.foreign_name = ast_impl.foreign_name;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundForeignFunction const &impl)
{
    out << impl.foreign_name;
}

#undef STRUCT
#define STRUCT BoundFunction

template<>
BoundNodeReference rebind<BoundFunction>(Binder &binder, BoundNodeReference ref)
{
    // ScopeGuard print_node {
    //     [&binder, ref]() {
    //         std::println("rebind<BoundFunction> {}. {} #Params {} {}",
    //             ref,
    //             IMPL.name,
    //             IMPL.parameters.size(),
    //             IMPL.return_type ? (binder[*IMPL.return_type].type.has_value() ? "bound" : "unbound") : "void");
    //         for (auto ix = 0; ix < IMPL.parameters.size(); ++ix) {
    //             std::println("* {}. {} {}",
    //                 ix,
    //                 I(BoundParameter, IMPL.parameters[ix]).name,
    //                 binder[IMPL.parameters[ix]].type.has_value() ? "bound" : "unbound");
    //         }
    //         std::println("Impl: {} {} {}",
    //             IMPL.implementation,
    //             binder[IMPL.implementation].type_name(),
    //             binder[IMPL.implementation].type.has_value() ? "bound" : "unbound");
    //     }
    // };
    auto all_bound { true };
    for (auto ix = 0; ix < IMPL.parameters.size(); ++ix) {
        IMPL.parameters[ix] = binder.rebind_node(IMPL.parameters[ix]);
        all_bound &= binder[IMPL.parameters[ix]].type.has_value();
    }

    binder.set_name(IMPL.name, ref);
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto param_ref : IMPL.parameters) {
            auto const &param = I(BoundParameter, param_ref);
            binder.set_name(param.name, param_ref);
        }
        IMPL.implementation = binder.rebind_node(IMPL.implementation);
    }
    all_bound &= binder[IMPL.implementation].type.has_value();

    TypeReference type_ref = VoidType;
    if (IMPL.return_type) {
        IMPL.return_type = binder.rebind_node(*IMPL.return_type);
        all_bound &= binder[*IMPL.return_type].type.has_value();
        type_ref = *binder[*IMPL.return_type].type;
    }
    if (all_bound) {
        binder[ref].type = type_ref;
    }

    return ref;
}

template<>
BoundNodeReference bind<Function>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Function>(ast_node.impl);
    auto        ref = add_node<BoundFunction>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.name = ast_impl.name;
    if (ast_impl.return_type) {
        IMPL.return_type = binder.bind_node(*ast_impl.return_type, ref);
    }
    for (auto param : ast_impl.parameters) {
        auto param_ref = binder.bind_node(param, ref);
        IMPL.parameters.push_back(param_ref);
    }
    if (ast_impl.implementation == 0) {
        std::println("--> {}", ast_impl.name);
        UNREACHABLE();
    }
    IMPL.implementation = binder.bind_node(ast_impl.implementation, ref);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundFunction const &impl)
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
}

template<>
void dump(std::ostream &out, Binder &binder, BoundFunction const &impl, int indent)
{
    binder.dump(out, impl.implementation, "Implementation", indent);
}

}

#include "FunctionCall.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundFunctionImplementation

template<>
BoundNodeReference bind<FunctionImplementation>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FunctionImplementation>(ast_node.impl);
    auto        ref = add_node<BoundFunctionImplementation>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.implementation = binder.bind_node(ast_impl.implementation, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundFunctionImplementation>(Binder &binder, BoundNodeReference ref)
{
    IMPL.implementation = binder.rebind_node(IMPL.implementation);
    if (binder[IMPL.implementation].type.has_value()) {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundFunctionImplementation const &impl, int indent)
{
    binder.dump(out, impl.implementation, "Implementation", indent);
}

}

#include "Identifier.h"

namespace Arwen {

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
    if (binder[IMPL.condition].type) {
        if (binder.registry[PrimitiveType::Bool].ref != *binder[IMPL.condition].type) {
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
    }
    return ref;
}

template<>
BoundNodeReference bind<If>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<If>(ast_node.impl);
    auto        ref = add_node<BoundIf>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.condition = binder.bind_node(ast_impl.condition, ref);
    IMPL.true_branch = binder.bind_node(ast_impl.true_branch, ref);
    if (ast_impl.false_branch) {
        IMPL.false_branch = binder.bind_node(*ast_impl.false_branch, ref);
    }
    return ref;
}

#undef STRUCT
#define STRUCT BoundIntrinsic

template<>
BoundNodeReference bind<Intrinsic>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Intrinsic>(ast_node.impl);
    auto        ref = add_node<BoundIntrinsic>(binder, ast_node.ref, ast_node.location, parent);

    binder[ref].type = VoidType;
    IMPL.name = ast_impl.name;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundIntrinsic const &impl)
{
    out << impl.name;
}

#undef STRUCT
#define STRUCT BoundLoop

template<>
BoundNodeReference bind<Loop>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Loop>(ast_node.impl);
    auto        ref = add_node<BoundLoop>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.body = binder.bind_node(ast_impl.body, ref);
    return ref;
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
BoundNodeReference bind<Member>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Member>(ast_node.impl);
    auto        ref = add_node<BoundMember>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.name = ast_impl.name;
    binder[ref].type = VoidType;
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
        IMPL.names[ix] = binder.rebind_node(IMPL.names[ix]);
        all_bound &= binder[IMPL.names[ix]].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
BoundNodeReference bind<Module>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Module>(ast_node.impl);
    auto        ref = add_node<BoundModule>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.name = ast_impl.name;
    {
        binder.push_namespace(ref);
        ScopeGuard sg {
            [&binder]() {
                binder.pop_namespace();
            }
        };
        for (auto decl_ref : ast_impl.names) {
            auto bound_decl = binder.bind_node(decl_ref, ref);
            IMPL.names.push_back(bound_decl);
        }
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundModule const &impl)
{
    out << impl.name;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundModule const &impl, int indent)
{
    // binder.dump(out, impl.names, "Name", indent);
}

#undef STRUCT
#define STRUCT Nullptr

template<>
BoundNodeReference bind<Nullptr>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Nullptr>(ast_node.impl);
    auto        ref = add_node<Nullptr>(binder, ast_node.ref, ast_node.location, parent);
    binder[ref].type = binder.registry[PrimitiveType::Ptr].ref;
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &, Nullptr const &impl)
{
    std::cout << "nullptr";
}

#undef STRUCT
#define STRUCT BoundParameter

template<>
BoundNodeReference rebind<BoundParameter>(Binder &binder, BoundNodeReference ref)
{
    IMPL.type = binder.rebind_node(IMPL.type);
    if (binder[IMPL.type].type) {
        binder[ref].type = binder[IMPL.type].type;
    }
    return ref;
}

template<>
BoundNodeReference bind<Parameter>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Parameter>(ast_node.impl);
    auto        ref = add_node<BoundParameter>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.name = ast_impl.name;
    IMPL.type = binder.bind_node(ast_impl.type, ref);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundParameter const &impl)
{
    out << impl.name;
}

#undef STRUCT
#define STRUCT BoundPointerType

template<>
BoundNodeReference bind<PointerType>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<PointerType>(ast_node.impl);
    auto        ref = add_node<BoundPointerType>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.element_type = binder.bind_node(ast_impl.element_type, ref);
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
        IMPL.modules[ix] = binder.rebind_node(IMPL.modules[ix]);
        all_bound &= binder[IMPL.modules[ix]].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
BoundNodeReference bind<Program>(Binder &binder, NodeReference ast_ref, BoundNodeReference)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Program>(ast_node.impl);
    auto        ref = add_node<BoundProgram>(binder, ast_node.ref, ast_node.location, 0);

    binder.push_namespace(ref);
    ScopeGuard sg {
        [&binder]() {
            binder.pop_namespace();
        }
    };

    for (auto mod_ref : ast_impl.modules) {
        auto bound_decl = binder.bind_node(mod_ref, ref);
        IMPL.modules.push_back(bound_decl);
        if (I(BoundModule, bound_decl).name == "#builtin") {
            for (auto n : I(BoundModule, bound_decl).names) {
                auto name = std::visit(overload {
                                           [](BoundFunction const &impl) {
                                               return impl.name;
                                           },
                                           [](BoundConstantDeclaration const &impl) {
                                               return impl.name;
                                           },
                                           [](BoundVariableDeclaration const &impl) {
                                               return impl.name;
                                           },
                                           [](auto const &impl) -> std::string_view {
                                               fatal("Why is there a {} in the builtin module?", typeid(decltype(impl)).name());
                                               return "";
                                           } },
                    binder[n].impl);
                binder.set_name(name, n);
            }
        }
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundProgram const &impl, int indent)
{
    // binder.dump(out, impl.modules, "Module", indent);
}

template<>
std::string_view to_string<BoundProgram>(BoundProgram const &node)
{
    return "Program";
}

}

#include "Range.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundReturn

template<>
BoundNodeReference rebind<BoundReturn>(Binder &binder, BoundNodeReference ref)
{
    if (IMPL.expression) {
        IMPL.expression = binder.rebind_node(*IMPL.expression);
        binder[ref].type = binder[*IMPL.expression].type;
    } else {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
BoundNodeReference bind<Return>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Return>(ast_node.impl);
    auto        ref = add_node<BoundReturn>(binder, ast_node.ref, ast_node.location, parent);

    if (ast_impl.expression) {
        IMPL.expression = binder.bind_node(*ast_impl.expression, ref);
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundReturn const &impl, int indent)
{
    binder.dump(out, impl.expression, "Expression", indent);
}

}

#include "SliceType.h"

namespace Arwen {

#undef STRUCT
#define STRUCT BoundSubscript

template<>
BoundNodeReference bind<Subscript>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<Subscript>(ast_node.impl);
    auto        ref = add_node<BoundSubscript>(binder, ast_node.ref, ast_node.location, parent);
    for (auto arg : ast_impl.subscripts) {
        IMPL.subscripts.push_back(binder.bind_node(arg, ref));
    }
    return ref;
}

template<>
BoundNodeReference rebind<BoundSubscript>(Binder &binder, BoundNodeReference ref)
{
    bool all_bound { true };
    for (auto ix = 0; ix < IMPL.subscripts.size(); ++ix) {
        auto arg = IMPL.subscripts[ix];
        auto new_arg = binder.rebind_node(arg);
        if (auto arg_type_ref = binder[new_arg].type; arg_type_ref) {
            auto type = binder.registry[*arg_type_ref];
            if (type.is_integer()) {
                new_arg = binder.accept(new_arg, static_cast<TypeReference>(PrimitiveType::I64));
            } else {
                add_error(binder, arg, "Subscript must be an integer");
                all_bound = false;
            }
        }
        if (new_arg != arg) {
            IMPL.subscripts[ix] = new_arg;
            arg = new_arg;
        }
        all_bound &= binder[arg].type.has_value();
    }
    if (all_bound) {
        binder[ref].type = VoidType;
    }
    return ref;
}

template<>
void dump(std::ostream &out, Binder &binder, BoundSubscript const &impl, int indent)
{
    binder.dump(out, impl.subscripts, "Subscripts", indent);
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundSubscript const &impl)
{
    std::string_view sep = "[";
    for (auto sub : impl.subscripts) {
        out << sep;
        binder.to_string(out, sub);
        sep = ", ";
    }
    out << "]";
}

#undef STRUCT
#define STRUCT BoundUnaryExpression

template<>
BoundNodeReference bind<UnaryExpression>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<UnaryExpression>(ast_node.impl);
    auto        ref = add_node<BoundUnaryExpression>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.op = ast_impl.op;
    IMPL.operand = binder.bind_node(ast_impl.operand, ref);
    return ref;
}

template<>
BoundNodeReference rebind<BoundUnaryExpression>(Binder &binder, BoundNodeReference ref)
{
    IMPL.operand = binder.rebind_node(IMPL.operand);
    if (!binder[IMPL.operand].type) {
        return ref;
    }

    UnaryOperatorMapping m { IMPL.op };
    auto                 flat = std::visit(
        overload {
            [&](BoundConstant &o) -> BoundNodeReference {
                if (auto result = m(o.value); result) {
                    o.value = *result;
                    binder[IMPL.operand].type = o.value.type();
                    binder[IMPL.operand].parent = binder[ref].parent;
                    return IMPL.operand;
                }
                return add_error(binder, ref, "Cannot use operator '{}' with constant operand '{}' of type '{}'",
                                    m.op, o.value, binder.registry[*binder[IMPL.operand].type].name);
            },
            [ref](auto &) -> BoundNodeReference {
                return ref;
            },
        },
        binder[IMPL.operand].impl);

    if (flat != ref) {
        return flat;
    }

    auto operand_types = binder.alternatives(IMPL.operand, *binder[IMPL.operand].type);
    auto test_type = [&](TypeReference t) {
        if (auto c = compatible(IMPL.op, static_cast<PrimitiveType>(t)); c) {
            binder[ref].type = static_cast<TypeReference>(*c);
            return true;
        }
        return false;
    };

    if (test_type(operand_types.preferred)) {
        return ref;
    }
    for (auto t : operand_types.alternatives) {
        if (test_type(t)) {
            IMPL.operand = binder.accept(IMPL.operand, t);
            return ref;
        }
    }
    add_error(binder, ref, "Cannot use operator '{}' with operand '{}'", to_string(IMPL.op), binder.registry[operand_types.preferred].name);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundUnaryExpression const &impl)
{
    out << to_string(impl.op) << " ";
    binder.to_string(out, impl.operand);
}

template<>
void dump(std::ostream &out, Binder &binder, BoundUnaryExpression const &impl, int indent)
{
    binder.dump(out, impl.operand, "Operand", indent);
}

#undef STRUCT
#define STRUCT BoundVariableDeclaration

template<>
BoundNodeReference bind<VariableDeclaration>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<VariableDeclaration>(ast_node.impl);
    auto        ref = add_node<BoundVariableDeclaration>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.name = ast_impl.name;
    if (ast_impl.type) {
        IMPL.type = binder.bind_node(*ast_impl.type, ref);
    }
    if (ast_impl.initializer) {
        IMPL.initializer = binder.bind_node(*ast_impl.initializer, ref);
    }
    return ref;
}

template<>
BoundNodeReference rebind<BoundVariableDeclaration>(Binder &binder, BoundNodeReference ref)
{
    binder.set_name(IMPL.name, ref);
    if (IMPL.initializer) {
        IMPL.initializer = binder.rebind_node(*IMPL.initializer);
        if (binder[*IMPL.initializer].type) {
            if (IMPL.type) {
                auto left_type = binder.registry[*binder[*IMPL.type].type];
                auto right_types = binder.alternatives(*IMPL.initializer, left_type.ref);
                if (right_types.preferred == left_type.ref) {
                    binder[ref].type = binder[*IMPL.type].type;
                    return ref;
                } else {
                    for (auto type : right_types.alternatives) {
                        if (type == left_type.ref) {
                            IMPL.initializer = binder.accept(*IMPL.initializer, type);
                            binder[ref].type = binder[*IMPL.type].type;
                            return ref;
                        }
                    }
                    return add_error(binder, ref, "Cannot initialize variable of type '{}' with expression of type '{}'", left_type.name, binder.registry[*binder[*IMPL.initializer].type].name);
                }
            } else {
                binder[ref].type = binder[*IMPL.initializer].type;
            }
        }
    } else if (IMPL.type) {
        binder[ref].type = binder[*IMPL.type].type;
    } else {
        return add_error(binder, ref, "Uninitialized variable must have type");
    }
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundVariableDeclaration const &impl)
{
    out << impl.name;
    if (impl.type) {
        out << ": ";
        binder.to_string(out, *impl.type);
    }
}

template<>
void dump(std::ostream &out, Binder &binder, BoundVariableDeclaration const &impl, int indent)
{
    binder.dump(out, impl.type, "Type", indent);
    binder.dump(out, impl.initializer, "Initializer", indent);
}

#undef STRUCT
#define STRUCT BoundWhile

template<>
BoundNodeReference rebind<BoundWhile>(Binder &binder, BoundNodeReference ref)
{
    IMPL.condition = binder.rebind_node(IMPL.condition);
    IMPL.body = binder.rebind_node(IMPL.body);
    if (binder[IMPL.condition].type) {
        if (binder.registry[PrimitiveType::Bool].ref != *binder[IMPL.condition].type) {
            return add_error(binder, ref, "'while' condition is not a boolean");
        }
        if (binder[IMPL.body].type) {
            auto type = *binder[IMPL.body].type;
            // TODO: Type compatibility
            binder[ref].type = type;
        }
    }
    return ref;
}

template<>
BoundNodeReference bind<While>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<While>(ast_node.impl);
    auto        ref = add_node<BoundWhile>(binder, ast_node.ref, ast_node.location, parent);

    IMPL.condition = binder.bind_node(ast_impl.condition, ref);
    IMPL.body = binder.bind_node(ast_impl.body, ref);
    return ref;
}

template<>
void to_string(std::ostream &out, Binder &binder, BoundWhile const &impl)
{
    to_string(out, binder, impl.condition);
}

template<>
void dump(std::ostream &out, Binder &binder, BoundWhile const &impl, int indent)
{
    binder.dump(out, impl.condition, "Condition", indent);
    binder.dump(out, impl.body, "Body", indent);
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
    return std::visit(
        [](auto impl) -> std::string_view {
            using T = std::decay_t<decltype(impl)>;
            return type_names[std::type_index(typeid(T))];
        },
        impl);
}

TypeAlternatives Binder::alternatives(BoundNodeReference ref, TypeReference hint)
{
    auto const &node = bound_nodes[ref];
    return std::visit(
        [this, ref, hint](auto &impl) -> TypeAlternatives {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::alternatives<T>(*this, ref, hint);
        },
        node.impl);
}

BoundNodeReference Binder::accept(BoundNodeReference ref, TypeReference type)
{
    auto const &node = bound_nodes[ref];
    auto        ret = ref;
    if (node.type) {
        ret = std::visit(
            [this, ref, type](auto &impl) -> BoundNodeReference {
                using T = std::decay_t<decltype(impl)>;
                return Arwen::accept<T>(*this, ref, type);
            },
            node.impl);
    }
    return ret;
}

BoundNodeReference Binder::bind_node(NodeReference ast_ref, BoundNodeReference parent)
{
    static bool fuck_off = false;
    auto const &ast_node = ast[ast_ref];
    assert(ast_ref || !fuck_off);
    fuck_off = true;
    auto ret = std::visit(
        [this, ast_node, parent](auto &impl) -> BoundNodeReference {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::bind<T>(*this, ast_node.ref, parent);
        },
        ast_node.impl);
    return rebind_node(ret);
}

BoundNodeReference Binder::rebind_node(BoundNodeReference ref)
{
    auto const &node = bound_nodes[ref];
    auto        ret = ref;
    if (visited.contains(ref)) {
        return ref;
    }
    if (!node.type) {
        ret = std::visit(
            [this, node](auto &impl) -> BoundNodeReference {
                using T = std::decay_t<decltype(impl)>;
                return Arwen::rebind<T>(*this, node.ref);
            },
            node.impl);
        visited.insert(ret);
        if (!bound_nodes[ret].type) {
            still_unbound.insert(ref);
        }
    }
    return ret;
}

Result<BoundNodeReference, bool> Binder::bind(NodeReference ast_entrypoint)
{
    errors.clear();
    pass = 0;
    entrypoint = bind_node(ast_entrypoint);
    if (log) {
        std::println("Pass 0 - AST Transformation");
        std::println("");
        dump(std::cout, entrypoint, "Program");
        std::println("");
        std::println("Visited: {}", visited);
        std::println("Unbound: {}", still_unbound);
    }

    if (!errors.empty()) {
        if (log) {
            std::println("Errors after pass 0:");
            for (auto const &err : errors) {
                auto const &node = bound_nodes[err];
                std::println("{}: {}", node.location, std::get<BindError>(node.impl).message);
            }
        }
        return false;
    }

    if (!still_unbound.empty() && log) {
        std::println("{} unbound nodes after pass 0", still_unbound.size());
    }

    while (!still_unbound.empty()) {
        std::set<BoundNodeReference> prev { still_unbound };
        still_unbound.clear();
        visited.clear();
        errors.clear();
        ++pass;
        rebind_node(entrypoint);
        if (log) {
            std::println("Pass {} - rebind", pass);
            dump(std::cout, entrypoint, "Program");
            std::println("Visited: {}", visited);
            std::println("Unbound: {}", still_unbound);
        }
        if (still_unbound.empty() && errors.empty()) {
            if (log) {
                std::println("No unbound nodes and no errors after pass {}", pass);
            }
            break;
        }
        if (log) {
            std::println("{} unbound nodes after pass {}", still_unbound.size(), pass);
        }

        if (!errors.empty()) {
            if (log) {
                std::println("Errors after pass {}:", pass);
                for (auto const &err : errors) {
                    auto const &node = bound_nodes[err];
                    std::println("{}: {}", node.location, std::get<BindError>(node.impl).message);
                }
            }
            return false;
        }
        if (prev == still_unbound) {
            for (auto ref : still_unbound) {
                auto const &node = bound_nodes[ref];
                std::print("Unbound node {}: {} ", ref, node.type_name());
                to_string(std::cout, ref);
                std::println("");
            }
            entrypoint = add_error(*this, entrypoint, "Infinite loop in bind stage");
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
    if (log) {
        std::println("push namespace {}", bound_nodes[ref].type_name());
    }
    namespaces.emplace_back(ref);
}

void Binder::set_name(std::string_view name, NodeReference ref)
{
    assert(!namespaces.empty());
    if (log) {
        std::println("set name '{}' -> {} in {} {}", name, ref, namespaces.back(), bound_nodes[namespaces.back()].type_name());
    }
    auto &ns = bound_nodes[namespaces.back()];
    ns.names.emplace(name, ref);
}

void Binder::pop_namespace()
{
    assert(!namespaces.empty());
    if (log) {
        std::println("pop namespace {}", bound_nodes[namespaces.back()].type_name());
    }
    namespaces.pop_back();
}

std::optional<NodeReference> Binder::resolve(std::string_view name)
{
    assert(!namespaces.empty());
    if (log)
        std::println("resolve name '{}'", name);
    for (int64_t depth = static_cast<int64_t>(namespaces.size()) - 1; depth >= 0; --depth) {
        if (log)
            std::print("resolve name '{}' in {} {}... ", name, namespaces[depth], bound_nodes[namespaces[depth]].type_name());
        auto &ns = bound_nodes[namespaces[depth]];
        if (auto it = ns.names.find(name); it != ns.names.end()) {
            if (log)
                std::println("Found!");
            return it->second;
        }
        if (log)
            std::println("");
    }
    if (log)
        std::println("Name '{}' not found", name);
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
    if (ref == entrypoint && !errors.empty()) {
        out << "Errors:\n";
        for (auto const &err : errors) {
            auto const &n = bound_nodes[err];
            out << std::format("{}", n.location) << std::get<BindError>(n.impl).message << "\n";
        }
    }
    BoundNode &node = bound_nodes[ref];
    out << std::string(indent, ' ') << caption << ": " << ref << ". " << node.type_name();

    out << " \033[";
    if (node.type) {
        out << "32m" << registry[*node.type].name;
    } else {
        out << "31mUnbound";
    }
    out << "\033[0m | ";
    to_string(out, ref);
    out << "\n";
    for (auto const &n : node.names) {
        dump(out, n.second, "Name", indent + 2);
    }
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
