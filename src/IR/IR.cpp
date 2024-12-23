/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Type/Value.h"
#include <cstdint>
#include <iostream>
#include <optional>
#include <print>
#include <string>
#include <type_traits>
#include <variant>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/IR.h>
#include <Type/Type.h>

#include <Lib.h>
#include <Logging.h>
#include <Result.h>

namespace Arwen::IR {

using namespace Arwen;

template<typename Container>
void map(BoundNodeReference ref, Binder &binder, Container &container)
{
    std::visit(
        [ref, &binder, &container](auto const &impl) {
            map<std::decay_t<decltype(impl)>>(ref, impl, binder, container);
        },
        binder[ref].impl);
}

template<typename NodeImpl, typename Container>
void map(BoundNodeReference decl, NodeImpl const &impl, Binder &, Container &ir)
{
}

template<typename Container>
void generate(BoundNodeReference ref, Binder &binder, Container &container)
{
    std::visit(
        [ref, &binder, &container](auto const &impl) {
            generate<std::decay_t<decltype(impl)>>(ref, impl, binder, container);
        },
        binder[ref].impl);
}

template<class NodeImpl, class Container>
void generate(BoundNodeReference ref, NodeImpl const &, Binder &binder, Container &)
{
    std::cerr << "No generator for " << typeid(NodeImpl).name() << " in " << typeid(Container).name() << "\n";
}

template<typename OpImpl>
void link(Ref ref, OpImpl &impl, Function &function)
{
    if (function.ops[ref].target) {
        std::cerr << "No linker for " << typeid(OpImpl).name() << " but there is a target\n";
    }
}

template<>
void generate(BoundNodeReference ref, BoundAssignmentExpression const &impl, Binder &binder, Function &ir)
{
    generate(impl.right, ir.program.binder, ir);
    std::visit(
        overload {
            [&](BoundIdentifier const &id) {
                ir.add_op<PopVariable>(ir.get_variable_address(*id.declaration), *binder[ref].type);
            },
            [&](BoundBinaryExpression const &binex) {
                if (binex.op != BinaryOperator::Subscript) {
                    UNREACHABLE();
                }
                generate(binex.right, ir.program.binder, ir);
                assert(binder.type_of(binex.left) == BoundNodeType::BoundIdentifier);
                auto type = binder.registry[*binder[binex.left].type];
                auto elem_type = std::visit(
                    overload {
                        [&](Array const &arr) {
                            return arr.element_type;
                        },
                        [&](Slice const &slice) {
                            return slice.element_type;
                        },
                        [&](Pointer const &ptr) {
                            return ptr.element_type;
                        },
                        [&](auto const &) {
                            UNREACHABLE();
                            return VoidType;
                        },
                    },
                    type.typespec.payload());
                auto lhs = ir.get_variable_address(*I(BoundIdentifier, binex.left).declaration);
                ir.add_op<PopArrayElement>(lhs, elem_type);
            },
            [](auto const &) {
                UNREACHABLE();
            },
        },
        ir.program.binder[impl.left].impl);
}

template<>
void generate(BoundNodeReference ref, BoundBinaryExpression const &impl, Binder &binder, Function &ir)
{
    generate(impl.right, binder, ir);
    switch (impl.op) {
    case BinaryOperator::MemberAccess: {
        assert(binder.type_of(impl.left) == BoundNodeType::BoundIdentifier);
        auto const &lhs = I(BoundIdentifier, impl.left);
        NYI();
        // ir.add_op<PushVariableRef>(  );
    } break;
    case BinaryOperator::Subscript: {
        assert(binder.type_of(impl.right) == BoundNodeType::BoundSubscript);
        assert(binder.type_of(impl.left) == BoundNodeType::BoundIdentifier);
        auto const &lhs = I(BoundIdentifier, impl.left);
        auto array_type = binder.registry[*binder[impl.left].type];
        ir.add_op<PushArrayElement>(ir.get_variable_address(*lhs.declaration), *binder[impl.left].type);
        return;
    } break;
    default:
        generate(impl.left, binder, ir);
        break;
    }
    ir.add_op<BinaryOperation>(*binder[impl.left].type, impl.op, *binder[impl.right].type);
}

template<>
void generate(BoundNodeReference, BoundConstant const &impl, Binder &, Function &ir)
{
    ir.add_op<PushConstant>(impl.value);
}

template<>
void generate(BoundNodeReference ref, BoundBlock const &impl, Binder &binder, Function &ir)
{
    if (impl.label) {
        ir.add_op<Label>(*impl.label);
    }
    ir.depth_stack.push_back(ir.current_depth);
    for (auto stmt : impl.statements) {
        map(stmt, binder, ir);
    }
    for (auto stmt : impl.statements) {
        generate(stmt, binder, ir);
    }
    if (ir.current_depth > ir.variable_depth) {
        ir.variable_depth = ir.current_depth;
    }
    ir.current_depth = ir.depth_stack.back();
    ir.depth_stack.pop_back();
    ir.scopes[ref] = ir.ops.size();
}

template<>
void generate(BoundNodeReference ref, BoundBreak const &impl, Binder &binder, Function &ir)
{
    if (impl.expression) {
        generate(*impl.expression, binder, ir);
    }
    ir.add_op<Break>(0ul);
    ir.ops.back().target = impl.block;
}

template<>
void link(Ref ref, Break &impl, Function &function)
{
    auto &op = function.ops[ref];
    if (op.target) {
        assert(function.scopes.contains(*op.target));
        impl.target = function.scopes[*op.target];
        op.target = {};
    }
}

template<>
void map(BoundNodeReference ref, BoundConstantDeclaration const &impl, Binder &binder, Module &ir)
{
    if (std::holds_alternative<BoundConstant>(binder[impl.initializer].impl)) {
        return;
    }
    auto type = binder.registry[*binder[ref].type];
    ir.program.depth += align_at(type.size(), 8);
    ir.program.variables[ref] = ir.program.depth;
}

template<>
void map(BoundNodeReference ref, BoundConstantDeclaration const &impl, Binder &binder, Function &ir)
{
    if (std::holds_alternative<BoundConstant>(binder[impl.initializer].impl)) {
        return;
    }
    auto type = binder.registry[*binder[ref].type];
    ir.current_depth += align_at(type.size(), 8);
    ir.variables[ref] = ir.parameter_depth + ir.current_depth;
}

template<>
void generate(BoundNodeReference ref, BoundConstantDeclaration const &impl, Binder &binder, Function &ir)
{
    if (std::holds_alternative<BoundConstant>(binder[impl.initializer].impl)) {
        return;
    }
    auto const &type = binder.registry[*binder[ref].type];
    generate(impl.initializer, ir.program.binder, ir);
    ir.add_op<PopVariable>(ir.get_variable_address(ref), type.ref);
}

template<>
void generate(BoundNodeReference, BoundCoercion const &impl, Binder &, Function &ir)
{
    generate(impl.expression, ir.program.binder, ir);
}

template<>
void generate(BoundNodeReference ref, BoundContinue const &impl, Binder &binder, Function &ir)
{
    ir.add_op<Jump>();
    ir.ops.back().target = impl.block;
}

template<>
void generate(BoundNodeReference ref, BoundFor const &impl, Binder &binder, Function &ir)
{
    ir.depth_stack.push_back(ir.current_depth);
    map(impl.variable_decl, binder, ir);

    generate(impl.variable_decl, binder, ir);

    auto start = ir.ops.size();
    generate(I(BoundRange, impl.range).end, binder, ir);
    ir.add_op<PushVariableValue>(
        ir.get_variable_address(impl.variable_decl),
        *binder[impl.variable_decl].type);
    ir.add_op<BinaryOperation>(*binder[impl.variable_decl].type, BinaryOperator::Less, *binder[impl.variable_decl].type);
    auto jump_to_end_ix = ir.ops.size();
    ir.add_op<JumpF>(0ul);

    generate(impl.body, binder, ir);

    ir.add_op<PushVariableValue>(
        ir.get_variable_address(impl.variable_decl),
        *binder[impl.variable_decl].type);
    u64 one = 1;
    ir.add_op<PushConstant>(Value { 1 }.coerce(*binder[impl.variable_decl].type));
    ir.add_op<BinaryOperation>(*binder[impl.variable_decl].type, BinaryOperator::Add, *binder[impl.variable_decl].type);
    ir.add_op<PopVariable>(ir.get_variable_address(impl.variable_decl), *binder[impl.variable_decl].type);
    ir.add_op<Jump>(start);
    std::get<JumpF>(ir.ops[jump_to_end_ix].op).target = ir.ops.size();

    if (ir.current_depth > ir.variable_depth) {
        ir.variable_depth = ir.current_depth;
    }
    ir.current_depth = ir.depth_stack.back();
    ir.depth_stack.pop_back();
    ir.scopes[ref] = ir.ops.size();
}

template<>
void link(Ref ref, Jump &impl, Function &function)
{
    auto &op = function.ops[ref];
    if (op.target) {
        assert(function.scopes.contains(*op.target));
        impl.target = function.scopes[*op.target];
        op.target = {};
    }
}

template<>
void link(Ref ref, JumpF &impl, Function &function)
{
    auto &op = function.ops[ref];
    if (op.target) {
        assert(function.scopes.contains(*op.target));
        impl.target = function.scopes[*op.target];
        op.target = {};
    }
}

template<>
void link(Ref ref, JumpT &impl, Function &function)
{
    auto &op = function.ops[ref];
    if (op.target) {
        assert(function.scopes.contains(*op.target));
        impl.target = function.scopes[*op.target];
        op.target = {};
    }
}

template<>
void generate(BoundNodeReference ref, BoundFunctionCall const &impl, Binder &binder, Function &ir)
{
    for (auto it = impl.arguments.crbegin(); it != impl.arguments.crend(); ++it) {
        generate(*it, binder, ir);
    }
    assert(impl.function.has_value());
    std::optional<TypeReference> result_type = {};
    if (!impl.discard_result && *binder[ref].type != VoidType) {
        result_type = *binder[ref].type;
    }
    auto func = binder[*impl.function];
    auto implementation = binder[I(BoundFunction, func.ref).implementation];
    std::visit(
        overload {
            [&](BoundFunctionImplementation const &func_impl) {
                ir.add_op<Call>(func_impl.name, func.ref, result_type);
            },
            [&](BoundForeignFunction const &foreign_func) {
                ir.add_op<ForeignCall>(foreign_func.foreign_name, func.ref, result_type);
            },
            [&](BoundIntrinsic const &intrinsic) {
                ir.add_op<Intrinsic>(intrinsic.name, result_type);
            },
            [&](auto const &) {
                std::println("{}", implementation.type_name());
                UNREACHABLE();
            } },
        implementation.impl);
}

template<>
void generate(BoundNodeReference ref, BoundIdentifier const &impl, Binder &binder, Function &ir)
{
    assert(impl.declaration.has_value());
    ir.add_op<PushVariableValue>(
        ir.get_variable_address(*impl.declaration),
        *binder[ref].type);
}

template<>
void generate(BoundNodeReference, BoundIf const &impl, Binder &, Function &ir)
{
    generate(impl.condition, ir.program.binder, ir);
    ir.add_op<JumpF>(0ul);
    auto jump_to_else_ix = ir.ops.size() - 1;
    generate(impl.true_branch, ir.program.binder, ir);
    std::get<JumpF>(ir.ops[jump_to_else_ix].op).target = ir.ops.size();
    if (impl.false_branch) {
        ir.add_op<Jump>(0ul);
        auto jump_to_end_ix = ir.ops.size() - 1;
        std::get<JumpF>(ir.ops[jump_to_else_ix].op).target = ir.ops.size();
        generate(*impl.false_branch, ir.program.binder, ir);
        std::get<Jump>(ir.ops[jump_to_end_ix].op).target = ir.ops.size();
    }
}

template<>
void generate(BoundNodeReference ref, BoundLoop const &impl, Binder &, Function &ir)
{
    auto start = ir.ops.size();
    generate(impl.body, ir.program.binder, ir);
    ir.add_op<Jump>(start);
    ir.scopes[ref] = ir.ops.size();
}

template<>
void generate(BoundNodeReference, BoundMember const &impl, Binder &, Function &ir)
{
    ir.add_op<PushConstant>(impl.name);
}

template<>
void generate(BoundNodeReference, Nullptr const &impl, Binder &, Function &ir)
{
    ir.add_op<PushNullptr>();
}

template<>
void map(BoundNodeReference ref, BoundParameter const &, Binder &binder, Function &ir)
{
    auto type = binder.registry[*binder[ref].type];
    ir.parameter_depth += align_at(type.size(), 8);
    ir.variables[ref] = ir.parameter_depth;
}

template<>
void generate(BoundNodeReference, BoundReturn const &impl, Binder &binder, Function &ir)
{
    std::optional<TypeReference> return_type = {};
    if (impl.expression) {
        generate(*impl.expression, ir.program.binder, ir);
        return_type = binder[*impl.expression].type;
    }
    ir.add_op<FunctionReturn>(return_type);
}

template<>
void generate(BoundNodeReference, BoundSubscript const &impl, Binder &, Function &ir)
{
    for (auto it = impl.subscripts.crbegin(); it != impl.subscripts.crend(); ++it) {
        generate(*it, ir.program.binder, ir);
    }
    ir.add_op<PushConstant>(static_cast<int64_t>(impl.subscripts.size()));
}

template<>
void generate(BoundNodeReference ref, BoundUnaryExpression const &impl, Binder &binder, Function &ir)
{
    generate(impl.operand, binder, ir);
    ir.add_op<UnaryOperation>(*binder[impl.operand].type, impl.op);
}

template<>
void generate(BoundNodeReference ref, BoundVariableDeclaration const &impl, Binder &binder, Function &ir)
{
    auto const &type = binder.registry[*binder[ref].type];
    if (type.typespec.tag() == TypeKind::Array) {
        auto &array_def = I(BoundArrayType, *impl.type);
        auto  element_type = type.typespec.get<TypeKind::Array>().element_type;
        generate(array_def.size, ir.program.binder, ir);
        ir.add_op<MakeArray>(element_type);
        ir.add_op<PopVariable>(ir.get_variable_address(ref), type.ref);
    } else {
        if (impl.initializer) {
            generate(*impl.initializer, ir.program.binder, ir);
            ir.add_op<PopVariable>(ir.get_variable_address(ref), type.ref);
        }
    }
}

template<typename Container>
void map(BoundNodeReference ref, BoundVariableDeclaration const &, Binder &binder, Module &ir)
{
    auto type = binder.registry[*binder[ref].type];
    ir.program.depth += align_at(type.size(), 8);
    ir.program.variables[ref] = ir.program.depth;
}

template<typename Container>
void map(BoundNodeReference ref, BoundVariableDeclaration const &decl, Binder &binder, Function &ir)
{
    auto type = binder.registry[*binder[ref].type];
    ir.variable_depth += align_at(type.size(), 8);
    ir.variables[ref] = ir.parameter_depth + ir.variable_depth;
}

template<>
void generate(BoundNodeReference ref, BoundWhile const &impl, Binder &, Function &ir)
{
    auto start = ir.ops.size();
    generate(impl.condition, ir.program.binder, ir);
    ir.add_op<JumpF>(0ul);
    auto jump_to_end_ix = ir.ops.size() - 1;
    generate(impl.body, ir.program.binder, ir);
    ir.add_op<Jump>(start);
    std::get<JumpF>(ir.ops[jump_to_end_ix].op).target = ir.ops.size();
    ir.scopes[ref] = ir.ops.size();
}

template<>
void generate(BoundNodeReference, BoundForeignFunction const &, Binder &, Module &)
{
}

template<>
void generate(BoundNodeReference ref, BoundFunctionImplementation const &impl, Binder &binder, Module &mod)
{
    auto       &function = mod.functions.back();
    auto const &func = I(BoundFunction, binder[ref].parent);
    for (auto param : func.parameters) {
        map(param, binder, function);
    }
    generate(impl.implementation, binder, function);
    for (auto &op : function.ops) {
        std::visit(
            [&op, &function](auto &impl) {
                link<std::decay_t<decltype(impl)>>(op.ref, impl, function);
            },
            op.op);
    }
}

template<>
void generate(BoundNodeReference, BoundIntrinsic const &, Binder &, Module &)
{
}

template<>
void generate(BoundNodeReference ref, BoundFunction const &func, Binder &binder, Module &mod)
{
    mod.functions.emplace_back(mod.program, mod.ref, mod.functions.size(), ref, func.name);
    mod.function_refs.emplace(mod.functions.back().name, mod.functions.back().ref);
    generate(func.implementation, binder, mod);
}

template<>
void generate(BoundNodeReference ref, BoundModule const &impl, Binder &binder, Program &ir)
{
    ir.modules.emplace_back(ir, ir.modules.size(), ref, Function { ir }, impl.name);
    auto &mod = ir.modules.back();
    mod.initializer.name = "#init";
    mod.initializer.address_type = AddressType::Data;
    for (auto decl : impl.names) {
        map(decl, binder, mod);
    }
    for (auto name_ref : impl.names) {
        std::visit(overload {
                       [name_ref, &binder, &mod](BoundFunction const &impl) {
                           generate<BoundFunction, Module>(name_ref, impl, binder, mod);
                       },
                       [name_ref, &binder, &mod](auto const &impl) {
                           generate<std::decay_t<decltype(impl)>, Function>(name_ref, impl, binder, mod.initializer);
                       },
                   },
            binder[name_ref].impl);
    }
}

template<>
void generate(BoundNodeReference ref, BoundProgram const &impl, Binder &binder, Program &ir)
{
    for (auto mod_ref : impl.modules) {
        auto const &mod = I(BoundModule, mod_ref);
        generate<BoundModule, Program>(mod_ref, mod, binder, ir);
    }
}

Error<bool> Program::generate()
{
    auto const &program_impl = I(BoundProgram, binder.entrypoint);
    Arwen::IR::generate(binder.entrypoint, program_impl, binder, *this);
    return {};
}

void Program::list() const
{
    std::println("Program Listing");
    std::cout << std::string(78, '=') << "\n\n";
    for (auto const &mod : modules) {
        mod.list(binder);
    }
}

void Module::list(Binder &binder) const
{
    std::println("Module {}", name);
    std::cout << std::string(78, '-') << "\n\n";
    bool foreign { false };
    for (auto const &f : functions) {
        auto const &func = I(BoundFunction, f.bound_ref);
        if (binder.type_of(func.implementation) == BoundNodeType::BoundForeignFunction) {
            std::println("{} -> {}", f.name, I(BoundForeignFunction, func.implementation).foreign_name);
            foreign = true;
        }
        if (binder.type_of(func.implementation) == BoundNodeType::BoundIntrinsic) {
            std::println("{} -> @{}", f.name, I(BoundIntrinsic, func.implementation).name);
            foreign = true;
        }
    }
    if (foreign) {
        std::println("");
    }
    if (!initializer.ops.empty()) {
        initializer.list();
    }
    for (auto const &f : functions) {
        auto const &func = I(BoundFunction, f.bound_ref);
        if (binder.type_of(func.implementation) == BoundNodeType::BoundFunctionImplementation) {
            f.list();
        }
    }
}

void Function::list() const
{
    std::println("Function {}", name);
    std::cout << std::string(78, '.') << "\n\n";
    for (auto const &o : ops) {
        std::println("{}", o);
    }
    std::println("");
}

Address Function::get_variable_address(BoundNodeReference ref)
{
    if (variables.contains(ref)) {
        return Address { AddressType::Stack, variables[ref] };
    }
    if (program.variables.contains(ref)) {
        return Address { AddressType::Data, program.variables[ref] };
    }
    fatal("Variable reference {} not found", ref);
}

}
