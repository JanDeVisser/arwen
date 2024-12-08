/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <iostream>
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
            [&impl, &ir](BoundIdentifier const &id) {
                ir.ops.push_back({ ir.ops.size(), PopVariable { id.name } });
            },
            [&](BoundBinaryExpression const &binex) {
                if (binex.op != BinaryOperator::Subscript) {
                    UNREACHABLE();
                }
                generate(binex.right, ir.program.binder, ir);
                assert(binder.type_of(binex.left) == BoundNodeType::BoundIdentifier);
                auto const &lhs = I(BoundIdentifier, binex.left);
                ir.ops.push_back({ ir.ops.size(), PushConstant { lhs.name } });
                ir.ops.push_back({ ir.ops.size(), PopArrayElement {} });
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
        ir.ops.push_back({ ir.ops.size(), PushVariableRef { lhs.name } });
    } break;
    case BinaryOperator::Subscript: {
        assert(binder.type_of(impl.right) == BoundNodeType::BoundSubscript);
        assert(binder.type_of(impl.left) == BoundNodeType::BoundIdentifier);
        auto const &lhs = I(BoundIdentifier, impl.left);
        ir.ops.push_back({ ir.ops.size(), PushConstant { lhs.name } });
        ir.ops.push_back({ ir.ops.size(), PushArrayElement {} });
        return;
    } break;
    default:
        generate(impl.left, binder, ir);
        break;
    }
    ir.ops.push_back({ ir.ops.size(), BinaryOperation { impl.op } });
}

template<>
void generate(BoundNodeReference, BoundConstant const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushConstant { impl.value } });
}

template<>
void generate(BoundNodeReference ref, BoundBlock const &impl, Binder &binder, Function &ir)
{
    if (impl.label) {
        ir.ops.push_back({ ir.ops.size(), Label { *impl.label } });
    }
    for (auto stmt : impl.statements) {
        generate(stmt, binder, ir);
    }
    ir.scopes[ref] = ir.ops.size();
}

template<>
void generate(BoundNodeReference ref, BoundBreak const &impl, Binder &binder, Function &ir)
{
    if (impl.expression) {
        generate(*impl.expression, binder, ir);
    }
    ir.ops.push_back({ ir.ops.size(), Break { 0, impl.block_is_loop } });
    ir.ops.back().target = impl.block;
}

template<>
void link(Ref ref, Break &impl, Function &function)
{
    auto &op = function.ops[ref];
    if (op.target) {
        assert(function.scopes.contains(*op.target));
        impl.target = function.scopes[*op.target];
        if (impl.block_is_loop) {
            impl.target += 1; // Jump over jump back to loop start
        }
        op.target = {};
    }
}

template<>
void generate(BoundNodeReference, BoundConstantDeclaration const &impl, Binder &, Function &ir)
{
    generate(impl.initializer, ir.program.binder, ir);
    ir.ops.push_back({ ir.ops.size(), PopVariable { impl.name } });
}

template<>
void generate(BoundNodeReference, BoundCoercion const &impl, Binder &, Function &ir)
{
    generate(impl.expression, ir.program.binder, ir);
}

template<>
void generate(BoundNodeReference ref, BoundContinue const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), Jump {} });
    ir.ops.back().target = impl.block;
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
void generate(BoundNodeReference ref, BoundForeignFunction const &impl, Binder &binder, Function &ir)
{
    auto func = binder[ref].parent;
    ir.ops.push_back({ ir.ops.size(), ForeignCall { impl.foreign_name, func } });
}

template<>
void generate(BoundNodeReference, BoundFunctionCall const &impl, Binder &binder, Function &ir)
{
    for (auto it = impl.arguments.crbegin(); it != impl.arguments.crend(); ++it) {
        generate(*it, binder, ir);
    }
    assert(impl.function.has_value());
    generate(I(BoundFunction, *impl.function).implementation, binder, ir);
    if (impl.discard_result && *binder[*impl.function].type != static_cast<TypeReference>(PseudoType::Void)) {
        ir.ops.push_back({ ir.ops.size(), Discard {} });
    }
}

template<>
void generate(BoundNodeReference ref, BoundFunctionImplementation const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), Call { I(BoundFunction, binder[ref].parent).name, binder[ref].parent } });
}

template<>
void generate(BoundNodeReference, BoundIdentifier const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushVariableValue { impl.name } });
}

template<>
void generate(BoundNodeReference, BoundIf const &impl, Binder &, Function &ir)
{
    generate(impl.condition, ir.program.binder, ir);
    ir.ops.push_back({ ir.ops.size(), JumpF { 0 } });
    auto jump_to_else_ix = ir.ops.size() - 1;
    generate(impl.true_branch, ir.program.binder, ir);
    std::get<JumpF>(ir.ops[jump_to_else_ix].op).target = ir.ops.size();
    if (impl.false_branch) {
        ir.ops.push_back({ ir.ops.size(), Jump { 0 } });
        auto jump_to_end_ix = ir.ops.size() - 1;
        std::get<JumpF>(ir.ops[jump_to_else_ix].op).target = ir.ops.size();
        generate(*impl.false_branch, ir.program.binder, ir);
        std::get<Jump>(ir.ops[jump_to_end_ix].op).target = ir.ops.size();
    }
}

template<>
void generate(BoundNodeReference, BoundIntrinsic const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), Intrinsic { impl.name } });
}

template<>
void generate(BoundNodeReference, BoundMember const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushConstant { impl.name } });
}

template<>
void generate(BoundNodeReference, Nullptr const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushNullptr {} });
}

template<>
void generate(BoundNodeReference, BoundReturn const &impl, Binder &, Function &ir)
{
    if (impl.expression) {
        generate(*impl.expression, ir.program.binder, ir);
    }
    ir.ops.push_back({ ir.ops.size(), FunctionReturn { impl.expression.has_value() } });
}

template<>
void generate(BoundNodeReference, BoundSubscript const &impl, Binder &, Function &ir)
{
    for (auto it = impl.subscripts.crbegin(); it != impl.subscripts.crend(); ++it) {
        generate(*it, ir.program.binder, ir);
    }
    ir.ops.push_back({ ir.ops.size(), PushConstant { static_cast<int64_t>(impl.subscripts.size()) } });
}

template<>
void generate(BoundNodeReference ref, BoundUnaryExpression const &impl, Binder &binder, Function &ir)
{
    generate(impl.operand, binder, ir);
    ir.ops.push_back({ ir.ops.size(), UnaryOperation { impl.op } });
}

template<>
void generate(BoundNodeReference ref, BoundVariableDeclaration const &impl, Binder &binder, Function &ir)
{
    auto const &type = binder.registry[*binder[ref].type];
    if (type.typespec.tag() == TypeKind::Array) {
        auto &array_def = I(BoundArrayType, *impl.type);
        generate(array_def.size, ir.program.binder, ir);
        ir.ops.push_back({ ir.ops.size(), MakeArray { type.typespec.get<TypeKind::Array>().element_type } });
    } else {
        if (impl.initializer) {
            generate(*impl.initializer, ir.program.binder, ir);
        } else {
            ir.ops.push_back({ ir.ops.size(), PushNullptr {} });
        }
    }
    ir.ops.push_back({ ir.ops.size(), PopVariable { impl.name } });
}

template<>
void generate(BoundNodeReference, BoundWhile const &impl, Binder &, Function &ir)
{
    auto start = ir.ops.size();
    generate(impl.condition, ir.program.binder, ir);
    ir.ops.push_back({ ir.ops.size(), JumpF { 0 } });
    auto jump_to_end_ix = ir.ops.size() - 1;
    generate(impl.body, ir.program.binder, ir);
    ir.ops.push_back({ ir.ops.size(), Jump { start } });
    std::get<JumpF>(ir.ops[jump_to_end_ix].op).target = ir.ops.size();
}

template<>
void generate(BoundNodeReference, BoundForeignFunction const &, Binder &, Module &)
{
}

template<>
void generate(BoundNodeReference, BoundFunctionImplementation const &impl, Binder &binder, Module &mod)
{
    auto &function = mod.functions.back();
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
    mod.functions.emplace_back(mod.program, mod.functions.size(), ref, func.name);
    mod.function_refs.emplace(mod.functions.back().name, mod.functions.back().ref);
    generate(func.implementation, binder, mod);
}

template<>
void generate(BoundNodeReference ref, BoundModule const &impl, Binder &binder, Program &ir)
{
    ir.modules.emplace_back(ir, ir.modules.size(), ref, Function { ir }, impl.name);
    auto &mod = ir.modules.back();
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

}
