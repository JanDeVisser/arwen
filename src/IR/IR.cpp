/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <print>
#include <string>
#include <type_traits>
#include <variant>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Binder/Binder.h>
#include <IR/IR.h>

#include <Lib.h>
#include <Result.h>

namespace Arwen::IR {

using namespace Arwen;

template <typename Container>
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

template<>
void generate(BoundNodeReference ref, BoundAssignmentExpression const &impl, Binder &, Function &ir)
{
    auto &binder = ir.program.binder;
    assert(std::holds_alternative<BoundIdentifier>(binder[impl.left].impl));
    auto &id = I(BoundIdentifier, impl.left);
    generate(impl.right, binder, ir);
    ir.ops.push_back({ ir.ops.size(), PopVariable { id.name } });
}

template<>
void generate(BoundNodeReference ref, BoundBinaryExpression const &impl, Binder &binder, Function &ir)
{
    generate(impl.right, binder, ir);
    if (impl.op == BinaryOperator::MemberAccess && binder.type_of(impl.left) == BoundNodeType::BoundIdentifier) {
        auto const &lhs = I(BoundIdentifier, impl.left);
        ir.ops.push_back({ ir.ops.size(), PushVariableRef { lhs.name } });
    } else {
        generate(impl.left, binder, ir);
    }
    ir.ops.push_back({ ir.ops.size(), BinaryOperation { impl.op } });
}

template<>
void generate(BoundNodeReference, BoolConstant const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushBoolean { impl.value } });
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
}

template<>
void generate(BoundNodeReference, FloatConstant const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushFloat { impl.value } });
}

template<>
void generate(BoundNodeReference, BoundForeignFunction const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), ForeignCall { impl.foreign_name, impl.declaration } });
}

template<>
void generate(BoundNodeReference, BoundFunctionCall const &impl, Binder &binder, Function &ir)
{
    for (auto it = impl.arguments.crbegin(); it != impl.arguments.crend(); ++it) {
        generate(*it, binder, ir);
    }
    assert(impl.function.has_value());
    generate(I(BoundFunction, *impl.function).implementation, binder, ir);
}

template<>
void generate(BoundNodeReference, BoundFunctionImplementation const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), Call { impl.name, impl.declaration } });
}

template<>
void generate(BoundNodeReference, BoundIdentifier const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushVariableValue { impl.name } });
}

template<>
void generate(BoundNodeReference, IntConstant const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushInt { impl.value } });
}

template<>
void generate(BoundNodeReference, BoundIntrinsic const &impl, Binder &binder, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), Intrinsic { impl.name } });
}

template<>
void generate(BoundNodeReference, BoundMember const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushString { impl.name } });
}

template<>
void generate(BoundNodeReference, StringConstant const &impl, Binder &, Function &ir)
{
    ir.ops.push_back({ ir.ops.size(), PushString { impl.value } });
}

template<>
void generate(BoundNodeReference, BoundForeignFunction const &, Binder &, Module &)
{
}

template<>
void generate(BoundNodeReference, BoundFunctionImplementation const &impl, Binder &binder, Module &mod)
{
    auto &ir_func = mod.functions.back();
    generate(impl.implementation, binder, ir_func);
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
    ir.modules.emplace_back(ir, ir.modules.size(), ref, impl.name);
    auto &mod = ir.modules.back();
    for (auto name_ref : impl.names) {
        std::visit(
            [name_ref, &binder, &mod](auto const &impl) {
                generate<std::decay_t<decltype(impl)>, Module>(name_ref, impl, binder, mod);
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
        auto const& func = I(BoundFunction, f.bound_ref);
        if (binder.type_of(func.implementation) == BoundNodeType::BoundForeignFunction) {
            std::println("{} -> {}", f.name, I(BoundForeignFunction, func.implementation).foreign_name);
            foreign = true;
        }
    }
    if (foreign) {
        std::println("");
    }
    for (auto const &f : functions) {
        auto const& func = I(BoundFunction, f.bound_ref);
        if (binder.type_of(func.implementation) != BoundNodeType::BoundForeignFunction) {
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
