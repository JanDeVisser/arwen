/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <ranges>
#include <string>
#include <variant>
#include <vector>

#include <Util/Align.h>
#include <Util/Defer.h>
#include <Util/Logging.h>

#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/IR/IR.h>

namespace Arwen::IR {

uint64_t next_label()
{
    static uint64_t label { 0 };
    return label++;
}

void add_operation(Generator &generator, Operation op)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (ctx.ir_node) {
            auto &ops = ctx.ir_node->operations;
            if (!ops.empty()) {
                auto &b { ops.back() };
                if (op.type() == Operation::Type::Discard && (b.type() == Operation::Type::PushConstant || b.type() == Operation::Type::PushValue)) {
                    ops.pop_back();
                    return;
                }
            }
            // if (b.type() == Operation::Type::Return && op.type() != Operation::Type::ScopeEnd && op.type() != Operation::Type::Label) {
            //     return true;
            // }
            ops.emplace_back(op);
            return;
        }
    }
    UNREACHABLE();
}

template<typename Op, typename Payload>
void add_operation(Generator &generator, Payload const &payload)
{
    add_operation(generator, std::move(Operation { Op { payload } }));
}

template<typename Op>
void add_operation(Generator &generator)
{
    add_operation(generator, std::move(Operation { Op {} }));
}

Operation &last_op(Generator &generator)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (ctx.ir_node) {
            auto ir_node = ctx.ir_node;
            return ir_node->operations.back();
        }
    }
    UNREACHABLE();
}

template<typename T>
pIR const &find_ir_node(Generator const &generator)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (ctx.ir_node && is<T>(ctx.ir_node)) {
            return ctx.ir_node;
        }
    }
    UNREACHABLE();
}

void assign(std::wstring varname, pType const &lhs, ASTNode const &rhs, Generator &generator)
{
    generator.generate(rhs);
    auto &rhs_type { rhs->bound_type };
    add_operation<Operation::PushVarAddress>(generator, VarPath { std::move(varname), lhs, 0 });
    if (rhs_type->kind() == TypeKind::ReferenceType) {
        add_operation<Operation::AssignFromRef>(generator, lhs);
    } else {
        add_operation<Operation::AssignValue>(generator, lhs);
    }
}

template<typename T>
void generate_node(Generator &, ASTNode const &, T const &)
{
    trace("generate_node({})", typeid(T).name());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, BinaryExpression const &node)
{
    trace("Operator `{}`", Operator_name(node.op));

    auto const &lhs { node.lhs };
    auto const &lhs_type { lhs->bound_type };
    auto        lhs_value_type { lhs_type->value_type() };
    auto const &rhs { node.rhs };

    if (node.op == Operator::MemberAccess) {
        generator.generate(node.lhs);
        auto const &ref = get<ReferenceType>(lhs->bound_type);
        auto        rhs_id = get<Identifier>(rhs);
        auto const &s = get<StructType>(ref.referencing);
        size_t      offset { 0 };
        for (auto const &f : s.fields) {
            offset = alignat(offset, f.type->align_of());
            if (f.name == rhs_id.identifier) {
                break;
            }
            offset += f.type->size_of();
        }
        Operation &operation = last_op(generator);
        auto      &push_var_address = std::get<Operation::PushVarAddress>(operation.op);
        push_var_address.payload.type = n->bound_type;
        push_var_address.payload.offset += offset;
        return;
    }

    auto const &rhs_type { rhs->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    if (node.op == Operator::Assign) {
        auto ident = get<Identifier>(node.lhs); // crashes if LHS is not identifier
        assign(ident.identifier, node.lhs->bound_type, node.rhs, generator);
        return;
    }
    generator.generate(node.lhs);
    if (lhs_type != lhs_value_type) {
        add_operation<Operation::Dereference>(generator, lhs_value_type);
    }
    generator.generate(node.rhs);
    if (rhs_type != rhs_value_type) {
        add_operation<Operation::Dereference>(generator, rhs_value_type);
    }
    add_operation<Operation::BinaryOperator>(generator, Operation::BinaryOperator { lhs_value_type, node.op, rhs_value_type });
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Block const &node)
{
    if (generator.ctxs.empty()) {
        auto module = make_node<Module>(generator.ir, L"anonymous", n);
        generator.ctxs.push_back(Context { module, {} });
        generator.ir.entry_point = module;
    }
    Declarations variables;
    assert(n->ns);
    for (auto const &var : n->ns->variables) {
        variables.emplace_back(var.first, var.second->bound_type);
    }
    add_operation<Operation::ScopeBegin>(generator, variables);
    auto const scope_end = next_label();
    auto const end_block = next_label();
    generator.ctxs.emplace_back(Context { {}, Context::BlockDescriptor { scope_end } });
    bool has_defered { false };
    {
        Defer _ { [&generator]() { generator.ctxs.pop_back(); } };

        pType discard { nullptr };
        auto  empty { true };
        for (auto const &stmt : node.statements) {
            if (discard != nullptr) {
                add_operation<Operation::Discard>(generator, discard);
            }
            discard = (stmt->bound_type && stmt->bound_type->size_of() > 0) ? stmt->bound_type : nullptr;
            empty &= (discard == nullptr);
            generator.generate(stmt);
        }
        auto &last = last_op(generator);

        // I can't remember what I did here. But a Break is basically a jump
        // so the Pop will never be executed?
        // if (last.type() == Operation::Type::Break) {
        //     add_operation<Operation::Pop>(generator, n->bound_type);
        // }

        if (empty) {
            add_operation<Operation::PushConstant>(generator, make_void());
        }
        add_operation<Operation::Label>(generator, scope_end);
        auto const &bd = std::get<Context::BlockDescriptor>(generator.ctxs.back().unwind);
        for (auto const &[defer, label] : bd.defer_stmts) {
            has_defered = true;
            add_operation<Operation::Label>(generator, label);
            generator.generate(defer);
            add_operation<Operation::Discard>(generator, defer->bound_type);
        }
    }

    uint64_t enclosing_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&enclosing_end](Context::BlockDescriptor const &bd) -> bool {
                        if (!bd.defer_stmts.empty()) {
                            enclosing_end = bd.defer_stmts.back().second;
                            return true;
                        }
                        return false;
                    },
                    [&enclosing_end](Context::FunctionDescriptor const &fd) -> bool {
                        enclosing_end = fd.end_label;
                        return true;
                    },
                    [](auto const &) {
                        return false;
                    } },
                ctx.unwind)) {
            break;
        }
    }
    add_operation<Operation::ScopeEnd>(generator, Operation::ScopeEnd { enclosing_end, has_defered, n->bound_type });
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Break const &node)
{
    uint64_t depth { 0 };
    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&node, &generator, &depth, &scope_end](Context::LoopDescriptor const &ld) -> bool {
                        if (node.label->empty() || ld.name == node.label.value()) {
                            add_operation<Operation::Break>(generator, IR::Operation::BreakOp { scope_end, depth, ld.loop_end });
                            return true;
                        }
                        return false;
                    },
                    [&generator, &depth, &scope_end](Context::BlockDescriptor const &bd) -> bool {
                        if (!bd.defer_stmts.empty()) {
                            if (scope_end == 0) {
                                scope_end = bd.defer_stmts.back().second;
                            }
                            ++depth;
                        }
                        return false;
                    },
                    [](Context::FunctionDescriptor const &) -> bool {
                        UNREACHABLE();
                    },
                    [](auto const &) {
                        return false;
                    } },
                ctx.unwind)) {
            return;
        }
    }
    UNREACHABLE();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Call const &node)
{
    std::vector<IRVariableDeclaration> params;
    auto const                        &definition = get<FunctionDefinition>(node.function);
    auto const                        &declaration = get<FunctionDeclaration>(definition.declaration);
    for (auto const &param_def : declaration.parameters) {
        params.emplace_back(get<Parameter>(param_def).name, param_def->bound_type);
    }
    for (auto const &expression : get<ExpressionList>(node.arguments).expressions) {
        generator.generate(expression);
        if (auto value_type = expression->bound_type->value_type(); expression->bound_type != value_type) {
            add_operation<Operation::Dereference>(generator, value_type);
        }
    }
    if (is<ExternLink>(get<FunctionDefinition>(node.function).implementation)) {
        auto const &extern_link = get<ExternLink>(get<FunctionDefinition>(node.function).implementation);
        add_operation<Operation::NativeCall>(generator, Operation::CallOp { extern_link.link_name, params, n->bound_type });
        return;
    }
    add_operation<Operation::Call>(generator, Operation::CallOp { get<FunctionDefinition>(node.function).name, params, n->bound_type });
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Comptime const &node)
{
    assert(node.statements != nullptr);
    generator.generate(node.statements);
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Constant const &node)
{
    assert(node.bound_value.has_value());
    add_operation<Operation::PushConstant>(generator, node.bound_value.value());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Continue const &node)
{
    uint64_t depth { 0 };
    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&node, &generator, &depth, &scope_end](Context::LoopDescriptor const &ld) -> bool {
                        if (node.label->empty() || ld.name == node.label.value()) {
                            add_operation<Operation::Break>(generator, IR::Operation::BreakOp { scope_end, depth, ld.loop_begin });
                            return true;
                        }
                        return false;
                    },
                    [&generator, &depth, &scope_end](Context::BlockDescriptor const &bd) -> bool {
                        if (!bd.defer_stmts.empty()) {
                            if (scope_end == 0) {
                                scope_end = bd.defer_stmts.back().second;
                            }
                            ++depth;
                        }
                        return false;
                    },
                    [](Context::FunctionDescriptor const &) -> bool {
                        UNREACHABLE();
                    },
                    [](auto const &) {
                        return false;
                    } },
                ctx.unwind)) {
            return;
        }
    }
    UNREACHABLE();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, DeferStatement const &node)
{
    auto const label = next_label();
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (std::visit(
                overloads {
                    [&node, &label](Context::BlockDescriptor &bd) -> bool {
                        bd.defer_stmts.emplace_back(node.statement, label);
                        return true;
                    },
                    [](auto &) -> bool {
                        return false;
                    } },
                ctx.unwind)) {
            add_operation<Operation::PushConstant>(generator, make_void());
            return;
        }
    }
    UNREACHABLE();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Enum const &node)
{
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, FunctionDefinition const &node)
{
    if (!is<ExternLink>(node.implementation)) {
        auto const &module = find_ir_node<Module>(generator);
        auto        function = make_node<Function>(generator.ir, node.name, n);
        auto       &f = get<Function>(function);
        f.module = module;
        auto const &decl = get<FunctionDeclaration>(node.declaration);
        f.return_type = decl.return_type->bound_type;
        get<Module>(module).functions.emplace(node.name, function);
        for (auto const &param : decl.parameters) {
            f.parameters.emplace_back(get<Parameter>(param).name, param->bound_type);
        }
        auto ret_label { next_label() };
        generator.ctxs.emplace_back(function, Context::FunctionDescriptor { ret_label, f.return_type });
        generator.generate(node.implementation);
        add_operation<Operation::Label>(generator, ret_label);
        generator.ctxs.pop_back();
    }
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Identifier const &node)
{
    add_operation<Operation::PushVarAddress>(generator, VarPath { node.identifier, n->bound_type, 0 });
    add_operation<Operation::Dereference>(generator, n->bound_type);
}

template<>
void generate_node(Generator &generator, ASTNode const &n, IfStatement const &node)
{
    generator.generate(node.condition);
    if (auto value_type = node.condition->bound_type->value_type(); node.condition->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    auto const else_label = next_label();
    auto const done_label = next_label();
    add_operation<Operation::JumpF>(generator, else_label);
    generator.generate(node.if_branch);
    add_operation<Operation::Jump>(generator, done_label);
    add_operation<Operation::Label>(generator, else_label);
    if (node.else_branch) {
        generator.generate(node.else_branch);
    } else {
        add_operation<Operation::PushConstant>(generator, make_void());
    }
    add_operation<Operation::Label>(generator, done_label);
}

template<>
void generate_node(Generator &generator, ASTNode const &n, LoopStatement const &node)
{
    add_operation<Operation::PushConstant>(generator, make_value(node.statement->bound_type));
    Context::LoopDescriptor const ld { node.label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    add_operation<Operation::Discard>(generator, node.statement->bound_type);
    generator.generate(node.statement);
    add_operation<Operation::Jump>(generator, ld.loop_begin);
    add_operation<Operation::Label>(generator, ld.loop_end);
    generator.ctxs.pop_back();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Arwen::Module const &node)
{
    auto module = make_node<IR::Module>(generator.ir, node.name, n);
    if (generator.ir.entry_point) {
        get<Module>(module).program = generator.ir.entry_point;
        get<Program>(generator.ir.entry_point).modules.emplace(node.name, module);
    } else {
        generator.ir.entry_point = module;
    }
    for (auto const &[name, var_node] : n->ns->variables) {
        module->variables.emplace_back(name, var_node->bound_type);
    }
    generator.ctxs.push_back(Context { module, {} });

    pType discard { nullptr };
    auto  empty { true };
    for (auto const &stmt : node.statements) {
        if (discard != nullptr) {
            add_operation<Operation::Discard>(generator, discard);
        }
        discard = stmt->bound_type;
        empty &= (discard == nullptr);
        generator.generate(stmt);
    }
    if (empty) {
        add_operation<Operation::PushConstant>(generator, make_void());
    }

    generator.ctxs.pop_back();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Arwen::Program const &node)
{
    assert(generator.ctxs.empty());
    generator.ir.entry_point = make_node<Program>(generator.ir, node.name, n);
    generator.ctxs.push_back(Context { generator.ir.entry_point, {} });
    for (auto const &[name, var_node] : n->ns->variables) {
        generator.ir.entry_point->variables.emplace_back(name, var_node->bound_type);
    }
    pType discard { nullptr };
    auto  empty { true };
    for (auto const &stmt : node.statements) {
        if (discard != nullptr) {
            add_operation<Operation::Discard>(generator, discard);
        }
        discard = stmt->bound_type;
        empty &= (discard == nullptr);
        generator.generate(stmt);
    }
    if (empty) {
        add_operation<Operation::PushConstant>(generator, make_void());
    }
    for (auto &mod : node.modules | std::views::values) {
        generator.generate(mod);
    }
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Return const &node)
{
    generator.generate(node.expression);
    if (auto value_type = node.expression->bound_type->value_type(); node.expression->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::Pop>(generator, node.expression->bound_type->value_type());

    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&generator, &scope_end](Context::BlockDescriptor const &bd) -> bool {
                        if (!bd.defer_stmts.empty()) {
                            if (scope_end == 0) {
                                scope_end = bd.defer_stmts.back().second;
                            }
                        } else {
                            scope_end = bd.scope_end_label;
                        }
                        return false;
                    },
                    [&generator, &scope_end](Context::FunctionDescriptor const &fd) -> bool {
                        add_operation<Operation::Break>(generator, IR::Operation::BreakOp { scope_end, {}, 0 });
                        return true;
                    },
                    [](auto const &) {
                        return false;
                    } },
                ctx.unwind)) {
            return;
        }
    }
    UNREACHABLE();
}

template<>
void generate_node(Generator &generator, ASTNode const &n, PublicDeclaration const &node)
{
    generator.generate(node.declaration);
}

template<>
void generate_node(Generator &generator, ASTNode const &n, Struct const &node)
{
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, UnaryExpression const &node)
{
    if (node.op == Operator::Sizeof && is<TypeSpecification>(node.operand)) {
        add_operation<Operation::PushConstant>(generator, Value { TypeRegistry::i64, node.operand->bound_type->size_of() });
        return;
    }
    generator.generate(node.operand);
    if (auto value_type = node.operand->bound_type->value_type(); node.operand->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::UnaryOperator>(generator, Operation::UnaryOperator { node.operand->bound_type, node.op });
}

template<>
void generate_node(Generator &, ASTNode const &, TypeSpecification const &)
{
}

template<>
void generate_node(Generator &generator, ASTNode const &n, VariableDeclaration const &node)
{
    add_operation<Operation::DeclVar>(generator, IRVariableDeclaration { node.name, n->bound_type });
    if (node.initializer) {
        assign(node.name, n->bound_type, node.initializer, generator);
    }
    add_operation<Operation::PushVarAddress>(generator, VarPath { node.name, n->bound_type, 0 });
    add_operation<Operation::Dereference>(generator, n->bound_type->value_type());
}

template<>
void generate_node(Generator &generator, ASTNode const &n, WhileStatement const &node)
{
    add_operation<Operation::PushConstant>(generator, make_value(node.statement->bound_type->value_type()));
    Context::LoopDescriptor const ld { node.label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    generator.generate(node.condition);
    if (auto value_type = node.condition->bound_type->value_type(); node.condition->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::JumpF>(generator, ld.loop_end);
    add_operation<Operation::Discard>(generator, node.statement->bound_type);
    generator.generate(node.statement);
    add_operation<Operation::Jump>(generator, ld.loop_begin);
    add_operation<Operation::Label>(generator, ld.loop_end);
    generator.ctxs.pop_back();
}

void Generator::generate(ASTNode const &n)
{
    std::visit(
        [&n, this](auto const &node) {
            generate_node(*this, n, node);
        },
        n->node);
}

IRNodes &generate_ir(ASTNode const &node, IRNodes &ir)
{
    Generator generator { ir };
    assert(is<Arwen::Program>(node) || is<Arwen::Module>(node) || is<Arwen::Call>(node) || is<Arwen::Block>(node));
    generator.generate(node);
    return generator.ir;
}
}
