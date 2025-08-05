/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <limits>
#include <memory>
#include <ranges>
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

void list(Operation const &op)
{
    if (is<Operation::Label>(op)) {
        std::wcout << get<Operation::Label>(op).payload << ":\n";
        return;
    }
    std::wcout << "    " << op << '\n';
}

void list(pFunction const &func)
{
    std::wcout << "== [F] = " << func->name << " ===================\n";
    for (auto const &op : func->operations) {
        list(op);
    }
}

void list(pModule const &mod)
{
    std::wcout << "== [M] " << mod->name << " =====================\n\n";
    for (auto const &op : mod->operations) {
        list(op);
    }
    std::wcout << '\n';
    for (auto const &func : mod->functions | std::views::values) {
        list(func);
    }
}

void list(pProgram const &prog)
{
    std::wcout << "== [P] " << prog->name << " ====================\n\n";
    for (auto const &op : prog->operations) {
        list(op);
    }
    std::wcout << '\n';
    for (auto const &mod : prog->modules | std::views::values) {
        list(mod);
    }
}

void list(IRNode const &ir)
{
    std::visit(overloads {
                   [](std::monostate const &) {
                   },
                   [](auto const &ir_node) {
                       list(ir_node);
                   } },
        ir);
}

void add_operation(Generator &generator, Operation op)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (std::visit(
                overloads {
                    [](std::monostate const &) -> bool {
                        return false;
                    },
                    [&op](auto const &o) -> bool {
                        auto &ops = o->operations;
                        if (!ops.empty()) {
                            auto &b { ops.back() };
                            if (op.type() == Operation::Type::Discard && (b.type() == Operation::Type::PushConstant || b.type() == Operation::Type::PushValue)) {
                                ops.pop_back();
                                return true;
                            }
                            // if (b.type() == Operation::Type::Return && op.type() != Operation::Type::ScopeEnd && op.type() != Operation::Type::Label) {
                            //     return true;
                            // }
                        }
                        ops.emplace_back(op);
                        return true;
                    } },
                ctx.ir_node)) {
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
        auto *operation = std::visit(
            overloads {
                [](pFunction const &f) -> Operation * {
                    return &f->operations.back();
                },
                [](pModule const &mod) -> Operation * {
                    return &mod->operations.back();
                },
                [](pProgram const &prog) -> Operation * {
                    return &prog->operations.back();
                },
                [](auto const &) -> Operation * {
                    return nullptr;
                } },
            ctx.ir_node);
        if (operation != nullptr) {
            return *operation;
        }
    }
    UNREACHABLE();
}

template<typename T>
T const &find_ir_node(Generator const &generator)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (std::holds_alternative<T>(ctx.ir_node)) {
            return std::get<T>(ctx.ir_node);
        }
    }
    UNREACHABLE();
}

template<typename T>
    requires std::derived_from<T, SyntaxNode>
void generate_node(Generator &, std::shared_ptr<T> const &)
{
    trace("generate_node({})", typeid(T).name());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<BinaryExpression> const &node)
{
    trace("Operator `{}`", Operator_name(node->op));

    auto const &lhs { node->lhs };
    auto const &lhs_type { lhs->bound_type };
    auto        lhs_value_type { lhs_type->value_type() };
    auto const &rhs { node->rhs };

    if (node->op == Operator::MemberAccess) {
        generator.generate(node->lhs);
        auto const &ref = std::get<ReferenceType>(lhs->bound_type->description);
        auto        rhs_id = std::dynamic_pointer_cast<Identifier>(rhs);
        auto const &s = std::get<StructType>(ref.referencing->description);
        size_t      offset { 0 };
        for (auto const &f : s.fields) {
            offset = alignat(offset, f.type->align_of());
            if (f.name == rhs_id->identifier) {
                break;
            }
            offset += f.type->size_of();
        }
        Operation &operation = last_op(generator);
        auto      &push_var_address = std::get<Operation::PushVarAddress>(operation.op);
        push_var_address.payload.type = node->bound_type;
        push_var_address.payload.offset += offset;
        return;
    }

    auto const &rhs_type { rhs->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    if (node->op == Operator::Assign) {
        generator.generate(node->rhs);
        generator.generate(node->lhs);
        if (rhs_type->kind() == TypeKind::ReferenceType) {
            add_operation<Operation::AssignFromRef>(generator, lhs_value_type);
        } else {
            add_operation<Operation::AssignValue>(generator, lhs_value_type);
        }
        generator.generate(node->lhs);
        add_operation<Operation::Dereference>(generator, lhs_value_type);
        return;
    }
    generator.generate(node->lhs);
    if (lhs_type != lhs_value_type) {
        add_operation<Operation::Dereference>(generator, lhs_value_type);
    }
    generator.generate(node->rhs);
    if (rhs_type != rhs_value_type) {
        add_operation<Operation::Dereference>(generator, rhs_value_type);
    }
    add_operation<Operation::BinaryOperator>(generator, Operation::BinaryOperator { lhs_value_type, node->op, rhs_value_type });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Block> const &node)
{
    if (generator.ctxs.empty()) {
        auto module = std::make_shared<Module>(L"anonymous", node);
        generator.ctxs.push_back(Context { module, {} });
    }
    std::vector<IRVariableDeclaration> variables;
    for (auto const &var : node->ns->variables) {
        variables.emplace_back(var.first, var.second->bound_type);
    }
    add_operation<Operation::ScopeBegin>(generator, variables);
    auto const scope_end = next_label();
    auto const end_block = next_label();
    add_operation<Operation::PushConstant>(generator, make_void());
    generator.ctxs.emplace_back(Context { {}, Context::BlockDescriptor { scope_end } });
    bool has_defered { false };
    {
        Defer _ { [&generator]() { generator.ctxs.pop_back(); } };

        pType discard { TypeRegistry::void_ };
        auto  empty { true };
        for (auto const &stmt : node->statements) {
            if (discard != nullptr) {
                add_operation<Operation::Discard>(generator, discard);
            }
            discard = stmt->bound_type;
            empty &= (discard == nullptr);
            generator.generate(stmt);
        }
        auto &last = last_op(generator);
        if (last.type() == Operation::Type::Break) {
            add_operation<Operation::Pop>(generator, node->bound_type);
        }

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
    add_operation<Operation::ScopeEnd>(generator, Operation::ScopeEnd { enclosing_end, has_defered, node->bound_type });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Break> const &node)
{
    uint64_t depth { 0 };
    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&node, &generator, &depth, &scope_end](Context::LoopDescriptor const &ld) -> bool {
                        if (node->label->empty() || ld.name == node->label.value()) {
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
void generate_node(Generator &generator, std::shared_ptr<Call> const &node)
{
    std::vector<IRVariableDeclaration> params;
    for (auto const &param_def : node->function->declaration->parameters) {
        params.emplace_back(param_def->name, param_def->bound_type);
    }
    for (auto const &expression : node->arguments->expressions) {
        generator.generate(expression);
        if (auto value_type = expression->bound_type->value_type(); expression->bound_type != value_type) {
            add_operation<Operation::Dereference>(generator, value_type);
        }
    }
    if (auto const &extern_link = std::dynamic_pointer_cast<ExternLink>(node->function->implementation); extern_link != nullptr) {
        add_operation<Operation::NativeCall>(generator, Operation::CallOp { extern_link->link_name, params, node->bound_type });
        return;
    }
    add_operation<Operation::Call>(generator, Operation::CallOp { node->function->name, params, node->bound_type });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Constant> const &node)
{
    assert(node->bound_value.has_value());
    add_operation<Operation::PushConstant>(generator, node->bound_value.value());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Continue> const &node)
{
    uint64_t depth { 0 };
    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&node, &generator, &depth, &scope_end](Context::LoopDescriptor const &ld) -> bool {
                        if (node->label->empty() || ld.name == node->label.value()) {
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
void generate_node(Generator &generator, std::shared_ptr<DeferStatement> const &node)
{
    auto const label = next_label();
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (std::visit(
                overloads {
                    [&node, &label](Context::BlockDescriptor &bd) -> bool {
                        bd.defer_stmts.emplace_back(node->stmt, label);
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
void generate_node(Generator &generator, std::shared_ptr<Enum> const &node)
{
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<FunctionDefinition> const &node)
{
    if (auto const &extern_link = std::dynamic_pointer_cast<ExternLink>(node->implementation); extern_link == nullptr) {
        auto const &module = find_ir_node<pModule>(generator);
        auto        function = std::make_shared<Function>(node->name, node, module);
        function->return_type = node->declaration->return_type->bound_type;
        module->functions.emplace(node->name, function);
        for (auto const &param : node->declaration->parameters) {
            function->parameters.emplace_back(param->name, param->bound_type);
        }
        auto ret_label { next_label() };
        generator.ctxs.emplace_back(function, Context::FunctionDescriptor { ret_label, function->return_type });
        generator.generate(node->implementation);
        add_operation<Operation::Label>(generator, ret_label);
        generator.ctxs.pop_back();
    }
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Identifier> const &node)
{
    add_operation<Operation::PushVarAddress>(generator, VarPath { node->identifier, node->bound_type, 0 });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<IfStatement> const &node)
{
    generator.generate(node->condition);
    if (auto value_type = node->condition->bound_type->value_type(); node->condition->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    auto const else_label = next_label();
    auto const done_label = next_label();
    add_operation<Operation::JumpF>(generator, else_label);
    generator.generate(node->if_branch);
    add_operation<Operation::Jump>(generator, done_label);
    add_operation<Operation::Label>(generator, else_label);
    if (node->else_branch) {
        generator.generate(node->else_branch);
    } else {
        add_operation<Operation::PushConstant>(generator, make_void());
    }
    add_operation<Operation::Label>(generator, done_label);
}

template<>
void generate_node(Generator &generator, std::shared_ptr<LoopStatement> const &node)
{
    add_operation<Operation::PushConstant>(generator, make_value(node->statement->bound_type));
    Context::LoopDescriptor const ld { node->label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    add_operation<Operation::Discard>(generator, node->statement->bound_type);
    generator.generate(node->statement);
    add_operation<Operation::Jump>(generator, ld.loop_begin);
    add_operation<Operation::Label>(generator, ld.loop_end);
    generator.ctxs.pop_back();
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Arwen::Module> const &node)
{
    pProgram program { nullptr };
    if (!generator.ctxs.empty()) {
        program = std::get<pProgram>(generator.ctxs.front().ir_node);
    }
    auto module = std::make_shared<Module>(node->name, node, program);
    for (auto const &[name, var_node] : node->ns->variables) {
        module->variables.emplace_back(name, var_node->bound_type);
    }
    program->modules.emplace(node->name, module);
    generator.ctxs.push_back(Context { module, {} });

    pType discard { nullptr };
    auto  empty { true };
    for (auto const &stmt : node->statements) {
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
void generate_node(Generator &generator, std::shared_ptr<Arwen::Program> const &node)
{
    assert(generator.ctxs.empty());
    auto program = std::make_shared<Program>(node->name, node);
    generator.ctxs.push_back(Context { program, {} });
    for (auto const &[name, var_node] : node->ns->variables) {
        program->variables.emplace_back(name, var_node->bound_type);
    }
    pType discard { nullptr };
    auto  empty { true };
    for (auto const &stmt : node->statements) {
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
    for (auto &mod : node->modules | std::views::values) {
        generator.generate(mod);
    }
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Return> const &node)
{
    generator.generate(node->expression);
    if (auto value_type = node->expression->bound_type->value_type(); node->expression->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::Pop>(generator, node->expression->bound_type->value_type());

    uint64_t depth { 0 };
    uint64_t scope_end { 0 };
    for (auto const &ctx : generator.ctxs | std::views::reverse) {
        if (std::visit(
                overloads {
                    [&generator, &depth, &scope_end](Context::BlockDescriptor const &bd) -> bool {
                        if (!bd.defer_stmts.empty()) {
                            if (scope_end == 0) {
                                scope_end = bd.defer_stmts.back().second;
                            }
                            ++depth;
                        }
                        return false;
                    },
                    [&generator, &scope_end](Context::FunctionDescriptor const &fd) -> bool {
                        add_operation<Operation::Break>(generator, IR::Operation::BreakOp { scope_end, std::numeric_limits<uint64_t>::max(), 0 });
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
void generate_node(Generator &generator, std::shared_ptr<Struct> const &node)
{
    add_operation<Operation::PushConstant>(generator, make_void());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<UnaryExpression> const &node)
{
    if (node->op == Operator::Sizeof && node->operand->type == SyntaxNodeType::TypeSpecification) {
        add_operation<Operation::PushConstant>(generator, Value { TypeRegistry::i64, node->operand->bound_type->size_of() });
        return;
    }
    generator.generate(node->operand);
    if (auto value_type = node->operand->bound_type->value_type(); node->operand->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::UnaryOperator>(generator, Operation::UnaryOperator { node->operand->bound_type, node->op });
}

template<>
void generate_node(Generator &, std::shared_ptr<TypeSpecification> const &)
{
}

template<>
void generate_node(Generator &generator, std::shared_ptr<VariableDeclaration> const &node)
{
    add_operation<Operation::DeclVar>(generator, IRVariableDeclaration { node->name, node->bound_type });
    if (node->initializer) {
        generator.generate(node->initializer);
        auto &rhs_type { node->initializer->bound_type };
        auto &lhs_type { node->bound_type };
        add_operation<Operation::PushVarAddress>(generator, VarPath { node->name, node->bound_type, 0 });
        if (rhs_type->kind() == TypeKind::ReferenceType) {
            add_operation<Operation::AssignFromRef>(generator, lhs_type);
        } else {
            add_operation<Operation::AssignValue>(generator, lhs_type);
        }
    }
    add_operation<Operation::PushVarAddress>(generator, VarPath { node->name, node->bound_type, 0 });
    add_operation<Operation::Dereference>(generator, node->bound_type->value_type());
}

template<>
void generate_node(Generator &generator, std::shared_ptr<WhileStatement> const &node)
{
    add_operation<Operation::PushConstant>(generator, make_value(node->statement->bound_type->value_type()));
    Context::LoopDescriptor const ld { node->label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    generator.generate(node->condition);
    if (auto value_type = node->condition->bound_type->value_type(); node->condition->bound_type != value_type) {
        add_operation<Operation::Dereference>(generator, value_type);
    }
    add_operation<Operation::JumpF>(generator, ld.loop_end);
    add_operation<Operation::Discard>(generator, node->statement->bound_type);
    generator.generate(node->statement);
    add_operation<Operation::Jump>(generator, ld.loop_begin);
    add_operation<Operation::Label>(generator, ld.loop_end);
    generator.ctxs.pop_back();
}

void Generator::generate(pSyntaxNode const &node)
{
    switch (node->type) {
#undef S
#define S(T)                                                             \
    case SyntaxNodeType::T: {                                            \
        trace("generate_node(" #T ")");                                  \
        generate_node(*this, std::dynamic_pointer_cast<Arwen::T>(node)); \
    } break;
        SyntaxNodeTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

IRNode generate_ir(pSyntaxNode const &node)
{
    Generator generator {};
    generator.generate(node);
    auto const &ret = generator.ctxs.back().ir_node;
    return ret;
}
}

std::wostream &operator<<(std::wostream &os, std::monostate const &)
{
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::IRVariableDeclaration const &var_decl)
{
    os << var_decl.name << ':' << var_decl.type->name;
    return os;
}

std::wostream &operator<<(std::wostream &os, std::vector<Arwen::IR::IRVariableDeclaration> const &var_decls)
{
    os << '[';
    bool first { true };
    for (auto const &var_decl : var_decls) {
        if (!first) {
            os << ',';
        }
        first = false;
        os << var_decl;
    }
    os << ']';
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::VarPath const &var_path)
{
    os << var_path.name << ':' << var_path.offset << ' ' << var_path.type->to_string();
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::BreakOp const &break_op)
{
    os << break_op.scope_end << ',' << break_op.depth << ',' << break_op.label;
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::CallOp const &call)
{
    os << call.name;
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::BinaryOp const &op)
{
    os << Arwen::Operator_name(op.op);
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::ScopeEndOp const &scope_end_op)
{
    os << scope_end_op.enclosing_end << ',' << scope_end_op.has_defers;
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation::UnaryOp const &op)
{
    os << Arwen::Operator_name(op.op);
    return os;
}

std::wostream &operator<<(std::wostream &os, Arwen::IR::Operation const &op)
{
    std::string s;
    switch (op.type()) {
#undef S
#define S(T, P)                              \
    case Arwen::IR::Operation::Type::T:      \
        s = std::format("{:15.15}    ", #T); \
        break;
        IROperationTypes(S)
#undef S
            default : Util::UNREACHABLE();
    }
    os << Util::as_wstring(s);
    std::visit(
        Util::overloads {
            [&os](std::monostate const &) -> void {
            },
            [&os](auto const &impl) -> void {
                os << impl.payload;
            } },
        op.op);
    return os;
}
