/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Type.h"
#include "Util/Logging.h"
#include <Util/Defer.h>

#include <App/IR/IR.h>
#include <cstdint>
#include <memory>
#include <ranges>
#include <variant>
#include <vector>

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
    for (auto const &mod : prog->modules | std::views::values) {
        list(mod);
    }
}

void add_operation(Generator &generator, Operation op)
{
    for (auto &ctx : std::views::reverse(generator.ctxs)) {
        if (std::visit(overloads {
                           [&op](pFunction const &f) -> bool {
                               f->operations.emplace_back(std::move(op));
                               return true;
                           },
                           [&op](pModule const &mod) -> bool {
                               mod->operations.emplace_back(std::move(op));
                               return true;
                           },
                           [](auto const &) -> bool {
                               return false;
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
    if (node->op == Operator::Assign) {
        auto path = std::dynamic_pointer_cast<MemberPath>(node->lhs);
        assert(path != nullptr);
        std::vector<uint64_t> var_path;
        for (auto ix = 1; ix < path->path.size(); ++ix) {
            auto &t { path->path[ix - 1]->bound_type };
            assert(std::holds_alternative<StructType>(t->description));
            auto st = std::get<StructType>(t->description);
            for (auto fld_ix = 0; fld_ix < st.fields.size(); ++fld_ix) {
                if (st.fields[fld_ix].name == path->path[ix]->identifier) {
                    var_path.push_back(fld_ix);
                    break;
                }
            }
            assert(var_path.size() == ix);
        }
        generator.generate(node->rhs);
        add_operation<Operation::PushVarAddress>(generator, VarPath { path->path[0]->identifier, var_path });
        add_operation<Operation::Assign>(generator);
        return;
    }
    if (node->op == Operator::Cast) {
        generator.generate(node->lhs);
        auto const rhs_type = std::dynamic_pointer_cast<TypeSpecification>(node->rhs);
        assert(rhs_type != nullptr && rhs_type->bound_type != nullptr);
        add_operation<Operation::Cast>(generator, rhs_type->bound_type);
        return;
    }
    generator.generate(node->lhs);
    generator.generate(node->rhs);
    add_operation<Operation::BinaryOperator>(generator, Operation::BinaryOperator { node->lhs->bound_type, node->op, node->rhs->bound_type });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Block> const &node)
{
    if (generator.ctxs.empty()) {
        auto module = std::make_shared<Module>(L"anonymous", node);
        generator.ctxs.push_back(Context { module, {} });
    }
    add_operation<Operation::ScopeBegin>(generator, node->ns->variables.size());
    generator.ctxs.emplace_back(Context { {}, Context::BlockDescriptor {} });
    {
        Defer _ { [&generator]() { generator.ctxs.pop_back(); } };

        auto discard { false };
        auto empty { true };
        for (auto const &stmt : node->statements) {
            if (discard) {
                add_operation<Operation::Discard>(generator);
            }
            discard = stmt->type != SyntaxNodeType::DeferStatement
                && stmt->type != SyntaxNodeType::FunctionDefinition
                && stmt->type != SyntaxNodeType::Enum
                && stmt->type != SyntaxNodeType::Struct;
            empty &= !discard;
            generator.generate(stmt);
        }

        if (empty) {
            add_operation<Operation::PushConstant>(generator, make_void());
        }

        auto &ctx = generator.ctxs.back();
        auto &[defer_stmts] = std::get<Context::BlockDescriptor>(ctx.unwind);
        if (!defer_stmts.empty()) {
            auto const end_block = next_label();
            auto const &[_, last_defer_label] = defer_stmts.back();
            add_operation<Operation::Sub>(generator, last_defer_label);
            add_operation<Operation::Jump>(generator, end_block);
            for (auto const &[defer, label] : defer_stmts) {
                add_operation<Operation::Label>(generator, label);
                generator.generate(defer);
                add_operation<Operation::Discard>(generator);
            }
            add_operation<Operation::SubRet>(generator);
            add_operation<Operation::Label>(generator, end_block);
        }
    }
    add_operation<Operation::ScopeEnd>(generator);
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Break> const &node)
{
    while (!generator.ctxs.empty()) {
        auto const &ctx = generator.ctxs.back();
        auto        brk = std::visit(
            overloads {
                [&node, &generator](Context::LoopDescriptor const &ld) -> bool {
                    if (node->label->empty() || ld.name == node->label.value()) {
                        add_operation<Operation::Jump>(generator, ld.loop_end);
                        return true;
                    }
                    return false;
                },
                [&generator](Context::BlockDescriptor const &bd) -> bool {
                    if (!bd.defer_stmts.empty()) {
                        add_operation<Operation::Sub>(generator, bd.defer_stmts.back().second);
                    }
                    return false;
                },
                [](auto const &) {
                    return false;
                } },
            ctx.unwind);
        generator.ctxs.pop_back();
        if (brk) {
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
    for (auto const &expression : std::ranges::reverse_view(node->arguments->expressions)) {
        generator.generate(expression);
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
    while (!generator.ctxs.empty()) {
        auto const &ctx = generator.ctxs.back();
        auto        brk = std::visit(
            overloads {
                [&node, &generator](Context::LoopDescriptor const &ld) -> bool {
                    if (node->label->empty() || ld.name == node->label.value()) {
                        add_operation<Operation::Jump>(generator, ld.loop_begin);
                        return true;
                    }
                    return false;
                },
                [&generator](Context::BlockDescriptor const &bd) -> bool {
                    if (!bd.defer_stmts.empty()) {
                        add_operation<Operation::Sub>(generator, bd.defer_stmts.back().second);
                    }
                    return false;
                },
                [](auto const &) -> bool {
                    return false;
                } },
            ctx.unwind);
        generator.ctxs.pop_back();
        if (brk) {
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
            return;
        }
    }
    UNREACHABLE();
}

template<>
void generate_node(Generator &generator, std::shared_ptr<FunctionDefinition> const &node)
{
    if (auto const &extern_link = std::dynamic_pointer_cast<ExternLink>(node->implementation); extern_link == nullptr) {
        auto const &module = find_ir_node<pModule>(generator);
        auto        function = std::make_shared<Function>(node->name, node, module);
        module->functions.emplace(node->name, function);
        for (auto const &param : node->declaration->parameters) {
            function->parameters.emplace_back(param->name, param->bound_type);
        }
        generator.ctxs.push_back(Context { function, {} });
        generator.generate(node->implementation);
        generator.ctxs.pop_back();
    }
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Identifier> const &node)
{
    add_operation<Operation::PushValue>(generator, node->identifier);
}

template<>
void generate_node(Generator &generator, std::shared_ptr<IfStatement> const &node)
{
    generator.generate(node->condition);
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
    add_operation<Operation::PushConstant>(generator, make_void());
    Context::LoopDescriptor const ld { node->label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    add_operation<Operation::Discard>(generator);
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
    for (auto const &[name, var_node] : node->statements->ns->variables) {
        module->variables.emplace_back(name, var_node->bound_type);
    }
    program->modules.emplace(node->name, module);
    generator.ctxs.push_back(Context { module, {} });
    generator.generate(node->statements);
    generator.ctxs.pop_back();
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Arwen::Program> const &node)
{
    assert(generator.ctxs.empty());
    auto program = std::make_shared<Program>(node->name, node);
    generator.ctxs.push_back(Context { program, {} });
    for (auto &mod : node->modules | std::views::values) {
        generator.generate(mod);
    }
}

template<>
void generate_node(Generator &generator, std::shared_ptr<Return> const &node)
{
    generator.generate(node->expression);
    add_operation<Operation::Return>(generator);
}

template<>
void generate_node(Generator &generator, std::shared_ptr<UnaryExpression> const &node)
{
    if (node->op == Operator::AddressOf) {
        auto const path = std::dynamic_pointer_cast<MemberPath>(node->operand);
        assert(path != nullptr);
        if (path->path.size() > 1) {
            fatal("Can't do structs yet");
        }
        add_operation<Operation::PushVarAddress>(generator, path->path[0]->identifier);
        return;
    }
    if (node->op == Operator::Sizeof && node->operand->type == SyntaxNodeType::TypeSpecification) {
        add_operation<Operation::PushConstant>(generator, Value { TypeRegistry::i64, node->operand->bound_type->size_of() });
        return;
    }
    generator.generate(node->operand);
    add_operation<Operation::UnaryOperator>(generator, Operation::UnaryOperator { node->operand->bound_type, node->op });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<VariableDeclaration> const &node)
{
    add_operation<Operation::DeclVar>(generator, IRVariableDeclaration { node->name, node->bound_type });
}

template<>
void generate_node(Generator &generator, std::shared_ptr<WhileStatement> const &node)
{
    add_operation<Operation::PushConstant>(generator, make_void());
    Context::LoopDescriptor const ld { node->label.value_or(std::wstring {}), next_label(), next_label() };
    generator.ctxs.push_back(Context { {}, ld });
    add_operation<Operation::Label>(generator, ld.loop_begin);
    generator.generate(node->condition);
    add_operation<Operation::JumpF>(generator, ld.loop_end);
    add_operation<Operation::Discard>(generator);
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
    std::visit(overloads {
                   [](std::monostate const &) {
                   },
                   [](auto const &ir_node) {
                       list(ir_node);
                   } },
        ret);
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

std::wostream &operator<<(std::wostream &os, Arwen::IR::VarPath const &var_path)
{
    os << var_path.name;
    for (auto ix : var_path.path) {
        os << '.' << ix;
    }
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
