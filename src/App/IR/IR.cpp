/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>
#include <ranges>

#include <App/IR/IR.h>
#include <Util/Logging.h>

namespace Arwen::IR {

size_t IRNodes::size() const
{
    return nodes.size();
}

bool IRNodes::empty() const
{
    return nodes.empty();
}

IRNode const &IRNodes::operator[](size_t ix) const
{
    return nodes[ix];
}

IRNode &IRNodes::operator[](size_t ix)
{
    return nodes[ix];
}

std::wostream &list(Operation const &op, std::wostream &os)
{
    if (is<Operation::Label>(op)) {
        os << get<Operation::Label>(op).payload << ":\n";
        return os;
    }
    os << "    " << op << '\n';
    return os;
}

std::wostream &Function::list(pIR const &, std::wostream &os) const
{
    return os;
}

std::wostream &Module::list(pIR const &ir, std::wostream &os) const
{
    os << '\n';
    for (auto const &func : functions | std::views::values) {
        list_node(func, os);
    }
    return os;
}

std::wostream &Program::list(pIR const &ir, std::wostream &os) const
{
    os << '\n';
    for (auto const &func : functions | std::views::values) {
        list_node(func, os);
    }
    os << '\n';
    for (auto const &mod : modules | std::views::values) {
        list_node(mod, os);
    }
    return os;
}

std::wostream &list_node(pIR const &ir, std::wostream &os)
{
    std::wcout << "== ["
               << std::visit(overloads { [](Function const &) -> wchar_t { return L'F'; },
                                 [](Module const &) -> wchar_t { return L'M'; },
                                 [](Program const &) -> wchar_t { return L'P'; } },
                      ir->node)
               << "] = " << ir->name << " ===================\n";
    for (auto const &op : ir->operations) {
        list(op, os);
    }
    std::visit([&os, &ir](auto const &n) { n.list(ir, os); }, ir->node);
    return os;
}

std::wostream &list(IRNodes const &ir, std::wostream &os)
{
    for (size_t ix = 0; ix < ir.size(); ++ix) {
        list_node(pIR { const_cast<IRNodes *>(&ir), ix }, os);
    }
    return os;
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

std::wostream &operator<<(std::wostream &os, Arwen::IR::Declarations const &var_decls)
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
