/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/QBE/QBE.h>

namespace Arwen::QBE {

std::wostream &operator<<(std::wostream &os, ILBaseType const &type)
{
    switch (type) {
#undef S
#define S(T, Code, Str) \
    case ILBaseType::T: \
        os << #Str;     \
        break;
        ILBASETYPES(S)
#undef S
    default:
        UNREACHABLE();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, ILType const &type)
{
    std::visit(
        overloads {
            [&os](ILBaseType const &inner) {
                os << inner;
            },
            [&os](std::wstring const &inner) {
                os << ':' << inner;
            } },
        type);
    return os;
}

std::wostream &operator<<(std::wostream &os, ILOperation const &op)
{
    switch (op) {
#undef S
#define S(Op, Str, ArwenOp) \
    case ILOperation::Op:   \
        os << #Str;         \
        break;
        ILOPERATIONS(S)
#undef S
    default:
        UNREACHABLE();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, ILValue const &value)
{
    std::visit(
        overloads {
            [&os](Local const &local) {
                os << "%v" << local.var;
            },
            [&os](ILValue::Global const &global) {
                os << '$' << global.name;
            },
            [&os](ILValue::Literal const &literal) {
                os << literal.literal;
            },
            [&os](ILValue::Variable const &param) {
                os << std::format(L"%{}$", param.name);
            },
            [&os](ILValue::Parameter const &param) {
                os << std::format(L"%{}$$", param.name);
            },
            [&os, &value](double const &dbl) {
                std::visit(
                    overloads {
                        [&os](ILBaseType const &type) {
                            switch (type) {
                            case ILBaseType::D:
                                os << "d_";
                                break;
                            case ILBaseType::S:
                                os << "s_";
                                break;
                            default:
                                break;
                            }
                        },
                        [](auto const &t) {
                            UNREACHABLE();
                        } },
                    value.type);
                os << dbl;
            },
            [&os](int64_t const &i) {
                os << i;
            } },
        value.inner);
    return os;
}

std::wostream &operator<<(std::wostream &os, AllocDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " alloc" << impl.alignment << " " << impl.bytes;
    return os;
}

std::wostream &operator<<(std::wostream &os, BlitDef const &impl)
{
    os << "    blit " << impl.src << ", " << impl.dest << ", " << impl.bytes;
    return os;
}

std::wostream &operator<<(std::wostream &os, CallDef const &impl)
{
    os << "    ";
    if (!std::holds_alternative<ILBaseType>(impl.target.type)
        || std::get<ILBaseType>(impl.target.type) != ILBaseType::V) {
        os << impl.target << " = " << impl.target.type << ' ';
    }
    std::wstring_view n { impl.name };
    if (auto colon = n.rfind(L':'); colon != std::wstring_view::npos) {
        n = std::wstring_view { n }.substr(colon + 1);
    }
    os << "call $" << n << '(';
    auto first { true };
    for (auto const &arg : impl.args) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << arg.type << " " << arg;
    }
    os << ")";
    return os;
}

std::wostream &operator<<(std::wostream &os, CastDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " cast " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, CopyDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " copy " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, ExprDef const &impl)
{
    std::wstringstream op;
    assert(std::holds_alternative<ILBaseType>(impl.target.type));
    auto t = std::get<ILBaseType>(impl.target.type);
    if (impl.op >= ILOperation::Equals) {
        os << 'c';
        if (impl.op >= ILOperation::GreaterEqual) {
            switch (t) {
            case ILBaseType::UB:
            case ILBaseType::UH:
            case ILBaseType::UW:
                op << 'u';
                break;
            case ILBaseType::S:
            case ILBaseType::D:
                break;
            default:
                op << 's';
                break;
            }
        }
    }
    op << impl.op;
    if (impl.op >= ILOperation::GreaterEqual) {
        op << static_cast<ILOperation>(static_cast<uint16_t>(t) & 0xFC);
    }
    os << "    " << impl.target << " = " << impl.target.type << " " << op.str() << " " << impl.lhs << ", " << impl.rhs;
    return os;
}

std::wostream &operator<<(std::wostream &os, HltDef const &impl)
{
    os << "    hlt";
    return os;
}

std::wostream &operator<<(std::wostream &os, JmpDef const &impl)
{
    os << "    jmp @lbl_" << impl.label;
    return os;
}

std::wostream &operator<<(std::wostream &os, JnzDef const &impl)
{
    os << "    jnz " << impl.expr << ", @lbl_" << impl.on_true << ", @lbl_" << impl.on_false;
    return os;
}

std::wostream &operator<<(std::wostream &os, LabelDef const &impl)
{
    os << "@lbl_" << impl.label;
    return os;
}

std::wostream &operator<<(std::wostream &os, LoadDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " load" << impl.pointer.type << " " << impl.pointer;
    return os;
}

std::wostream &operator<<(std::wostream &os, PhiDef::PhiArgDef const &impl)
{
    os << "@lbl_" << impl.come_from << " " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, PhiDef const &impl)
{
    os << "    " << impl.target << " = " << impl.type << " phi ";
    bool first { true };
    for (auto const &arg : impl.args) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << arg;
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, RetDef const &impl)
{
    os << "    ret";
    if (impl.expr) {
        os << ' ' << impl.expr.value();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, StoreDef const &impl)
{
    os << "    store" << impl.expr.type << ' ' << impl.expr << ", " << impl.target;
    return os;
}

std::wostream &operator<<(std::wostream &os, VaArgDef const &impl)
{
    os << "    " << impl.target << " = " << impl.type << " vaarg " << impl.arglist;
    return os;
}

std::wostream &operator<<(std::wostream &os, VaStartDef const &impl)
{
    os << "    vastart " << impl.arglist;
    return os;
}

}
