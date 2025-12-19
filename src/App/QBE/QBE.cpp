/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ranges>
#include <string>
#include <string_view>
#include <variant>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Pipe.h>
#include <Util/Process.h>
#include <Util/Utf8.h>

#include <App/Config.h>
#include <App/Operator.h>
#include <App/Type.h>
#include <App/Value.h>

#include <App/IR/IR.h>

#include <App/Parser.h>
#include <App/QBE/QBE.h>
#include <App/SyntaxNode.h>

namespace Arwen::QBE {

struct QBEContext {
    std::wstring              text;
    int                       next_label;
    int                       next_var;
    std::vector<std::wstring> strings;
    std::vector<std::string>  cstrings;
    fs::path                  file_name;
    bool                      has_exports;

    int add_string(std::wstring_view s)
    {
        for (auto const &[ix, str] : std::ranges::views::enumerate(strings)) {
            if (str == s) {
                return ix + 1;
            }
        }
        strings.emplace_back(s);
        return strings.size();
    }

    int add_cstring(std::string_view s)
    {
        for (auto const &[ix, str] : std::ranges::views::enumerate(cstrings)) {
            if (str == s) {
                return ix + 1;
            }
        }
        cstrings.emplace_back(s);
        return cstrings.size();
    }
};

using QBEContexts = std::vector<QBEContext>;

GenResult generate_qbe_node(ASTNode const &, QBEContext &);
GenResult generate_qbe_nodes(ASTNodes const &, QBEContext &);

bool qbe_first_class_type(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &) -> bool {
                return true;
            },
            [](FloatType const &) -> bool {
                return true;
            },
            [](BoolType const &) -> bool {
                return true;
            },
            [](ZeroTerminatedArray const &) -> bool {
                return true;
            },
            [](auto const &) -> bool {
                return false;
            },
        },
        type->description);
}

std::wstring_view qbe_type(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &descr) -> std::wstring_view {
                return (descr.width_bits < 64) ? L"w" : L"l";
            },
            [](BoolType const &) -> std::wstring_view {
                return L"w";
            },
            [](FloatType const &descr) -> std::wstring_view {
                return (descr.width_bits < 64) ? L"s" : L"d";
            },
            [](ZeroTerminatedArray const &) -> std::wstring_view {
                return L"l";
            },
            [](SliceType const &) -> std::wstring_view {
                return L":slice_t";
            },
            [](auto const &) -> std::wstring_view {
                return L"l";
            },
        },
        type->description);
}

char qbe_type_code(IntType const &type)
{
    switch (type.width_bits) {
    case 8:
        return 'b';
    case 16:
        return 'h';
    case 32:
        return 'w';
    case 64:
        return 'l';
    default:
        UNREACHABLE();
    }
}

char qbe_type_code(FloatType const &type)
{
    switch (type.width_bits) {
    case 32:
        return 's';
    case 64:
        return 'd';
    default:
        UNREACHABLE();
    }
}

char qbe_type_code(ZeroTerminatedArray const &)
{
    return 'l';
}

char qbe_type_code(BoolType const &)
{
    return 'w';
}

char qbe_type_code(auto const &type)
{
    NYI("qbe_type_code(`{}')", typeid(type).name());
}

char qbe_type_code(pType const &type)
{
    return std::visit(
        [](auto const &descr) -> char {
            return qbe_type_code(descr);
        },
        type->description);
}

using QBEOperandValue = std::variant<ASTNode, int>;

struct QBEOperand {
    QBEOperandValue value;
    pType           type;
};

struct QBEBinExpr {
    QBEOperand lhs;
    Operator   op;
    QBEOperand rhs;
};

struct QBEUnaryExpr {
    Operator   op;
    QBEOperand operand;
};

#define TRY_GENERATE(n, ctx)                                          \
    (                                                                 \
        {                                                             \
            int __var { 0 };                                          \
            if (auto __res = generate_qbe_node((n), (ctx)); !__res) { \
                return __res;                                         \
            } else {                                                  \
                __var = __res.value();                                \
            }                                                         \
            (__var);                                                  \
        })

static GenResult generate_not_inline(ASTNode const &n, QBEContext &ctx)
{
    if (!is<Constant>(n) || !qbe_first_class_type(n->bound_type)) {
        if (auto res = generate_qbe_node(n, ctx); !res) {
            return res;
        } else {
            assert(res.value() != 0);
            return res;
        }
    }
    return 0;
}

#define TRY_GENERATE_NOT_INLINE(n, ctx)                                 \
    (                                                                   \
        {                                                               \
            QBEOperandValue __val;                                      \
            if (auto __res = generate_not_inline((n), (ctx)); !__res) { \
                return __res;                                           \
            } else {                                                    \
                int __var = __res.value();                              \
                if (__var != 0) {                                       \
                    __val = __var;                                      \
                } else {                                                \
                    __val = (n);                                        \
                }                                                       \
            }                                                           \
            (__val);                                                    \
        })

static GenResult generate_inline(QBEOperandValue const &value, QBEContext &ctx)
{
    return std::visit(
        overloads {
            [&ctx](ASTNode const &n) -> GenResult {
                return generate_qbe_node(n, ctx);
            },
            [&ctx](int const &var) -> GenResult {
                ctx.text += std::format(L"%v{}", var);
                return var;
            },
        },
        value);
}

#define TRY_GENERATE_INLINE(val, ctx)                                 \
    (                                                                 \
        {                                                             \
            int __val = 0;                                            \
            if (auto __res = generate_inline((val), (ctx)); !__res) { \
                return __res;                                         \
            } else {                                                  \
                __val = __res.value();                                \
            }                                                         \
            (__val);                                                  \
        })

template<class TLeft, class TRight = TLeft>
static GenResult qbe_operator(QBEBinExpr const &expr, TLeft const &lhs, TRight const &rhs, QBEContext &ctx)
{
    fatal(L"Invalid operator `{}` `{}` `{}`", expr.lhs.type->to_string(), as_wstring(Operator_name(expr.op)), expr.rhs.type->to_string());
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, BoolType const &lhs, BoolType const &rhs, QBEContext &ctx)
{
    int var = ++ctx.next_var;
    ctx.text += std::format(L"    %v{} = {} ", var, qbe_type(expr.lhs.type));
    switch (expr.op) {
    case Operator::LogicalAnd:
        ctx.text += L"and";
        break;
    case Operator::LogicalOr:
        ctx.text += L"or";
        break;
    default:
        NYI("QBE mapping for bool operator `{}`", Operator_name(expr.op));
        break;
    }
    ctx.text += ' ';
    TRY_GENERATE_INLINE(expr.lhs.value, ctx);
    ctx.text += L", ";
    TRY_GENERATE_INLINE(expr.rhs.value, ctx);
    ctx.text += '\n';
    return var;
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, IntType const &lhs, IntType const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::Divide || expr.op == Operator::Modulo) {
        int zero = ++ctx.next_var;
        int carry_on = ++ctx.next_label;
        int abort_mission = ++ctx.next_label;
        ctx.text += std::format(L"    %v{} = w cne{} 0, ", zero, qbe_type_code(lhs));
        TRY_GENERATE_INLINE(expr.rhs.value, ctx);
        ctx.text += std::format(L"\n    jnz %v{}, @lbl_{}, @lbl_{}\n", zero, carry_on, abort_mission);
        ctx.text += std::format(L"@lbl_{}\n", abort_mission);
        int division_by_zero = ctx.add_string(L"Division by zero");
        ctx.text += std::format(L"    call $arwen$abort(l $str_{}, l {})\n",
            division_by_zero, strlen("Division by zero"));
        ctx.text += std::format(L"@lbl_{}\n", carry_on);
    }
    int var = ++ctx.next_var;
    ctx.text += std::format(L"    %v{} = {} ", var, qbe_type(expr.lhs.type));
    switch (expr.op) {
    case Operator::Add:
        ctx.text += L"add";
        break;
    case Operator::BinaryAnd:
        ctx.text += L"and";
        break;
    case Operator::BinaryOr:
        ctx.text += L"or";
        break;
    case Operator::BinaryXor:
        ctx.text += L"xor";
        break;
    case Operator::Divide:
        ctx.text += (lhs.is_signed) ? L"div" : L"udiv";
        break;
    case Operator::Equals:
        ctx.text += L"ceq";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Greater:
        ctx.text += (lhs.is_signed) ? L"cugt" : L"csgt";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::GreaterEqual:
        ctx.text += (lhs.is_signed) ? L"cuge" : L"csge";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Less:
        ctx.text += (lhs.is_signed) ? L"cult" : L"cslt";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::LessEqual:
        ctx.text += (lhs.is_signed) ? L"cule" : L"csle";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Modulo:
        ctx.text += (lhs.is_signed) ? L"rem" : L"urem";
        break;
    case Operator::Multiply:
        ctx.text += L"mul";
        break;
    case Operator::NotEqual:
        ctx.text += L"cne";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::ShiftLeft:
        ctx.text += L"shl";
        break;
    case Operator::ShiftRight:
        ctx.text += L"shr";
        break;
    case Operator::Subtract:
        ctx.text += L"sub";
        break;
    default:
        NYI("QBE mapping for int operator `{}`", Operator_name(expr.op));
        break;
    }
    ctx.text += ' ';
    TRY_GENERATE_INLINE(expr.lhs.value, ctx);
    ctx.text += L", ";
    TRY_GENERATE_INLINE(expr.rhs.value, ctx);
    ctx.text += '\n';
    return var;
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, FloatType const &lhs, FloatType const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::Divide || expr.op == Operator::Modulo) {
        int zero = ++ctx.next_var;
        int carry_on = ++ctx.next_label;
        int abort_mission = ++ctx.next_label;
        ctx.text += std::format(L"    %v{} = w cne{} 0, ", zero, qbe_type_code(lhs));
        TRY_GENERATE_INLINE(expr.rhs.value, ctx);
        ctx.text += std::format(L"\n    jnz %v{}, @lbl_{}, @lbl_{}\n", zero, carry_on, abort_mission);
        ctx.text += std::format(L"@lbl_{}\n", abort_mission);
        int division_by_zero = ctx.add_string(L"Division by zero");
        ctx.text += std::format(L"    call $arwen$abort(l $str_{}, l {})\n",
            division_by_zero, strlen("Division by zero"));
        ctx.text += std::format(L"@lbl_{}\n", carry_on);
    }
    int var = ++ctx.next_var;
    ctx.text += std::format(L"    %v{} = {} ", var, qbe_type(expr.lhs.type));
    switch (expr.op) {
    case Operator::Add:
        ctx.text += L"add";
        break;
    case Operator::Divide:
        ctx.text += L"div";
        break;
    case Operator::Equals:
        ctx.text += L"ceq";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Greater:
        ctx.text += L"cgt";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::GreaterEqual:
        ctx.text += L"cge";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Less:
        ctx.text += L"clt";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::LessEqual:
        ctx.text += L"cle";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Multiply:
        ctx.text += L"mul";
        break;
    case Operator::NotEqual:
        ctx.text += L"cne";
        ctx.text += qbe_type_code(lhs);
        break;
    case Operator::Subtract:
        ctx.text += L"sub";
        break;
    default:
        NYI("QBE mapping for float operator `{}`", Operator_name(expr.op));
        break;
    }
    ctx.text += ' ';
    TRY_GENERATE_INLINE(expr.lhs.value, ctx);
    ctx.text += L", ";
    TRY_GENERATE_INLINE(expr.rhs.value, ctx);
    ctx.text += '\n';
    return var;
}

static GenResult qbe_operator(QBEBinExpr const &expr, QBEContext &ctx)
{
    return std::visit(
        [&expr, &ctx](auto const &lhs_descr, auto const &rhs_descr) {
            return qbe_operator(expr, lhs_descr, rhs_descr, ctx);
        },
        expr.lhs.type->description, expr.rhs.type->description);
}

template<class T>
static GenResult qbe_operator(QBEUnaryExpr const &expr, T const &operand, QBEContext &ctx)
{
    fatal(L"Invalid operator `{}` `{}` ", expr.operand.type->to_string(), as_wstring(Operator_name(expr.op)));
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, BoolType const &, QBEContext &ctx)
{
    int var = ++ctx.next_var;
    switch (expr.op) {
    case Operator::LogicalInvert:
        ctx.text += std::format(L"    %v{} = {} xor ", var, qbe_type(TypeRegistry::boolean));
        TRY_GENERATE_INLINE(expr.operand.value, ctx);
        var = ++ctx.next_var;
        ctx.text += std::format(LR"(, 1
    %v{} = {} and %v{}, 1
)",
            var, qbe_type(TypeRegistry::boolean), var - 1);
        break;
    default:
        NYI("QBE mapping for bool operator `{}`", Operator_name(expr.op));
        break;
    }
    return var;
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, IntType const &, QBEContext &ctx)
{
    int var = ++ctx.next_var;
    switch (expr.op) {
    case Operator::Negate:
        ctx.text += std::format(L"    %v{} = {} neg ", var, qbe_type(expr.operand.type));
        TRY_GENERATE_INLINE(expr.operand.value, ctx);
        ctx.text += '\n';
        break;
    case Operator::BinaryInvert:
        ctx.text += std::format(L"    %v{} = {} xor ~0, ", var, qbe_type(TypeRegistry::boolean));
        TRY_GENERATE_INLINE(expr.operand.value, ctx);
        var = ++ctx.next_var;
        ctx.text += std::format(LR"(
    %v{} = {} and %v{}, ~0
)",
            var, qbe_type(TypeRegistry::boolean), var - 1);
        break;
    default:
        NYI("QBE mapping for int operator `{}`", Operator_name(expr.op));
        break;
    }
    return var;
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, FloatType const &operand, QBEContext &ctx)
{
    int var = ++ctx.next_var;
    switch (expr.op) {
    case Operator::Negate:
        ctx.text += std::format(L"    %v{} = {} neg ", var, qbe_type(expr.operand.type));
        TRY_GENERATE_INLINE(expr.operand.value, ctx);
        ctx.text += '\n';
        break;
    default:
        NYI("QBE mapping for float operator `{}`", Operator_name(expr.op));
        break;
    }
    return var;
}

static GenResult qbe_operator(QBEUnaryExpr const &expr, QBEContext &ctx)
{
    if (expr.op == Operator::Idempotent) {
        if (std::holds_alternative<int>(expr.operand.value)) {
            return std::get<int>(expr.operand.value);
        }
        int var = ++ctx.next_var;
        ctx.text += std::format(L"    %{} = {} copy ", var, qbe_type(expr.operand.type));
        TRY_GENERATE_INLINE(expr.operand.value, ctx);
        return var;
    }
    return std::visit(
        [&expr, &ctx](auto const &descr) {
            return qbe_operator(expr, descr, ctx);
        },
        expr.operand.type->description);
}

static GenResult assign(std::wstring_view varname, pType const &type, ASTNode const &rhs, QBEContext &ctx)
{
    auto rhs_val = TRY_GENERATE_NOT_INLINE(rhs, ctx);
    ctx.text += std::format(L"    store{} ", qbe_type_code(rhs->bound_type));
    auto var = TRY_GENERATE_INLINE(rhs_val, ctx);
    ctx.text += std::format(L", %{}$\n", varname);
    return var;
}

template<class Node>
GenResult generate_qbe_node(ASTNode const &, Node const &, QBEContext &)
{
    NYI("Unimplemented QBE serialization for {}", typeid(Node).name());
}

template<>
GenResult generate_qbe_node(ASTNode const &n, BinaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.rhs->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    if (impl.op == Operator::Assign) {
        auto ident = get<Identifier>(impl.lhs);
        return assign(ident.identifier, rhs_value_type, impl.rhs, ctx);
    }
    QBEOperand lhs_operand { TRY_GENERATE_NOT_INLINE(impl.lhs, ctx), impl.lhs->bound_type };
    QBEOperand rhs_operand { TRY_GENERATE_NOT_INLINE(impl.rhs, ctx), impl.rhs->bound_type };
    QBEBinExpr expr { lhs_operand, impl.op, rhs_operand };
    auto       var = qbe_operator(expr, ctx);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Block const &impl, QBEContext &ctx)
{
    ctx.text += std::format(L"@lbl_{}\n", ++ctx.next_label);
    for (auto const &s : impl.statements) {
        if (auto res = generate_qbe_node(s, ctx); !res) {
            return res;
        }
    }
    return {};
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Call const &impl, QBEContext &ctx)
{
    auto def = get<FunctionDefinition>(impl.function);
    auto decl = get<FunctionDeclaration>(def.declaration);

    std::vector<QBEOperandValue> args;
    for (auto const &expression : get<ExpressionList>(impl.arguments).expressions) {
        args.emplace_back(TRY_GENERATE_NOT_INLINE(expression, ctx));
    }
    ctx.text += L"    ";
    int ret = 0;
    if (n->bound_type != TypeRegistry::void_) {
        ret = ++ctx.next_var;
        ctx.text += std::format(L"%v{} = {} ", ret, qbe_type(n->bound_type));
    }
    auto name = std::visit(
        overloads {
            [](ExternLink const &link) -> std::wstring_view {
                if (auto colon = link.link_name.rfind(L':'); colon != std::wstring::npos) {
                    return std::wstring_view { link.link_name }.substr(colon + 1);
                }
                return link.link_name;
            },
            [&def](auto const &) -> std::wstring_view {
                return def.name;
            } },
        def.implementation->node);
    ctx.text += std::format(L"call ${}(", name);
    bool first = true;
    for (auto const &[param, arg] : std::ranges::views::zip(decl.parameters, args)) {
        if (!first) {
            ctx.text += L", ";
        }
        first = false;
        ctx.text += std::format(L"{} ", qbe_type(param->bound_type));
        TRY_GENERATE_INLINE(arg, ctx);
    }
    ctx.text += L")\n";
    return ret;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Comptime const &impl, QBEContext &ctx)
{
    return generate_qbe_node(impl.statements, ctx);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Constant const &impl, QBEContext &ctx)
{
    if (impl.bound_value->type == TypeRegistry::void_) {
        return 0;
    }
    int var = 0;
    std::visit(
        overloads {
            [&var, &ctx, &impl](BoolType const &) -> void {
                ctx.text += std::format(L"{}", as<bool>(*impl.bound_value) ? 1 : 0);
            },
            [&ctx, &impl](IntType const &int_type) -> void {
                switch (int_type.width_bits) {
                case 8:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int8_t>(*impl.bound_value) : as<uint8_t>(*impl.bound_value));
                    break;
                case 16:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int16_t>(*impl.bound_value) : as<uint16_t>(*impl.bound_value));
                    break;
                case 32:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int32_t>(*impl.bound_value) : as<uint32_t>(*impl.bound_value));
                    break;
                case 64:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int64_t>(*impl.bound_value) : as<uint64_t>(*impl.bound_value));
                    break;
                }
            },
            [&var, &ctx, &impl](FloatType const &float_type) -> void {
                switch (float_type.width_bits) {
                case 32:
                    ctx.text += std::format(L"{}", as<float>(*impl.bound_value));
                    break;
                case 64:
                    ctx.text += std::format(L"{}", as<double>(*impl.bound_value));
                    break;
                }
            },
            [&ctx, &impl, &var](ZeroTerminatedArray const &zta) -> void {
                var = ++ctx.next_var;
                assert(zta.array_of == TypeRegistry::u8);
                auto cstr = static_cast<char const *>(as<void *>(*impl.bound_value));
                auto str_id = ctx.add_cstring(static_cast<char const *>(as<void *>(*impl.bound_value)));
                auto len_id = ++ctx.next_var;
                ctx.text += std::format(L"$cstr_{}", str_id);
            },
            [&ctx, &impl, &var](SliceType const &slice_type) -> void {
                var = ++ctx.next_var;
                ctx.text += L"    ";
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(*impl.bound_value);
                auto str_id = ctx.add_string(std::wstring_view { static_cast<wchar_t *>(slice.ptr), static_cast<size_t>(slice.size) });
                auto len_id = ++ctx.next_var;
                ctx.text += std::format(
                    LR"(%v{} = l alloc16 16
    storel $str_{}, %v{}
    %v{} = l add %v{}, 8
    storel {}, %v{}
)",
                    var, str_id, var, len_id, var, slice.size, len_id);
            },
            [](auto const &descr) -> void {
                UNREACHABLE();
            } },
        impl.bound_value->type->description);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Dummy const &impl, QBEContext &ctx)
{
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDefinition const &impl, QBEContext &ctx)
{
    if (!is<ExternLink>(impl.implementation)) {
        if (auto res = generate_qbe_node(impl.declaration, ctx); !res) {
            return res;
        }
        TRY_GENERATE(impl.implementation, ctx);
        ctx.text += L"}\n\n";
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDeclaration const &impl, QBEContext &ctx)
{
    ctx.text += std::format(L"function {} ${}(", qbe_type(impl.return_type->bound_type), impl.name);
    auto first = true;
    for (auto const &param : impl.parameters) {
        auto p = get<Parameter>(param);
        if (!first) {
            ctx.text += L", ";
        }
        first = false;
        ctx.text += std::format(L"{} %{}$$", qbe_type(param->bound_type), p.name);
    }
    ctx.text += ')';
    ctx.text += L" {\n@start\n";
    ctx.next_var = 0;
    ctx.next_label = 0;
    auto _ = generate_qbe_nodes(impl.parameters, ctx);
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Identifier const &impl, QBEContext &ctx)
{
    auto              var = ++ctx.next_var;
    auto              size = n->bound_type->size_of();
    std::wstring_view code = std::visit(
        overloads {
            [](IntType const &int_type) -> std::wstring_view {
                switch (int_type.width_bits) {
                case 8:
                    return (int_type.is_signed) ? L"sb" : L"ub";
                    break;
                case 16:
                    return (int_type.is_signed) ? L"sh" : L"uh";
                    break;
                case 32:
                    return (int_type.is_signed) ? L"sw" : L"uw";
                    break;
                case 64:
                    return L"l";
                    break;
                default:
                    UNREACHABLE();
                }
            },
            [](BoolType const &) -> std::wstring_view {
                return L"w";
            },
            [](FloatType const &float_type) -> std::wstring_view {
                return (float_type.width_bits == 64) ? L"d" : L"s";
            },
            [](ZeroTerminatedArray const &) -> std::wstring_view {
                return L"l";
            },
            [](auto const &) -> std::wstring_view {
                NYI("Identifier");
            } },
        n->bound_type->description);
    ctx.text += std::format(L"    %v{} = {} load{} %{}$\n", var, qbe_type(n->bound_type), code, impl.identifier);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, IfStatement const &impl, QBEContext &ctx)
{
    auto if_block = ++ctx.next_label;
    auto after_if = ++ctx.next_label;
    int  else_block = 0;
    auto cond_false = after_if;
    if (impl.else_branch != nullptr) {
        else_block = ++ctx.next_label;
        cond_false = else_block;
    }
    int condition_var = TRY_GENERATE(impl.condition, ctx);
    ctx.text += std::format(L"    jnz %v{}, @lbl_{}, @lbl_{}\n", condition_var, if_block, cond_false);
    ctx.text += std::format(L"@lbl_{}\n", if_block);
    TRY_GENERATE(impl.if_branch, ctx);
    ctx.text += std::format(L"    jmp @lbl_{}\n", after_if);
    if (impl.else_branch != nullptr) {
        ctx.text += std::format(L"@lbl_{}\n", else_block);
        TRY_GENERATE(impl.else_branch, ctx);
    }
    ctx.text += std::format(L"@lbl_{}\n", after_if);
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Module const &impl, QBEContext &ctx)
{
    ctx.text += LR"(type :slice_t = { l, l }

)";
    if (auto res = generate_qbe_nodes(impl.statements, ctx); !res) {
        return res;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(ctx.strings)) {
        ctx.text += std::format(L"data $str_{} = {{ ", ix + 1);
        for (auto ch : s) {
            ctx.text += std::format(L"w {:d}, ", ch);
        }
        ctx.text += L"w 0 }\n";
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(ctx.cstrings)) {
        ctx.text += std::format(L"data $cstr_{} = {{ ", ix + 1);
        for (auto ch : s) {
            ctx.text += std::format(L"b {:d}, ", ch);
        }
        ctx.text += L"b 0 }\n";
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Parameter const &impl, QBEContext &ctx)
{
    auto size = n->bound_type->size_of();
    ctx.text += std::format(L"    %{}$ = l alloc{} {}\n", impl.name, (size < 8) ? '4' : '8', size);
    ctx.text += std::format(L"    store{} %{}$$, %{}$\n", qbe_type_code(n->bound_type), impl.name, impl.name);
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Program const &impl, QBEContext &ctx)
{
    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    QBEContexts           objects;
    std::vector<fs::path> o_files;

    for (auto const &[mod_name, mod] : impl.modules) {
        auto &mod_ctx = objects.emplace_back();
        mod_ctx.file_name = dot_arwen / mod_name;
        mod_ctx.file_name.replace_extension("ssa");
        if (auto res = generate_qbe_node(mod, mod_ctx); !res) {
            return res;
        }
        if (mod_ctx.has_exports) {
            {
                std::wofstream os { mod_ctx.file_name };
                os << mod_ctx.text << '\n';
            }
            fs::path s_file { mod_ctx.file_name };
            s_file.replace_extension("s");
            fs::path o_file { mod_ctx.file_name };
            o_file.replace_extension("o");

            info("[QBE] Compiling `{}`", mod_ctx.file_name.string());
            Util::Process qbe { "qbe", "-o", s_file.string(), mod_ctx.file_name.string() };
            if (auto res = qbe.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"qbe execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"qbe failed: {}", as_wstring(qbe.stderr())));
            }
            Util::Process as { "as", "-o", o_file.string(), s_file.string() };
            if (auto res = as.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"as execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"as failed: {}", as_wstring(as.stderr())));
            }
            if (!has_option("keep-assembly")) {
                fs::remove(mod_ctx.file_name);
            }
            o_files.push_back(o_file);
        }
    }

    if (!o_files.empty()) {
        fs::path program_path { as_utf8(impl.name) };
        program_path.replace_extension();
        info("[QBE] Linking `{}`", program_path.string());

        std::vector<std::string> ld_args {
            "-o",
            fs::path { as_utf8(impl.name) }.replace_extension("").string(),
            // "-no-pie",
            std::format("-L{}/lib", Arwen::arwen_dir().string()),
            "-larwenrt",
        };
        if (has_option("L")) {
            for (auto const &lib_path : get_option_values("L")) {
                ld_args.push_back(std::format("-L{}", lib_path));
            }
        }
        if (has_option("l")) {
            for (auto const &lib : get_option_values("l")) {
                ld_args.push_back(std::format("-l{}", lib));
            }
        }
        for (auto const &o : o_files) {
            ld_args.push_back(o.string());
        }

        Util::Process link { "cc", ld_args };
        if (auto res = link.execute(); !res.has_value()) {
            return std::unexpected(std::format(L"Linker execution failed: {}", as_wstring(res.error().description)));
        } else if (res.value() != 0) {
            return std::unexpected(std::format(L"Linking failed: {}", as_wstring(link.stderr())));
        }
        if (!has_option("keep-objects")) {
            for (auto const &o : o_files) {
                fs::remove(o);
            }
        }
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, PublicDeclaration const &impl, QBEContext &ctx)
{
    ctx.text += L"export ";
    ctx.has_exports = true;
    return generate_qbe_node(impl.declaration, ctx);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Return const &impl, QBEContext &ctx)
{
    QBEOperandValue val;
    if (impl.expression != nullptr) {
        val = TRY_GENERATE_NOT_INLINE(impl.expression, ctx);
    }
    ctx.text += L"    ret";
    if (impl.expression != nullptr) {
        ctx.text += ' ';
        TRY_GENERATE_INLINE(val, ctx);
    }
    ctx.text += '\n';
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, VariableDeclaration const &impl, QBEContext &ctx)
{
    auto size = n->bound_type->size_of();
    ctx.text += std::format(L"    %{}$ = l alloc{} {}\n", impl.name, (size < 8) ? '4' : '8', size);
    if (impl.initializer != nullptr) {
        return assign(impl.name, n->bound_type, impl.initializer, ctx);
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, UnaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.operand->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    QBEOperand   operand { TRY_GENERATE_NOT_INLINE(impl.operand, ctx), impl.operand->bound_type };
    QBEUnaryExpr expr { impl.op, operand };
    auto         var = qbe_operator(expr, ctx);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, WhileStatement const &impl, QBEContext &ctx)
{
    auto top_loop = ++ctx.next_label;
    auto cont_loop = ++ctx.next_label;
    auto end_loop = ++ctx.next_label;
    ctx.text += std::format(L"@lbl_{}\n", top_loop);
    int condition_var = TRY_GENERATE(impl.condition, ctx);
    ctx.text += std::format(L"    jnz %v{}, @lbl_{}, @lbl_{}\n", condition_var, cont_loop, end_loop);
    ctx.text += std::format(L"@lbl_{}\n", cont_loop);
    TRY_GENERATE(impl.statement, ctx);
    ctx.text += std::format(L"    jmp @lbl_{}\n", top_loop);
    ctx.text += std::format(L"@lbl_{}\n", end_loop);
    return 0;
}

GenResult generate_qbe_node(ASTNode const &n, QBEContext &ctx)
{
    return std::visit(
        [&n, &ctx](auto const &impl) -> GenResult {
            return generate_qbe_node(n, impl, ctx);
        },
        n->node);
}

GenResult generate_qbe_nodes(ASTNodes const &nodes, QBEContext &ctx)
{
    int var = 0;
    for (auto const &n : nodes) {
        var = TRY_GENERATE(n, ctx);
    }
    return var;
}

GenResult generate_qbe(ASTNode const &n)
{
    QBEContext ctx {};
    return generate_qbe_node(n, ctx);
}
}
