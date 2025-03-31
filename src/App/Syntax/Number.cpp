/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <string_view>
#include <type_traits>

#include <Util/Utf8.h>
#include <App/SyntaxNode.h>

namespace Arwen {

template<typename Func>
pSyntaxNode evaluate_numeric_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
{
    switch (rhs->type) {
    case SyntaxNodeType::Integer: {
        auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
        auto z = func(lhs->value, rhs_integer->value);
        if constexpr (std::is_integral_v<decltype(z)>) {
            return make_node<Integer>(z);
        }
        if constexpr (std::is_floating_point_v<decltype(z)>) {
            return make_node<Decimal>(z);
        }
        fatal("Unexpected return type in evaluate_numeric_op");
    }
    case SyntaxNodeType::Decimal: {
        auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
        auto z = func(lhs->value, rhs_decimal->value);
        if constexpr (std::is_integral_v<decltype(z)>) {
            return make_node<Integer>(z);
        }
        if constexpr (std::is_floating_point_v<decltype(z)>) {
            return make_node<Decimal>(z);
        }
        fatal("Unexpected return type in evaluate_numeric_op");
    }
    default:
        return nullptr;
    }
}

template<typename Func>
pSyntaxNode evaluate_comparison_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
{
    switch (rhs->type) {
    case SyntaxNodeType::Integer: {
        auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
        bool z = func(lhs->value, rhs_integer->value);
        return make_node<BoolConstant>(z);
    }
    case SyntaxNodeType::Decimal: {
        auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
        bool z = func(lhs->value, rhs_integer->value);
        return make_node<BoolConstant>(z);
    }
    default:
        return nullptr;
    }
}

bool check_zero(pConstantExpression const &expr)
{
    switch (expr->type) {
    case SyntaxNodeType::Integer:
        return std::dynamic_pointer_cast<Integer>(expr)->value == 0;
    case SyntaxNodeType::Decimal:
        return std::dynamic_pointer_cast<Integer>(expr)->value == 0;
    default:
        return false;
    }
}

Number::Number(std::wstring_view number, NumberType type)
    : SyntaxNode(SyntaxNodeType::Number)
    , number(number)
    , number_type(type)
{
}

pBoundNode Number::bind()
{
    return nullptr;
}

void Number::header()
{
    std::wcout << number << L" ";
    std::cout << NumberType_name(number_type);
}

pSyntaxNode Number::normalize()
{
    if (number_type == NumberType::Decimal) {
        return make_node<Decimal>(number);
    }
    return make_node<Integer>(number);
}

Decimal::Decimal(std::wstring const& number)
    : ConstantExpression(SyntaxNodeType::Decimal)
{
    char * end_ptr;
    auto narrow_string = as_utf8(number);
    value = strtod(as_utf8(narrow_string).data(), &end_ptr);
    assert(end_ptr != narrow_string.data());
}

Decimal::Decimal(double number)
    : ConstantExpression(SyntaxNodeType::Decimal)
    , value(number)
{
}

pBoundNode Decimal::bind()
{
    return nullptr;
}

void Decimal::header()
{
    std::wcout << value;
}

pSyntaxNode Decimal::evaluate_Add(pConstantExpression const &rhs)
{
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x + y; });
}

pSyntaxNode Decimal::evaluate_Subtract(pConstantExpression const &rhs)
{
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x - y; });
}

pSyntaxNode Decimal::evaluate_Multiply(pConstantExpression const &rhs)
{
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x * y; });
}

pSyntaxNode Decimal::evaluate_Divide(pConstantExpression const &rhs)
{
    if (auto rhs_is_zero = check_zero(rhs); rhs_is_zero) {
        std::wcerr << "Division by zero" << std::endl;
        return nullptr;
    }
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x / y; });
}

pSyntaxNode Decimal::evaluate_Modulo(pConstantExpression const &rhs)
{
    if (auto rhs_is_zero = check_zero(rhs); rhs_is_zero) {
        std::wcerr << "Division by zero" << std::endl;
        return nullptr;
    }
    switch (rhs->type) {
    case SyntaxNodeType::Integer: {
        auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
        return make_node<Decimal>(fmod(value, rhs_integer->value));
    }
    case SyntaxNodeType::Decimal: {
        auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
        return make_node<Decimal>(fmod(value, rhs_decimal->value));
    }
    default:
        return nullptr;
    }
}

pSyntaxNode Decimal::evaluate_Equals(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x == y; });
}

pSyntaxNode Decimal::evaluate_NotEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x != y; });
}

pSyntaxNode Decimal::evaluate_Less(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x < y; });
}

pSyntaxNode Decimal::evaluate_LessEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x <= y; });
}

pSyntaxNode Decimal::evaluate_Greater(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x > y; });
}

pSyntaxNode Decimal::evaluate_GreaterEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x >= y; });
}

Integer::Integer(std::wstring_view number)
    : ConstantExpression(SyntaxNodeType::Integer)
{
    this->value = string_to_integer<uint64_t>(number)
                      .or_else([number]() -> std::optional<uint64_t> {
                          std::wcerr << "Could not convert string '" << number << "' to integer. This is unexpected" << std::endl;
                          abort();
                          return { 0 };
                      })
                      .value();
}

Integer::Integer(uint64_t number)
    : ConstantExpression(SyntaxNodeType::Integer)
    , value(number)
{
}

pBoundNode Integer::bind()
{
    return nullptr;
}

void Integer::header()
{
    std::wcout << value;
}

pSyntaxNode Integer::evaluate_Add(pConstantExpression const &rhs)
{
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x + y; });
}

pSyntaxNode Integer::evaluate_Subtract(pConstantExpression const &rhs)
{
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x - y; });
}

pSyntaxNode Integer::evaluate_Multiply(pConstantExpression const &rhs)
{
    if (auto rhs_string = std::dynamic_pointer_cast<DoubleQuotedString>(rhs); rhs_string != nullptr) {
        std::wstring      s;
        for (auto ix = 0; ix < value; ++value) {
            s += rhs_string->string;
        }
        return make_node<DoubleQuotedString>(s, false);
    }
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x * y; });
}

pSyntaxNode Integer::evaluate_Divide(pConstantExpression const &rhs)
{
    if (auto rhs_is_zero = check_zero(rhs); rhs_is_zero) {
        std::wcerr << "Division by zero" << std::endl;
        return nullptr;
    }
    return evaluate_numeric_op(
        this, rhs,
        [](auto x, auto y) { return x / y; });
}

pSyntaxNode Integer::evaluate_Modulo(pConstantExpression const &rhs)
{
    if (auto rhs_is_zero = check_zero(rhs); rhs_is_zero) {
        std::wcerr << "Division by zero" << std::endl;
        return nullptr;
    }
    switch (rhs->type) {
    case SyntaxNodeType::Integer: {
        auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
        return make_node<Integer>(value % rhs_integer->value);
    }
    case SyntaxNodeType::Decimal: {
        auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
        return make_node<Decimal>(fmod(value, rhs_decimal->value));
    }
    default:
        return nullptr;
    }
}

pSyntaxNode Integer::evaluate_Equals(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x == y; });
}

pSyntaxNode Integer::evaluate_NotEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x != y; });
}

pSyntaxNode Integer::evaluate_Less(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x < y; });
}

pSyntaxNode Integer::evaluate_LessEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x <= y; });
}

pSyntaxNode Integer::evaluate_Greater(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x > y; });
}

pSyntaxNode Integer::evaluate_GreaterEqual(pConstantExpression const &rhs)
{
    return evaluate_comparison_op(
        this, rhs,
        [](auto x, auto y) { return x >= y; });
}

}
