/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

template<typename T, SyntaxNodeType NodeType>
struct Num : ConstantExpression {
    using Integer = Num<uint64_t, SyntaxNodeType::Integer>;
    using SignedInteger = Num<int64_t, SyntaxNodeType::SignedInteger>;
    using Decimal = Num<double, SyntaxNodeType::Decimal>;
    using This = Num<T, NodeType>;

    T value;

    Num(std::wstring_view number)
        : ConstantExpression(NodeType)
    {
        if constexpr (std::is_integral_v<T>) {
            this->value = string_to_integer<T>(number)
                              .or_else([number]() -> std::optional<T> {
                                  std::wcerr << "Could not convert string '" << number << "' to integer. This is unexpected" << std::endl;
                                  abort();
                                  return { 0 };
                              })
                              .value();
        }
        if constexpr (std::is_floating_point_v<T>) {
            char *end_ptr;
            auto  narrow_string = as_utf8(number);
            value = static_cast<T>(strtod(narrow_string.data(), &end_ptr));
            assert(end_ptr != narrow_string.data());
        }
    }

    Num(T number)
        : ConstantExpression(NodeType)
        , value(number)
    {
    }

    template<typename Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        fatal("Unexpected return type in evaluate_numeric_op");
    }

    template<std::unsigned_integral Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<uint64_t, SyntaxNodeType::Integer>>(std::move(location), result);
    }

    template<std::signed_integral Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<int64_t, SyntaxNodeType::SignedInteger>>(std::move(location), result);
    }

    template<std::floating_point Result>
    pSyntaxNode make_result(TokenLocation location, Result result)
    {
        return make_node<Num<double, SyntaxNodeType::Decimal>>(std::move(location), result);
    }

    template<typename Func>
    pSyntaxNode evaluate_numeric_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
    {
        switch (rhs->type) {
        case SyntaxNodeType::Integer: {
            auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::Decimal: {
            auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_decimal->value));
        }
        default:
            return nullptr;
        }
    }

    template<typename Func>
    pSyntaxNode evaluate_binary_op(auto *lhs, pConstantExpression const &rhs, Func const &func)
    {
        switch (rhs->type) {
        case SyntaxNodeType::Integer: {
            auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            return make_result(lhs->location + rhs->location, func(lhs->value, rhs_integer->value));
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
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        case SyntaxNodeType::SignedInteger: {
            auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
            bool z = func(lhs->value, rhs_integer->value);
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        case SyntaxNodeType::Decimal: {
            auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
            bool z = func(lhs->value, rhs_decimal->value);
            return make_node<BoolConstant>(lhs->location + rhs->location, z);
        }
        default:
            return nullptr;
        }
    }

    pSyntaxNode stamp(Parser &parser) override
    {
        return make_node<This>(location, value);
    }

    pType bind(Parser &parser) override
    {
        if (value > IntType::u32.max_value) {
            return TypeRegistry::u64;
        }
        return TypeRegistry::u32;
    }

    template<typename V>
    pSyntaxNode coerce_value(pType const &target, Parser &parser)
    {
        return ConstantExpression::coerce(target, parser);
    }

    template<std::unsigned_integral V>
    pSyntaxNode coerce_value(pType const &target, Parser &)
    {
        if (!std::holds_alternative<IntType>(target->description)) {
            return nullptr;
        }
        if (value <= std::get<IntType>(target->description).max_value) {
            auto ret = make_node<Integer>(location, value);
            ret->bound_type = target;
            return ret;
        }
        return nullptr;
    }

    template<std::signed_integral V>
    pSyntaxNode coerce_value(pType const &target, Parser &)
    {
        if (!std::holds_alternative<IntType>(target->description)) {
            return nullptr;
        }
        if (value >= std::get<IntType>(target->description).min_value
            && value <= std::get<IntType>(target->description).max_value) {
            auto ret = make_node<Integer>(location, value);
            ret->bound_type = target;
            return ret;
        }
        return nullptr;
    }

    pSyntaxNode coerce(pType const &target, Parser &parser) override
    {
        if (bound_type == target) {
            return shared_from_this();
        }
        return coerce_value<T>(target, parser);
    }

    std::wostream &header(std::wostream &os) override
    {
        return os << value;
    }

    pSyntaxNode evaluate_Add(pConstantExpression const &rhs) override
    {
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x + y; });
    }

    pSyntaxNode evaluate_Subtract(pConstantExpression const &rhs) override
    {
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x - y; });
    }

    pSyntaxNode evaluate_Multiply(pConstantExpression const &rhs) override
    {
        if (auto rhs_string = std::dynamic_pointer_cast<DoubleQuotedString>(rhs); rhs_string != nullptr) {
            std::wstring s;
            for (auto ix = 0; ix < value; ++value) {
                s += rhs_string->string;
            }
            return make_node<DoubleQuotedString>(location + rhs->location, s, false);
        }
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x * y; });
    }

    pSyntaxNode evaluate_Divide(pConstantExpression const &rhs) override
    {
        if (value == 0) {
            std::wcerr << "Division by zero" << std::endl;
            return nullptr;
        }
        return evaluate_numeric_op(
            this, rhs,
            [](auto x, auto y) { return x / y; });
    }

    pSyntaxNode evaluate_Modulo(pConstantExpression const &rhs) override
    {
        if (value == 0) {
            std::wcerr << "Division by zero" << std::endl;
            return nullptr;
        }
        if constexpr (std::is_integral_v<T>) {
            switch (rhs->type) {
            case SyntaxNodeType::Integer: {
                auto rhs_integer = std::dynamic_pointer_cast<Integer>(rhs);
                return make_node<Integer>(location + rhs->location, value % rhs_integer->value);
            }
            case SyntaxNodeType::SignedInteger: {
                auto rhs_integer = std::dynamic_pointer_cast<SignedInteger>(rhs);
                return make_node<SignedInteger>(location + rhs->location, value % rhs_integer->value);
            }
            case SyntaxNodeType::Decimal: {
                auto rhs_decimal = std::dynamic_pointer_cast<Decimal>(rhs);
                return make_node<Decimal>(location + rhs->location, fmod(value, rhs_decimal->value));
            }
            default:
                return nullptr;
            }
        } else {
            return evaluate_numeric_op(
                this, rhs,
                [](auto x, auto y) { return fmod(x, y); });
        }
    }

    pSyntaxNode evaluate_Equals(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x == y; });
    }

    pSyntaxNode evaluate_NotEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x != y; });
    }

    pSyntaxNode evaluate_Less(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x < y; });
    }

    pSyntaxNode evaluate_LessEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x <= y; });
    }

    pSyntaxNode evaluate_Greater(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x > y; });
    }

    pSyntaxNode evaluate_GreaterEqual(pConstantExpression const &rhs) override
    {
        return evaluate_comparison_op(
            this, rhs,
            [](auto x, auto y) { return x >= y; });
    }

    pSyntaxNode evaluate_BinaryAnd(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryAnd(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x & y; });
        }
    }

    pSyntaxNode evaluate_BinaryOr(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryOr(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x | y; });
        }
    }

    pSyntaxNode evaluate_BinaryXor(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryXor(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x ^ y; });
        }
    }

    pSyntaxNode evaluate_ShiftLeft(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_ShiftLeft(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x << y; });
        }
    }

    pSyntaxNode evaluate_ShiftRight(pConstantExpression const &rhs) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_ShiftRight(rhs);
        } else {
            return evaluate_binary_op(
                this, rhs,
                [](auto x, auto y) { return x >> y; });
        }
    }

    pSyntaxNode evaluate_Idempotent(pConstantExpression const &) override
    {
        return make_result(location, value);
    }

    pSyntaxNode evaluate_Negate(pConstantExpression const &) override
    {
        if constexpr (std::is_integral_v<T> && std::is_unsigned_v<T>) {
            if (value > std::numeric_limits<int64_t>::max()) {
                fatal("Cannot invert integer larger than int64_t::max");
            }
        }
        return make_result(location, -value);
    }

    pSyntaxNode evaluate_BinaryInvert(pConstantExpression const &) override
    {
        if constexpr (std::is_floating_point_v<T>) {
            return ConstantExpression::evaluate_BinaryInvert();
        } else {
            return make_result(location, ~value);
        }
    }
};

using Integer = Num<uint64_t, SyntaxNodeType::Integer>;
using SignedInteger = Num<int64_t, SyntaxNodeType::SignedInteger>;
using Decimal = Num<double, SyntaxNodeType::Decimal>;

}
