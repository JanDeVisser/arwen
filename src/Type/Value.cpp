/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <cstdint>
#include <optional>
#include <variant>

#include <Logging.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen {

// This crashes if Value is an array! So nested arrays don't work.
Val::Val(Value const &value)
    : Val(std::get<Val>(value.m_payload))
{
}

Val& Val::operator=(Value const &other)
{
    auto const &v = std::get<Val>(other.m_payload);
    if (index() == v.index()) {
        return *this = v;
    }
    if (index() >= I8Type && index() <= DoubleType) {
        switch (index()) {
            case I8Type: emplace<I8Type>(other.as<i8>()); return *this;
            case U8Type: emplace<U8Type>(other.as<u8>()); return *this;
            case I16Type: emplace<I16Type>(other.as<i16>()); return *this;
            case U16Type: emplace<U16Type>(other.as<u16>()); return *this;
            case I32Type: emplace<I32Type>(other.as<i32>()); return *this;
            case U32Type: emplace<U32Type>(other.as<u32>()); return *this;
            case I64Type: emplace<I64Type>(other.as<i64>()); return *this;
            case U64Type: emplace<U64Type>(other.as<u64>()); return *this;
            case FloatType: emplace<FloatType>(other.as<f32>()); return *this;
            case DoubleType: emplace<FloatType>(other.as<f64>()); return *this;
        }
    } else {
        std::println("Cannot assign to non-numeric type {}", index());
    }
    return *this;
}

std::optional<Value> Value::add(Value const &other) const
{
    return std::visit(
        overload {
            [](Numeric auto v1, Numeric auto v2) -> std::optional<Value> {
                return Value { v1 + v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::subtract(Value const &other) const
{
    return std::visit(
        overload {
            [](Numeric auto v1, Numeric auto v2) -> std::optional<Value> {
                return Value { v1 - v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::multiply(Value const &other) const
{
    return std::visit(
        overload {
            [](Numeric auto v1, Numeric auto v2) -> std::optional<Value> {
                return Value { v1 * v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::divide(Value const &other) const
{
    return std::visit(
        overload {
            [](Numeric auto v1, Numeric auto v2) -> std::optional<Value> {
                return Value { v1 / v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::modulo(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                return Value { v1 % v2 };
            },
            [](std::floating_point auto v1, std::floating_point auto v2) -> std::optional<Value> {
                return Value { fmod(v1, v2) };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::shl(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                Value ret { v1 << v2 };
                // std::println("{} {} << {} {} = {}", typeid(decltype(v1)).name(), v1, typeid(decltype(v2)).name(), v2, ret);
                return ret;
            },
            [](auto v1, auto v2) -> std::optional<Value> {
                std::println("Cannot shift non-integral types {} and {}", typeid(decltype(v1)).name(), typeid(decltype(v2)).name());
                return {};
            },
        },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::shr(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                Value ret { v1 >> v2 };
                // std::println("{} {} >> {} {} = {}", typeid(decltype(v1)).name(), v1, typeid(decltype(v2)).name(), v2, ret);
                return ret;
            },
            [](auto v1, auto v2) -> std::optional<Value> {
                std::println("Cannot shift non-integral types {} and {}", typeid(decltype(v1)).name(), typeid(decltype(v2)).name());
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::binary_or(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                Value ret { v1 | v2 };
                // std::println("{} {} | {} {} = {}", typeid(decltype(v1)).name(), v1, typeid(decltype(v2)).name(), v2, ret);
                return ret;
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::binary_and(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                Value ret { v1 & v2 };
                // std::println("{} {} & {} {} = {}", typeid(decltype(v1)).name(), v1, typeid(decltype(v2)).name(), v2, ret);
                return ret;
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::binary_xor(Value const &other) const
{
    return std::visit(
        overload {
            [](std::integral auto v1, std::integral auto v2) -> std::optional<Value> {
                return Value { v1 ^ v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::logical_or(Value const &other) const
{
    return std::visit(
        overload {
            [](bool v1, bool v2) -> std::optional<Value> {
                return Value { v1 || v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::logical_and(Value const &other) const
{
    return std::visit(
        overload {
            [](bool v1, bool v2) -> std::optional<Value> {
                return Value { v1 && v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::idempotent() const
{
    return std::visit(
        overload {
            [](Numeric auto v) -> std::optional<Value> {
                return Value { v };
            },
            [](auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload));
}

std::optional<Value> Value::negate() const
{
    return std::visit(
        overload {
            [](Numeric auto v) -> std::optional<Value> {
                return Value { -v };
            },
            [](auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload));
}

std::optional<Value> Value::logical_negate() const
{
    return std::visit(
        overload {
            [](bool v) -> std::optional<Value> {
                return Value { !v };
            },
            [](auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload));
}

std::optional<Value> Value::invert() const
{
    return std::visit(
        overload {
            [](int64_t v) -> std::optional<Value> {
                return Value { ~v };
            },
            [](auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload));
}
}
