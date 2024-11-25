/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <optional>
#include <variant>

#include <Logging.h>
#include <Type/Value.h>

namespace Arwen {

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
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
                return Value { v1 % v2 };
            },
            [](Numeric auto v1, Numeric auto v2) -> std::optional<Value> {
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
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
                return Value { v1 << v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            },
        },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::shr(Value const &other) const
{
    return std::visit(
        overload {
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
                return Value { v1 >> v2 };
            },
            [](auto, auto) -> std::optional<Value> {
                return {};
            } },
        std::get<Val>(m_payload), std::get<Val>(other.m_payload));
}

std::optional<Value> Value::binary_or(Value const &other) const
{
    return std::visit(
        overload {
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
                return Value { v1 | v2 };
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
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
                return Value { v1 & v2 };
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
            [](int64_t v1, int64_t v2) -> std::optional<Value> {
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
