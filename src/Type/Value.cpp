/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <limits>
#include <optional>
#include <variant>

#include <Logging.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen {

Value::Value(TypeReference type, void *ptr)
    : m_type(type)
{
    auto t = TypeRegistry::the()[type];
    if (t.typespec.tag() == TypeKind::Primitive) {
        switch (type) {
        case VoidType:
            m_payload = std::monostate {};
            break;
#undef S
#define S(T, C)                                                                      \
    case T##Type:                                                                    \
        m_payload = C { (ptr) ? *(reinterpret_cast<C *>(ptr)) : static_cast<C>(0) }; \
        break;
            NumericTypes(S);
#undef S
        case BoolType:
            m_payload = bool {};
            break;
        case PtrType:
            m_payload = ptr;
            break;
        default:
            UNREACHABLE();
        }
    }
    if (t.typespec.tag() == TypeKind::Pointer) {
        m_payload = *(reinterpret_cast<void **>(ptr));
    }
    if (t.typespec.tag() == TypeKind::Slice) {
        if (ptr) {
            m_payload = *(reinterpret_cast<SliceValue *>(ptr));
        } else {
            m_payload = SliceValue {};
        }
    }
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload, other.m_payload);
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
        m_payload);
}

std::optional<Value> Value::negate() const
{
    return std::visit(
        overload {
            [](bool v) -> std::optional<Value> {
                return {};
            },
            [](u8 v) -> std::optional<Value> {
                if (v > std::numeric_limits<i8>::max()) {
                    return Value { -static_cast<i16>(v) };
                } else {
                    return Value { -static_cast<i8>(v) };
                }
            },
            [](u16 v) -> std::optional<Value> {
                if (v > std::numeric_limits<i16>::max()) {
                    return Value { -static_cast<i32>(v) };
                } else {
                    return Value { -static_cast<i16>(v) };
                }
            },
            [](u32 v) -> std::optional<Value> {
                if (v > std::numeric_limits<i32>::max()) {
                    return Value { -static_cast<i64>(v) };
                } else {
                    return Value { -static_cast<i32>(v) };
                }
            },
            [](u64 v) -> std::optional<Value> {
                if (v > std::numeric_limits<i64>::max()) {
                    fatal("Value too large to negate");
                } else {
                    return Value { -static_cast<i64>(v) };
                }
            },
            [](std::signed_integral auto v) -> std::optional<Value> {
                return Value { -v };
            },
            [](auto) -> std::optional<Value> {
                return {};
            } },
        m_payload);
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
        m_payload);
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
        m_payload);
}
}
