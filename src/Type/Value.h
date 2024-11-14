/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cerrno>
#include <charconv>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <format>
#include <ios>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>

#include <Type/Type.h>

#include <Lib.h>
#include <Logging.h>

namespace Arwen {

template<typename T>
concept Numeric = std::is_arithmetic_v<T> && !std::is_same_v<T, bool> && !std::is_same_v<T, std::monostate>;

inline bool is_a(PrimitiveType concrete, BasicType abstract)
{
    switch (abstract) {
    case BasicType::Any:
        return true;
    case BasicType::Numeric:
        return concrete == PrimitiveType::Int || concrete == PrimitiveType::Float;
    case BasicType::Aggregate:
        return false;
    case BasicType::Self:
        UNREACHABLE();
    default:
        return static_cast<BasicType>(concrete) == abstract;
    }
}

class Value {
public:
    constexpr Value() = default;

    constexpr Value(Numeric auto val)
    {
        using T = std::decay_t<decltype(val)>;
        if (std::is_floating_point_v<T>) {
            m_type = PrimitiveType::Float;
            m_payload = static_cast<double>(val);
        } else if (std::is_integral_v<T>) {
            m_type = PrimitiveType::Int;
            m_payload = static_cast<int64_t>(val);
        } else {
            UNREACHABLE();
        }
    }

    constexpr Value(bool b)
        : m_type(PrimitiveType::Bool)
        , m_payload(b)
    {
    }

    [[nodiscard]] constexpr PrimitiveType type() const
    {
        return static_cast<PrimitiveType>(m_payload.index());
    }

    [[nodiscard]] constexpr bool operator<(Arwen::Value const &rhs) const
    {
        if (is_null()) {
            return !rhs.is_null();
        }
        return m_payload < rhs.m_payload;
    }

    [[nodiscard]] constexpr bool operator==(Value const &rhs) const
    {
        if (is_null()) {
            return rhs.is_null();
        }
        if (rhs.is_null()) {
            return false;
        }
        return !(*this < rhs) && !(rhs < *this);
    }

    [[nodiscard]] constexpr bool is_null() const
    {
        return type() == PrimitiveType::Null;
    }

    [[nodiscard]] constexpr bool is_bool() const
    {
        return type() == PrimitiveType::Bool;
    }

    [[nodiscard]] constexpr bool is_int() const
    {
        return type() == PrimitiveType::Int;
    }

    [[nodiscard]] constexpr bool is_float() const
    {
        return type() == PrimitiveType::Float;
    }

    [[nodiscard]] constexpr bool as_bool() const
    {
        return std::get<bool>(m_payload);
    }

    [[nodiscard]] constexpr int64_t as_int() const
    {
        return std::get<int64_t>(m_payload);
    }

    [[nodiscard]] constexpr double as_float() const
    {
        return (is_float()) ? std::get<double>(m_payload) : static_cast<double>(as_int());
    }

    [[nodiscard]] constexpr bool is_numeric() const
    {
        return is_int() || is_float();
    }

    [[nodiscard]] constexpr bool is_truthy() const
    {
        if (is_bool()) {
            return as_bool();
        }
        if (is_numeric()) {
            return as_int() != 0;
        }
        return false;
    }

    [[nodiscard]] constexpr bool is_falsy() const
    {
        return !is_truthy();
    }

    [[nodiscard]] constexpr std::optional<Value> add(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> subtract(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> multiply(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> divide(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> modulo(Value const &other) const
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
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> shl(Value const &other) const
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
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> shr(Value const &other) const
    {
        return std::visit(
            overload {
                [](int64_t v1, int64_t v2) -> std::optional<Value> {
                    return Value { v1 >> v2 };
                },
                [](auto, auto) -> std::optional<Value> {
                    return {};
                } },
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> binary_or(Value const &other) const
    {
        return std::visit(
            overload {
                [](int64_t v1, int64_t v2) -> std::optional<Value> {
                    return Value { v1 | v2 };
                },
                [](auto, auto) -> std::optional<Value> {
                    return {};
                } },
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> binary_and(Value const &other) const
    {
        return std::visit(
            overload {
                [](int64_t v1, int64_t v2) -> std::optional<Value> {
                    return Value { v1 & v2 };
                },
                [](auto, auto) -> std::optional<Value> {
                    return {};
                } },
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> binary_xor(Value const &other) const
    {
        return std::visit(
            overload {
                [](int64_t v1, int64_t v2) -> std::optional<Value> {
                    return Value { v1 ^ v2 };
                },
                [](auto, auto) -> std::optional<Value> {
                    return {};
                } },
            m_payload, other.m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> logical_or(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> logical_and(Value const &other) const
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

    [[nodiscard]] constexpr std::optional<Value> idempotent() const
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

    [[nodiscard]] constexpr std::optional<Value> negate() const
    {
        return std::visit(
            overload {
                [](Numeric auto v) -> std::optional<Value> {
                    return Value { -v };
                },
                [](auto) -> std::optional<Value> {
                    return {};
                } },
            m_payload);
    }

    [[nodiscard]] constexpr std::optional<Value> logical_negate() const
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

    [[nodiscard]] constexpr std::optional<Value> invert() const
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

private:
    PrimitiveType              m_type { PrimitiveType::Null };
    std::optional<std::string> m_str {};
    std::variant<
#undef S
#define S(T, L, ...) __VA_ARGS__,
        PrimitiveTypes(S)
            std::monostate>
        m_payload {};
};

template<>
inline std::optional<Value> decode(std::string_view s, ...)
{
    if (auto col_ix = s.find(':'); col_ix != std::string_view::npos) {
        auto m_type_str = s.substr(0, col_ix);
        auto val_str = s.substr(col_ix + 1);
        if (auto m_type = decode<PrimitiveType>(m_type_str); m_type) {
            switch (*m_type) {
            case PrimitiveType::Bool:
                return Value { iequals(val_str, "true") };
            case PrimitiveType::Int: {
                int64_t result {};
                auto [ptr, ec] = std::from_chars(val_str.data(), val_str.data() + val_str.size(), result);
                if (ec != std::errc()) {
                    return {};
                }
                return Value { result };
            }
            case PrimitiveType::Float: {
                char  *end;
                double flt = std::strtod(val_str.data(), &end);
                if (errno == ERANGE || (flt == 0 && end == val_str.data())) {
                    return {};
                }
                return Value { flt };
            }
            default:
                UNREACHABLE();
            }
        }
    }
    return {};
}

}

template<>
struct std::formatter<Arwen::Value, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Value const &v, FmtContext &ctx) const
    {
        std::ostringstream out;
        switch (v.type()) {
        case Arwen::PrimitiveType::Null:
            break;
        case Arwen::PrimitiveType::Bool:
            out << ios::boolalpha << v.as_bool();
            break;
        case Arwen::PrimitiveType::Int:
            out << v.as_int();
            break;
        case Arwen::PrimitiveType::Float:
            out << v.as_float();
            break;
        default:
            UNREACHABLE();
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
