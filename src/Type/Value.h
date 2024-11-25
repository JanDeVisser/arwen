/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cerrno>
#include <charconv>
#include <cstdlib>
#include <format>
#include <optional>
#include <ostream>
#include <sstream>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>
#include <variant>

#include <Type/Type.h>

#include <Lib.h>
#include <Logging.h>
#include <SimpleFormat.h>
#include <vector>

namespace Arwen {

template<typename T>
concept Numeric = std::is_arithmetic_v<T> && !std::is_same_v<T, bool> && !std::is_same_v<T, std::monostate>;

struct SliceValue {
    size_t      len;
    char const *ptr;

    SliceValue(size_t len, char const *ptr)
        : len(len)
        , ptr(ptr)
    {
    }

    SliceValue(std::string_view const &sv)
        : len(sv.length())
        , ptr(sv.data())
    {
    }

    [[nodiscard]] bool operator<(SliceValue const &rhs) const
    {
        std::string_view lhs_sv { ptr, len };
        std::string_view rhs_sv { rhs.ptr, rhs.len };
        return lhs_sv < rhs_sv;
    }

    [[nodiscard]] bool operator==(SliceValue const &rhs) const
    {
        std::string_view lhs_sv { ptr, len };
        std::string_view rhs_sv { rhs.ptr, rhs.len };
        return lhs_sv == rhs_sv;
    }
};

using Val = std::variant<
#undef S
#define S(T, L, ...) __VA_ARGS__,
    PrimitiveTypes(S)
#undef S
        SliceValue>;

using Vals = std::vector<Val>;

class Value {
public:
#undef S
#define S(T, L, ...) constexpr static TypeReference T##Type = static_cast<TypeReference>(PrimitiveType::T);
    PrimitiveTypes(S)
#undef S
        constexpr static TypeReference StringType
        = static_cast<TypeReference>(BuiltinType::String);
    constexpr static TypeReference IntType = static_cast<TypeReference>(BuiltinType::Int);
    constexpr static TypeReference SliceKind = static_cast<TypeReference>(PrimitiveType::ConstPtr) + 1;

    constexpr Value() = default;

#define ValueConstructors(S)     \
    S(Bool, bool)                \
    S(Double, f64)               \
    S(Float, f32)                \
    S(U8, u8)                    \
    S(I8, i8)                    \
    S(U16, u16)                  \
    S(I16, i16)                  \
    S(U32, u32)                  \
    S(I32, i32)                  \
    S(U64, u64)                  \
    S(I64, i64)                  \
    S(Ptr, void *)               \
    S(ConstPtr, void const *)    \
    S(Ptr, std::integral auto *) \
    S(ConstPtr, std::integral auto const *)

#undef S
#define S(T, ...)                  \
    constexpr Value(__VA_ARGS__ v) \
        : m_type(T##Type)          \
    {                              \
        m_payload = Val { v };     \
    }
    ValueConstructors(S)
#undef S

        Value(std::string_view v)
        : m_type(StringType)
    {
        m_payload = Val { SliceValue { v } };
    }

    [[nodiscard]] constexpr TypeReference type() const
    {
        return m_type;
    }

    [[nodiscard]] constexpr size_t index() const
    {
        return m_payload.index();
    }

    [[nodiscard]] constexpr bool operator<(Arwen::Value const &rhs) const
    {
        return std::visit(
            overload {
                [](Val const &lhs, Val const &rhs) {
                    return lhs < rhs;
                },
                [](Vals const &lhs, Vals const &rhs) {
                    return lhs < rhs;
                },
                [](auto const &, auto const &) {
                    return false;
                } },
            m_payload, rhs.m_payload);
    }

    [[nodiscard]] constexpr bool operator==(Value const &rhs) const
    {
        return std::visit(
            overload {
                [](Val const &lhs, Val const &rhs) {
                    return lhs == rhs;
                },
                [](Vals const &lhs, Vals const &rhs) {
                    return lhs == rhs;
                },
                [](auto const &, auto const &) {
                    return false;
                },
            },
            m_payload, rhs.m_payload);
    }

    [[nodiscard]] constexpr bool is_null() const
    {
        return type() == NullType;
    }

    [[nodiscard]] constexpr bool is_bool() const
    {
        return type() == BoolType;
    }

    [[nodiscard]] constexpr bool is_int() const
    {
        return type() >= U8Type && type() <= I64Type;
    }

    [[nodiscard]] constexpr bool is_float() const
    {
        return type() == FloatType || type() == DoubleType;
    }

    [[nodiscard]] constexpr bool as_bool() const
    {
        return std::get<bool>(std::get<Val>(m_payload));
    }

    template<TypeReference Tag>
    constexpr std::variant_alternative_t<static_cast<std::size_t>(Tag), Val> get() const
    {
        return std::get<static_cast<std::size_t>(Tag)>(std::get<Val>(m_payload));
    }

    template<typename T>
    [[nodiscard]] constexpr T value() const
    {
        if constexpr (std::is_same_v<T, bool>) {
            return std::get<static_cast<std::size_t>(BoolType)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, u8>) {
            return std::get<static_cast<std::size_t>(U8Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, i8>) {
            return std::get<static_cast<std::size_t>(I8Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, u16>) {
            return std::get<static_cast<std::size_t>(U16Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, i16>) {
            return std::get<static_cast<std::size_t>(I16Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, u32>) {
            return std::get<static_cast<std::size_t>(U32Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, i32>) {
            return std::get<static_cast<std::size_t>(I32Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, u64>) {
            return std::get<static_cast<std::size_t>(U64Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, i64>) {
            return std::get<static_cast<std::size_t>(I64Type)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, double>) {
            return std::get<static_cast<std::size_t>(DoubleType)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, float>) {
            return std::get<static_cast<std::size_t>(FloatType)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, void *>) {
            return std::get<static_cast<std::size_t>(PtrType)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, void const *>) {
            return std::get<static_cast<std::size_t>(ConstPtrType)>(std::get<Val>(m_payload));
        }
        if constexpr (std::is_same_v<T, std::string_view>) {
            auto const &slice = std::get<static_cast<std::size_t>(SliceKind)>(std::get<Val>(m_payload));
            return { slice.ptr, slice.len };
        }
        UNREACHABLE();
    }

    [[nodiscard]] constexpr bool is_numeric() const
    {
        return is_int() || is_float();
    }

    [[nodiscard]] std::optional<Value> add(Value const &other) const;
    [[nodiscard]] std::optional<Value> subtract(Value const &other) const;
    [[nodiscard]] std::optional<Value> multiply(Value const &other) const;
    [[nodiscard]] std::optional<Value> divide(Value const &other) const;
    [[nodiscard]] std::optional<Value> modulo(Value const &other) const;
    [[nodiscard]] std::optional<Value> shl(Value const &other) const;
    [[nodiscard]] std::optional<Value> shr(Value const &other) const;
    [[nodiscard]] std::optional<Value> binary_or(Value const &other) const;
    [[nodiscard]] std::optional<Value> binary_and(Value const &other) const;
    [[nodiscard]] std::optional<Value> binary_xor(Value const &other) const;
    [[nodiscard]] std::optional<Value> logical_or(Value const &other) const;
    [[nodiscard]] std::optional<Value> logical_and(Value const &other) const;
    [[nodiscard]] std::optional<Value> idempotent() const;
    [[nodiscard]] std::optional<Value> negate() const;
    [[nodiscard]] std::optional<Value> logical_negate() const;
    [[nodiscard]] std::optional<Value> invert() const;

    using Payload = std::variant<Val, Vals>;

private:
    TypeReference m_type { NullType };
    Payload       m_payload {};
};

template<typename T>
std::optional<Value> parse(std::string_view val_str)
{
    UNREACHABLE();
}

template<std::floating_point T = double>
auto parse(std::string_view val_str) -> std::optional<Value>
{
    char *end;
    T     result = std::strtod(val_str.data(), &end);
    if (errno == ERANGE || (result == 0 && end == val_str.data())) {
        return {};
    }
    return Value { result };
};

template<std::integral T = i64>
std::optional<Value> parse(std::string_view val_str)
{
    T result;
    auto [ptr, ec] = std::from_chars(val_str.data(), val_str.data() + val_str.size(), result);
    if (ec != std::errc()) {
        return {};
    }
    return Value { result };
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
            case PrimitiveType::U8:
                return parse<u8>(val_str);
            case PrimitiveType::I8:
                return parse<i8>(val_str);
            case PrimitiveType::U16:
                return parse<u16>(val_str);
            case PrimitiveType::I16:
                return parse<i16>(val_str);
            case PrimitiveType::U32:
                return parse<u32>(val_str);
            case PrimitiveType::I32:
                return parse<i32>(val_str);
            case PrimitiveType::U64:
                return parse<u64>(val_str);
            case PrimitiveType::I64:
                return parse<i64>(val_str);
            case PrimitiveType::Double:
                return parse<f64>(val_str);
            case PrimitiveType::Float:
                return parse<f32>(val_str);
            default:
                UNREACHABLE();
            }
        }
    }
    return {};
}
}

inline std::ostream &operator<<(std::ostream &os, std::monostate const &)
{
    os << "{null}";
    return os;
}

template<>
struct std::formatter<std::monostate, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(std::monostate const &v, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << "{null}";
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Value, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Value const &v, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("[{}, {}] ", v.type(), v.index());
        switch (v.type()) {
#undef S
#define S(T, L, ...)                                      \
    case Arwen::Value::T##Type:                           \
        out << std::format("{}", v.value<__VA_ARGS__>()); \
        break;
        PrimitiveTypes(S)
#undef S
            case Arwen::Value::StringType:
            out << std::format("{}", v.value<std::string_view>());
            break;
        default:
            fatal("Cannot format value of type [{}, {}]", v.type(), v.index());
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
