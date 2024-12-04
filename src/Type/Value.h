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
#include <concepts>
#include <cstdlib>
#include <format>
#include <limits>
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

    constexpr SliceValue() = default;
    constexpr SliceValue(SliceValue const &) = default;

    constexpr SliceValue(size_t len, char const *ptr)
        : len(len)
        , ptr(ptr)
    {
    }

    constexpr SliceValue(std::string_view const &sv)
        : len(sv.length())
        , ptr(sv.data())
    {
    }

    [[nodiscard]] constexpr bool operator<(SliceValue const &rhs) const
    {
        std::string_view lhs_sv { ptr, len };
        std::string_view rhs_sv { rhs.ptr, rhs.len };
        return lhs_sv < rhs_sv;
    }

    [[nodiscard]] constexpr bool operator==(SliceValue const &rhs) const
    {
        std::string_view lhs_sv { ptr, len };
        std::string_view rhs_sv { rhs.ptr, rhs.len };
        return lhs_sv == rhs_sv;
    }
};

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

using ValPayload = std::variant<
#undef S
#define S(T, L, ...) __VA_ARGS__,
    PrimitiveTypes(S)
#undef S
        SliceValue>;

struct Val : public ValPayload {
    Val() = default;
    Val(Val const &) = default;
#undef S
#define S(T, ...)                \
    constexpr Val(__VA_ARGS__ v) \
        : ValPayload(v)          \
    {                            \
    }
    ValueConstructors(S)
#undef S

        constexpr Val(SliceValue const &slice)
        : ValPayload(slice)
    {
    }

    constexpr Val(std::string_view v)
        : ValPayload(SliceValue { v })
    {
    }

    constexpr Val& operator=(Val const &other) = default;
    Val& operator=(struct Value const &other);

    [[nodiscard]] constexpr bool operator==(Val const &other) const
    {
        return std::visit(
            overload {
                [](std::integral auto lhs, std::integral auto rhs) {
                    return lhs == rhs;
                },
                [](std::floating_point auto lhs, std::floating_point auto rhs) {
                    return lhs == rhs;
                },
                [](auto *lhs, auto *rhs) {
                    return lhs == rhs;
                },
                [](auto lhs, auto rhs) -> bool {
                    UNREACHABLE();
                } },
            *this, other);
    }

    [[nodiscard]] constexpr bool operator<(Val const &other) const
    {
        return std::visit(
            overload {
                [](std::integral auto lhs, std::integral auto rhs) {
                    return lhs < rhs;
                },
                [](std::floating_point auto lhs, std::floating_point auto rhs) {
                    return lhs < rhs;
                },
                [](auto *lhs, auto *rhs) {
                    return lhs < rhs;
                },
                [](auto lhs, auto rhs) -> bool {
                    UNREACHABLE();
                } },
            *this, other);
    }

    Val(struct Value const &value);
};

using Vals = std::vector<Val>;
using ValuePayload = std::variant<Val, Vals>;

class Value {
public:
    constexpr static TypeReference SliceKind = static_cast<TypeReference>(PrimitiveType::ConstPtr) + 1;

    constexpr Value() = default;

    struct MakeArray { };
    constexpr static MakeArray make_array {};

#undef S
#define S(T, ...)                  \
    constexpr Value(__VA_ARGS__ v) \
        : m_type(T##Type)          \
    {                              \
        m_payload = Val { v };     \
    }
    ValueConstructors(S)
#undef S

        Value(TypeReference type)
        : m_type(type)
    {
        switch (type) {
        case NullType:
            m_payload = Val {};
            break;
        case I8Type:
            m_payload = Val { i8 {} };
            break;
        case U8Type:
            m_payload = Val { u8 {} };
            break;
        case I16Type:
            m_payload = Val { i16 {} };
            break;
        case U16Type:
            m_payload = Val { u16 {} };
            break;
        case I32Type:
            m_payload = Val { i32 {} };
            break;
        case U32Type:
            m_payload = Val { u32 {} };
            break;
        case I64Type:
            m_payload = Val { i64 {} };
            break;
        case U64Type:
            m_payload = Val { u64 {} };
            break;
        case BoolType:
            m_payload = Val { bool {} };
            break;
        case FloatType:
            m_payload = Val { f32 {} };
            break;
        case DoubleType:
            m_payload = Val { f64 {} };
            break;
        case PtrType:
            m_payload = Val { static_cast<void *>(nullptr) };
            break;
        case ConstPtrType:
            m_payload = Val { static_cast<void const *>(nullptr) };
            break;
        default:
            UNREACHABLE();
        }
    }

    Value(TypeReference type, Val const &v)
        : m_type(type)
        , m_payload(Val { v })
    {
    }

    Value(std::string_view v)
        : m_type(StringType)
    {
        m_payload = Val { v };
    }

    Value(SliceValue const &slice)
        : m_type(StringType)
    {
        m_payload = Val { slice };
    }

    Value(MakeArray const &, TypeReference type, size_t initial_size = 0)
        : m_type(type)
    {
        Vals vals {};
        vals.resize(initial_size, Value { type });
        m_payload = vals;
    }

    [[nodiscard]] constexpr TypeReference type() const
    {
        return m_type;
    }

    [[nodiscard]] constexpr size_t index() const
    {
        return m_payload.index();
    }

    [[nodiscard]] constexpr bool is_array() const
    {
        return m_payload.index() == 1;
    }

    [[nodiscard]] constexpr bool operator<(Value const &rhs) const
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

    [[nodiscard]] Val const &operator[](size_t ix) const
    {
        assert(is_array());
        auto const &values = std::get<Vals>(m_payload);
        assert(ix < values.size());
        return values[ix];
    }

    [[nodiscard]] Val &operator[](size_t ix)
    {
        assert(is_array());
        auto &values = std::get<Vals>(m_payload);
        assert(ix < values.size());
        return values[ix];
    }

    void push_back(Value const &value)
    {
        assert(is_array());
        std::get<Vals>(m_payload).push_back(value);
    }

    [[nodiscard]] Val const &back() const
    {
        assert(is_array());
        auto const &values = std::get<Vals>(m_payload);
        return values.back();
    }

    [[nodiscard]] Val &back()
    {
        assert(is_array());
        auto &values = std::get<Vals>(m_payload);
        return values.back();
    }

    [[nodiscard]] constexpr bool is_string() const
    {
        return type() == StringType;
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

    template <typename T>
    [[nodiscard]] constexpr T as() const
    {
        UNREACHABLE();
    }

    template <std::integral IntType>
    [[nodiscard]] constexpr IntType as() const
    {
        assert(m_payload.index() == 0);
        return std::visit(
            overload {
                [](std::integral auto v) {
                    if (v > std::numeric_limits<IntType>::max() || v < std::numeric_limits<IntType>::min()) {
                        fatal("Integer value {} cannot be converted to {}", v, typeid(IntType).name());
                    };
                    return static_cast<IntType>(v);
                },
                [](std::floating_point auto fp) {
                    if (fp > std::numeric_limits<IntType>::max() || fp < std::numeric_limits<IntType>::min()) {
                        fatal("Floating point value {} cannot be converted to {}", fp, typeid(IntType).name());
                    }
                    return static_cast<IntType>(fp);
                },
                [](auto v) -> IntType { fatal("Cannot convert Value of type {} to type {}", typeid(decltype(v)).name(), typeid(IntType).name()); return 0; },
            },
            std::get<Val>(m_payload));
    }

    [[nodiscard]] constexpr u64 as_signed() const
    {
        return as<i64>();
    }

    [[nodiscard]] constexpr u64 as_unsigned() const
    {
        return as<u64>();
    }

    template <std::floating_point FltType>
    [[nodiscard]] constexpr FltType as() const
    {
        assert(m_payload.index() == 0);
        return std::visit(
            overload {
                [](std::floating_point auto v) { return static_cast<FltType>(v); },
                [](std::integral auto v) { return static_cast<FltType>(v); },
                [](auto v) -> FltType { fatal("Cannot convert Value of type {} to type {}", typeid(decltype(v)).name(), typeid(FltType).name()); return 0.0; },
            },
            std::get<Val>(m_payload));
    }

    template<TypeReference Tag>
    constexpr std::variant_alternative_t<static_cast<std::size_t>(Tag), Val> get() const
    {
        return std::get<static_cast<std::size_t>(Tag)>(std::get<Val>(m_payload));
    }

    [[nodiscard]] std::vector<Val> const& values() const
    {
        assert(is_array());
        return std::get<Vals>(m_payload);
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

private:
    Value(TypeReference type, ValuePayload const &payload)
        : m_type(type)
        , m_payload(payload)
    {
    }

    TypeReference m_type { NullType };
    ValuePayload  m_payload {};

    friend struct Val;
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
        if (v.is_array()) {
            out << "[";
            for (size_t ix = 0; ix < v.values().size(); ++ix) {
                if (ix > 0) {
                    out << ", ";
                }
                out << std::format("{}", Arwen::Value { v.type(), v.values()[ix] });
            }
            out << "]";
            return std::ranges::copy(std::move(out).str(), ctx.out()).out;
        }
        if (v.type() != Arwen::StringType) {
            out << std::format("[{}] ", static_cast<Arwen::PrimitiveType>(v.type()));
        } else {
            out << "[String] ";
        }
        switch (v.type()) {
#undef S
#define S(T, L, ...)                                      \
    case Arwen::T##Type:                           \
        out << std::format("{}", v.value<__VA_ARGS__>()); \
        break;
        PrimitiveTypes(S)
#undef S
            case Arwen::StringType:
            out << std::format("{}", v.value<std::string_view>());
            break;
        default:
            fatal("Cannot format value of type [{}, {}]", v.type(), v.index());
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
