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
#include <cstring>
#include <format>
#include <ios>
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

namespace Arwen {

template<typename T>
concept Numeric = std::is_arithmetic_v<T> && !std::is_same_v<T, bool> && !std::is_same_v<T, std::monostate>;

struct SliceValue {
    size_t len;
    u8    *ptr;

    constexpr SliceValue() = default;
    constexpr SliceValue(SliceValue const &) = default;

    constexpr SliceValue(u64 len, u8 *ptr, u64 element_size = 1)
        : len(len)
        , ptr(ptr)
    {
    }

    constexpr SliceValue(std::string_view const &sv)
        : len(sv.length())
        , ptr(static_cast<u8 *>(static_cast<void *>(const_cast<char *>(sv.data()))))
    {
    }

    [[nodiscard]] constexpr bool operator==(SliceValue const &other) const
    {
        return len == other.len && ptr == other.ptr;
    }

    [[nodiscard]] constexpr bool operator<(SliceValue const &other) const
    {
        if (len != other.len) {
            return len < other.len;
        }
        return ptr < other.ptr;
    }

    std::string_view as_sv() const
    {
        return { const_cast<char const *>(static_cast<char *>(static_cast<void *>(ptr))), len };
    }
};

template<>
inline TypeReference type_of<SliceValue>() { return StringType; }

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
    S(Ptr, std::integral auto *) \
    S(String, SliceValue)        \
    S(String, std::string_view)

using ValuePayload = std::variant<
#undef S
#define S(T, L, ...) __VA_ARGS__,
    PrimitiveTypes(S)
#undef S
        SliceValue>;

class Value {
public:
    constexpr Value() = default;

#undef S
#define S(T, ...)                  \
    constexpr Value(__VA_ARGS__ v) \
        : m_type(T##Type)          \
        , m_payload(v)             \
    {                              \
    }
    ValueConstructors(S)
#undef S

#undef S
#define S(T, CType)       \
    Value(CType *ptr)     \
        : m_type(T##Type) \
        , m_payload(*ptr) \
    {                     \
    }
        NumericTypes(S)
#undef S

            Value(TypeReference type, void *ptr = nullptr);

    [[nodiscard]] constexpr TypeReference type() const
    {
        return m_type;
    }

    [[nodiscard]] constexpr size_t index() const
    {
        return m_payload.index();
    }

    constexpr Value &operator=(Value const &other) = default;

    [[nodiscard]] constexpr bool operator==(Value const &other) const
    {
        if (type() != other.type()) {
            return type() - other.type();
        }
        return std::visit(
            [&](auto lhs) -> bool {
                using LHS = decltype(lhs);
                return std::visit(
                    overload {
                        [&](std::same_as<LHS> auto rhs) {
                            return lhs == rhs;
                        },
                        [](auto rhs) -> bool {
                            UNREACHABLE();
                        } },
                    other.m_payload);
            },
            this->m_payload);
    }

    [[nodiscard]] constexpr bool operator<(Value const &other) const
    {
        if (type() != other.type()) {
            return type() - other.type();
        }
        return std::visit(
            [&](auto lhs) -> bool {
                using LHS = decltype(lhs);
                return std::visit(
                    overload {
                        [&](std::same_as<LHS> auto rhs) {
                            return lhs < rhs;
                        },
                        [](auto rhs) -> bool {
                            UNREACHABLE();
                        } },
                    other.m_payload);
            },
            this->m_payload);
    }

    [[nodiscard]] constexpr bool is_string() const
    {
        return type() == StringType;
    }

    [[nodiscard]] constexpr bool is_void() const
    {
        return type() == VoidType;
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
        return std::get<bool>(m_payload);
    }

    template<typename T>
    [[nodiscard]] constexpr T as() const
    {
        UNREACHABLE();
    }

    template<std::signed_integral IntType>
    [[nodiscard]] constexpr IntType as() const
    {
        return std::visit(
            overload {
                [](std::signed_integral auto v) {
                    if (v > std::numeric_limits<IntType>::max() || v < std::numeric_limits<IntType>::min()) {
                        fatal("Integer value {} cannot be converted to {}", v, typeid(IntType).name());
                    };
                    return static_cast<IntType>(v);
                },
                [](std::unsigned_integral auto v) {
                    if (v > std::numeric_limits<IntType>::max()) {
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
            m_payload);
    }

    template<std::unsigned_integral IntType>
    [[nodiscard]] constexpr IntType as() const
    {
        return std::visit(
            overload {
                [](std::signed_integral auto v) {
                    if (v > std::numeric_limits<IntType>::max()) {
                        fatal("Integer value {} cannot be converted to {}", v, typeid(IntType).name());
                    };
                    return static_cast<IntType>(v);
                },
                [](std::unsigned_integral auto v) {
                    if (v < 0 || v > std::numeric_limits<IntType>::max()) {
                        fatal("Integer value {} cannot be converted to {}", v, typeid(IntType).name());
                    };
                    return static_cast<IntType>(v);
                },
                [](std::floating_point auto fp) {
                    if (fp > std::numeric_limits<IntType>::max() || fp < 0.0) {
                        fatal("Floating point value {} cannot be converted to {}", fp, typeid(IntType).name());
                    }
                    return static_cast<IntType>(fp);
                },
                [](auto v) -> IntType { fatal("Cannot convert Value of type {} to type {}", typeid(decltype(v)).name(), typeid(IntType).name()); return 0; },
            },
            m_payload);
    }

    [[nodiscard]] constexpr u64 as_signed() const
    {
        return as<i64>();
    }

    [[nodiscard]] constexpr u64 as_unsigned() const
    {
        return as<u64>();
    }

    template<std::floating_point FltType>
    [[nodiscard]] constexpr FltType as() const
    {
        assert(m_payload.index() != 0);
        return std::visit(
            overload {
                [](std::floating_point auto v) { return static_cast<FltType>(v); },
                [](std::integral auto v) { return static_cast<FltType>(v); },
                [](auto v) -> FltType { fatal("Cannot convert Value of type {} to type {}", typeid(decltype(v)).name(), typeid(FltType).name()); return 0.0; },
            },
            m_payload);
    }

    template<TypeReference Tag>
    constexpr std::variant_alternative_t<static_cast<std::size_t>(Tag), ValuePayload> get() const
    {
        return std::get<static_cast<std::size_t>(Tag)>(m_payload);
    }

    template<typename T>
    [[nodiscard]] constexpr T value() const
    {
        if constexpr (std::is_same_v<T, std::monostate>) {
            return std::get<static_cast<std::size_t>(VoidType)>(m_payload);
        }
        if constexpr (std::is_same_v<T, bool>) {
            return std::get<static_cast<std::size_t>(BoolType)>(m_payload);
        }
        if constexpr (std::is_same_v<T, u8>) {
            return std::get<static_cast<std::size_t>(U8Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, i8>) {
            return std::get<static_cast<std::size_t>(I8Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, u16>) {
            return std::get<static_cast<std::size_t>(U16Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, i16>) {
            return std::get<static_cast<std::size_t>(I16Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, u32>) {
            return std::get<static_cast<std::size_t>(U32Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, i32>) {
            return std::get<static_cast<std::size_t>(I32Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, u64>) {
            return std::get<static_cast<std::size_t>(U64Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, i64>) {
            return std::get<static_cast<std::size_t>(I64Type)>(m_payload);
        }
        if constexpr (std::is_same_v<T, double>) {
            return std::get<static_cast<std::size_t>(DoubleType)>(m_payload);
        }
        if constexpr (std::is_same_v<T, float>) {
            return std::get<static_cast<std::size_t>(FloatType)>(m_payload);
        }
        if constexpr (std::is_same_v<T, void *>) {
            return std::get<static_cast<std::size_t>(PtrType)>(m_payload);
        }
        if constexpr (std::is_same_v<T, char *>) {
            return static_cast<char *>(std::get<static_cast<std::size_t>(PtrType)>(m_payload));
        }
        if constexpr (std::is_same_v<T, std::string_view>) {
            auto const &slice = std::get<static_cast<std::size_t>(StringType)>(m_payload);
            return slice.as_sv();
        }
        UNREACHABLE();
    }

    void copy_value(u8 *dest) const
    {
        void *ptr = dest;
        *dest = 0;
        switch (type()) {
        case VoidType:
            break;
#undef S
#define S(T, C)                                                  \
    case T##Type: {                                              \
        *(reinterpret_cast<C *>(dest)) = std::get<C>(m_payload); \
    } break;
            NumericTypes(S);
#undef S
        case BoolType: {
            *(reinterpret_cast<bool *>(dest)) = std::get<bool>(m_payload);
        } break;
        case StringType: {
            *(reinterpret_cast<SliceValue *>(dest)) = std::get<SliceValue>(m_payload);
        } break;
        default:
            UNREACHABLE();
        }
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

    TypeReference m_type { VoidType };
    ValuePayload  m_payload {};
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

struct TypedSlice {
    TypeReference type;
    SliceValue    value;

    u64       length() const { return value.len; }
    u8 const *ptr() const { return value.ptr; }

    Value at(size_t ix) const
    {
        auto t = TypeRegistry::the()[type];
        return Value { type, value.ptr + ix * t.size() };
    }

    void set(size_t ix, Value const &v)
    {
        auto t = TypeRegistry::the()[type];
        v.copy_value(value.ptr + ix * t.size());
    }

    [[nodiscard]] constexpr bool operator<(TypedSlice const &rhs) const
    {
        if (type != rhs.type) {
            return type < rhs.type;
        }
        if (ptr() == rhs.ptr()) {
            return length() < rhs.length();
        }
        auto t = TypeRegistry::the()[type];
        u64  element_size = t.size();
        for (size_t ix = 0; ix < std::min(length(), rhs.length()); ix++) {
            if (auto cmp = memcmp(ptr() + element_size * ix, rhs.ptr() + element_size * ix, element_size); cmp) {
                return cmp < 0;
            }
        }
        return false;
    }

    [[nodiscard]] constexpr bool operator==(TypedSlice const &rhs) const
    {
        if (type != rhs.type) {
            return false;
        }
        if (ptr() == rhs.ptr()) {
            return length() == rhs.length();
        }
        auto t = TypeRegistry::the()[type];
        u64  element_size = t.size();
        for (size_t ix = 0; ix < std::min(length(), rhs.length()); ix++) {
            if (auto cmp = memcmp(ptr() + ix, rhs.ptr() + ix, element_size); cmp) {
                return false;
            }
        }
        return true;
    }
};

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
        out << std::format("[{}] ", Arwen::TypeRegistry::the()[v.type()]);
        switch (v.type()) {
#undef S
#define S(T, L, ...)                                      \
    case Arwen::T##Type:                                  \
        out << std::format("{}", v.value<__VA_ARGS__>()); \
        break;
        PrimitiveTypes(S)
#undef S
            case Arwen::StringType:
            out << std::format("{}", v.value<std::string_view>());
            break;
        default: {
            auto t = Arwen::TypeRegistry::the()[v.type()];
            if (t.typespec.tag() == Arwen::TypeKind::Pointer && t.typespec.get<Arwen::TypeKind::Pointer>().element_type == Arwen::U8Type) {
                out << std::hex << "0x" << reinterpret_cast<u64>(v.value<void *>()) << std::dec;
            } else {
                out << "xx";
            }
        } break;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
