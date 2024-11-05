/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <charconv>

#include <Lib.h>

namespace Arwen {

#define ValueTypes(S) \
    S(Null)           \
    S(Bool)           \
    S(Int)            \
    S(Float)          \
    S(String)

enum class ValueType {
#undef S
#define S(T) T,
    ValueTypes(S)
#undef S
};

template<>
inline std::optional<ValueType> decode(std::string_view s, ...)
{
#undef S
#define S(T)            \
    if (iequals(s, #T)) \
        return ValueType::T;
    ValueTypes(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(ValueType const &v)
{
    switch (v) {
#undef S
#define S(T)           \
    case ValueType::T: \
        return #T;
        ValueTypes(S)
#undef S
    }
}

struct Value {
    ValueType type;
    union {
        bool             bool_value = false;
        uint64_t         int_value;
        double           float_value;
        std::string_view string;
    } payload;

    [[nodiscard]] constexpr bool operator<(Arwen::Value const &rhs) const
    {
        if (type != rhs.type) {
            return static_cast<int>(type) - static_cast<int>(rhs.type);
        }
        switch (type) {
        case ValueType::Null:
            return false;
        case ValueType::Bool:
            return payload.bool_value < rhs.payload.bool_value;
        case ValueType::Int:
            return payload.int_value < rhs.payload.int_value;
        case ValueType::Float:
            return payload.float_value < rhs.payload.float_value;
        case ValueType::String:
            return payload.string < rhs.payload.string;
        }
    }

    [[nodiscard]] constexpr bool operator==(Value const &rhs) const
    {
        return !(*this < rhs) && !(rhs < *this);
    }
};

template<>
inline std::optional<Value> decode(std::string_view s, ...)
{
    if (auto col_ix = s.find(':'); col_ix != std::string_view::npos) {
        auto type_str = s.substr(0, col_ix);
        auto val_str = s.substr(col_ix + 1);
        if (auto type = decode<ValueType>(type_str); type) {
            switch (*type) {
            case ValueType::Bool:
                return Value { .type = *type, .payload = { .bool_value = iequals(val_str, "true") } };
            case ValueType::Int: {
                uint64_t result {};
                auto [ptr, ec] = std::from_chars(val_str.data(), val_str.data() + val_str.size(), result);
                if (ec != std::errc()) {
                    return {};
                }
                return Value { .type = *type, .payload = { .int_value = result } };
            }
            case ValueType::Float: {
                char  *end;
                double flt = std::strtod(val_str.data(), &end);
                if (errno == ERANGE || (flt == 0 && end == val_str.data())) {
                    return {};
                }
                return Value { .type = *type, .payload = { .float_value = flt } };
            }
            default:
                return Value { .type = *type };
            }
        }
    }
    return {};
}

}

template<>
struct std::formatter<Arwen::ValueType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::ValueType const &k, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(k);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Value, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Value const &v, FmtContext &ctx) const
    {
        std::ostringstream out;
        switch (v.type) {
        case Arwen::ValueType::Null:
            break;
        case Arwen::ValueType::Bool:
            out << ((v.payload.bool_value) ? "true" : "false");
            break;
        case Arwen::ValueType::Int:
            out << v.payload.int_value;
            break;
        case Arwen::ValueType::Float:
            out << v.payload.float_value;
            break;
        case Arwen::ValueType::String:
            out << v.payload.string;
            break;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
