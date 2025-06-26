/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <concepts>
#include <cstdint>
#include <cxxabi.h>
#include <string>
#include <variant>

#include <Util/Arena.h>
#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/Type.h>

namespace Arwen {

using namespace Util;

struct Atom {
    std::variant<
        bool,
#undef S
#define S(W) int##W##_t, uint##W##_t,
        BitWidths(S)
#undef S
#define S(W, T) T,
            FloatBitWidths(S)
#undef S
                Slice,
        DynamicArray,
        StaticArray,
        void *>
        payload;

    explicit Atom(std::integral auto val)
    {
        payload = val;
    }

    explicit Atom(std::floating_point auto val)
    {
        payload = val;
    }

    explicit Atom(bool val)
    {
        payload = val;
    }

    explicit Atom(Slice const &slice)
    {
        payload = slice;
    }

    explicit Atom(DynamicArray const &dyn_array)
    {
        payload = dyn_array;
    }

    explicit Atom(StaticArray const &static_array)
    {
        payload = static_array;
    }

    template<typename T>
    explicit Atom(T *val)
    {
        payload = (void *) val;
    }

    Atom &operator=(Atom const &) = default;
    bool  operator==(Atom const &) const = default;

    [[nodiscard]] pType               type() const;
    [[nodiscard]] std::optional<Atom> coerce(pType const &to_type) const;
    [[nodiscard]] std::wstring        to_string() const;
};

template<typename T>
T const &as(Atom const &atom)
{
    int         status;
    char const *mangled { typeid(T).name() };
    char       *demangled = abi::__cxa_demangle(mangled, 0, 0, &status);
    return std::get<T>(atom.payload);
}

template<typename T>
T &as(Atom &atom)
{
    return std::get<T>(atom.payload);
}

inline void *address_of(Atom &atom)
{
    return std::visit(
        [](auto &val) -> void * {
            return &val;
        },
        atom.payload);
}

// using Atoms = std::vector<Atom>;

Atom evaluate(Atom const &lhs, Operator op, Atom const &rhs);
Atom evaluate(Operator op, Atom const &operand);

struct Value {
    static Arena arena;
    pType        type { TypeRegistry::void_ };
    using Values = std::vector<Value>;
    std::variant<std::monostate, Atom, Values> payload;

    Value() = default;
    Value(Value const &) = default;

    Value(Value &other);
    explicit Value(pType const &type);
    Value(int8_t val);
    Value(uint8_t val);
    Value(int16_t val);
    Value(uint16_t val);
    Value(int32_t val);
    Value(uint32_t val);
    Value(int64_t val);
    Value(uint64_t val);
    Value(float val);
    Value(double val);
    Value(bool val);
    Value(void *val);
    Value(Atom atom);

    template<typename T>
    Value(pType const &type, T val)
        : Value(type, Atom { val })
    {
    }

    template<>
    Value(pType const &type, Atom val)
        : type(type)
        , payload(val)
    {
    }

    template<>
    Value(pType const &type, Values values)
        : type(type)
        , payload(values)
    {
    }

    // Value &operator=(Value const &) = default;

    [[nodiscard]] std::optional<Value> coerce(pType const &to_type) const;
    [[nodiscard]] std::wstring         to_string() const;

    std::wstring as_wstring() const
    {
        return std::visit(
            overloads {
                [](std::monostate const &) -> std::wstring {
                    fatal("Cannot convert empty value to std::wstring");
                },
                [](Atom const &atom) -> std::wstring {
                    auto const &[ptr, size] = as<Slice>(atom);
                    return { static_cast<wchar_t *>(ptr), static_cast<size_t>(size) };
                },
                [](Values const &values) -> std::wstring {
                    UNREACHABLE();
                } },
            payload);
    }
};

template<typename T>
T &as(Value &val)
{
    return std::visit(
        overloads {
            [](std::monostate &) -> T & {
                fatal("Cannot convert empty value to type `{}`", typeid(T).name());
            },
            [](Atom &atom) -> T & {
                return as<T>(atom);
            },
            [](Value::Values &) -> T & {
                UNREACHABLE();
            } },
        val.payload);
}

template<typename T>
T const &as(Value const &val)
{
    return std::visit(
        overloads {
            [](std::monostate const &) -> T const & {
                fatal("Cannot convert empty value to type `{}`", typeid(T).name());
            },
            [](Atom const &atom) -> T const & {
                return as<T>(atom);
            },
            [](Value::Values const &) -> T const & {
                UNREACHABLE();
            } },
        val.payload);
}

void  *address_of(Value &val);
pType  type_of(Value &val, std::vector<uint64_t> path = {});
void  *address_of(Value &val, uint64_t field);
Value &subvalue_of(Value &val, uint64_t ix);
void  *address_of(Value &val, std::vector<uint64_t> const &path);

template<typename T>
Value make_value(T const &val)
{
    return Value { val };
}

template<>
inline Value make_value(std::wstring const &val)
{
    wchar_t const       *ptr = make_interned_string(Value::arena, std::wstring_view { val });
    InternedString const interned { ptr };
    return Value { TypeRegistry::string, Slice { const_cast<void *>(static_cast<void const *>(ptr)), interned.length() } };
}

template<>
inline Value make_value(std::string const &val)
{
    char const          *ptr = make_interned_string(Value::arena, std::string_view { val });
    InternedString const interned { ptr };
    return Value { TypeRegistry::string, Slice { const_cast<void *>(static_cast<void const *>(interned.converted<wchar_t>())), interned.length() } };
}

template<typename T>
Value make_value(pType const &type, T const &val)
{
    return Value { type, Atom { val } };
}

template<>
inline Value make_value(pType const &type)
{
    switch (type->kind()) {
    case TypeKind::IntType: {
        switch (auto const &d = std::get<IntType>(type->description); d.width_bits) {
#undef S
#define S(W) \
    case W:  \
        return (d.is_signed) ? Value(static_cast<int##W##_t>(0)) : Value(static_cast<uint##W##_t>(0));
            BitWidths(S) default : UNREACHABLE();
        }
    }
    case TypeKind::FloatType: {
        switch (auto const &d = std::get<FloatType>(type->description); d.width_bits) {
#undef S
#define S(W, T) \
    case W:     \
        return Value(static_cast<T>(0));
            FloatBitWidths(S) default : UNREACHABLE();
        }
    }
    case TypeKind::BoolType:
        return Value(false);
    case TypeKind::SliceType:
        return make_value(type, Slice { nullptr, 0 });
    case TypeKind::DynArray:
        return make_value(type, DynamicArray { nullptr, 0, 0 });
    case TypeKind::Array:
        return make_value(type, StaticArray { nullptr, 0 });
    default:
        UNREACHABLE();
    }
}

inline Value make_void()
{
    return Value { TypeRegistry::void_ };
}

Value evaluate(Value const &lhs, Operator op, Value const &rhs);
Value evaluate(Operator op, Value const &operand);

}

inline std::wostream &operator<<(std::wostream &os, Arwen::Atom const &atom)
{
    using namespace Util;
    using namespace Arwen;
    std::visit(
        overloads {
            [&os](bool const &b) -> void {
                os << std::boolalpha << b;
            },
            [&os](Slice const &v) -> void {
                os << static_cast<char *>(v.ptr);
            },
            [&os](DynamicArray const &v) -> void {
                UNREACHABLE();
            },
            [&os](StaticArray const &v) -> void {
                UNREACHABLE();
            },
            [&os](auto const &v) -> void {
                os << v;
            },
        },
        atom.payload);
    return os;
}

inline std::wostream &operator<<(std::wostream &os, Arwen::Value const &value)
{
    using namespace Util;
    using namespace Arwen;
    os << "[" << value.type->to_string() << "] ";
    std::visit(
        overloads {
            [&os](std::monostate const &) -> void {
            },
            [&os](Atom const &atom) -> void {
                os << atom;
            },
            [&os](Value::Values const &values) -> void {
                auto first { true };
                for (auto const &value : values) {
                    if (!first) {
                        os << ", ";
                    }
                    first = false;
                    os << value;
                }
            },
        },
        value.payload);
    return os;
}
