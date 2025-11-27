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

    Value(pType const &type, Atom val)
        : type(type)
        , payload(val)
    {
    }

    Value(pType const &type, Values values)
        : type(type)
        , payload(values)
    {
    }

    Value(pType const &type, auto val)
        : Value(type, Atom { val })
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

Value  make_from_buffer(pType const &type, void *buffer);
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
    return make_from_buffer(type, nullptr);
}

inline Value make_void()
{
    return Value { TypeRegistry::void_ };
}

Value evaluate(Value const &lhs, Operator op, Value const &rhs);
Value evaluate(Operator op, Value const &operand);

}

namespace std {

using namespace Util;
using namespace Arwen;

inline wostream &operator<<(wostream &os, Atom const &atom)
{
    visit(
        overloads {
            [&os](bool const &b) -> void {
                os << std::boolalpha << b;
            },
            [&os](Slice const &v) -> void {
                os << static_cast<wchar_t *>(v.ptr);
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

inline wostream &operator<<(wostream &os, Value const &value)
{
    using namespace Util;
    using namespace Arwen;
    visit(
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

template<>
struct formatter<Value, wchar_t> {
    bool with_type { false };

    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end() || *it == '}')
            return it;

        switch (*it) {
        case 't':
            with_type = true;
            break;
        default:
            throw std::format_error("Invalid format args for Value");
        }
        ++it;
        if (it != ctx.end() && *it != '}') {
            throw std::format_error("Invalid format args for Value");
        }
        return it;
    }

    template<class FmtContext>
    FmtContext::iterator format(Value const &val, FmtContext &ctx) const
    {
        std::wostringstream out;
        out << val;
        if (with_type) {
            out << L" [" << val.type << ']';
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

}
