/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Util/JSON.h"

#include <concepts>
#include <cstdint>
#include <ranges>
#include <set>
#include <string>
#include <variant>

#include <Util/Logging.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Type.h>

namespace Arwen {

using namespace Util;

using StringSet = std::set<std::string>;
static StringSet interned_strings {};

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

    template<typename T>
    explicit Atom(T *val)
    {
        payload = (void *) val;
    }

    template<typename T>
    T as() const
    {
        return std::get<T>(payload);
    }

    Atom &operator=(Atom const &) = default;
    bool  operator==(Atom const &) const = default;

    [[nodiscard]] pType               type() const;
    [[nodiscard]] std::optional<Atom> coerce(pType const &to_type) const;
};

using Atoms = std::vector<Atom>;

Atom evaluate(Atom const &lhs, Operator op, Atom const &rhs);
Atom evaluate(Operator op, Atom const &operand);

struct Value {
    pType                     type { TypeRegistry::void_ };
    std::variant<Atom, Atoms> payload { Atoms {} };

    Value() = default;
    Value(Value const &) = default;

    Value(Value &other)
        : type(other.type)
    {
        std::visit(overloads {
                       [this](Atom const &atom) {
                           this->payload = atom;
                       },
                       [this](Atoms const &atoms) {
                           Atoms mine { atoms };
                           this->payload = atoms;
                       } },
            other.payload);
    }

    explicit Value(pType const &type)
        : Value(type, Atoms {})
    {
    }

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
    Value(pType const &type, std::vector<Atom> values)
        : type(type)
        , payload(values)
    {
    }

    Value(int8_t const val)
        : Value(TypeRegistry::i8, val)
    {
    }

    Value(uint8_t const val)
        : Value(TypeRegistry::u8, val)
    {
    }

    Value(int16_t const val)
        : Value(TypeRegistry::i16, val)
    {
    }

    Value(uint16_t const val)
        : Value(TypeRegistry::u16, val)
    {
    }

    Value(int32_t const val)
        : Value(TypeRegistry::i32, val)
    {
    }

    Value(uint32_t const val)
        : Value(TypeRegistry::u32, val)
    {
    }

    Value(int64_t const val)
        : Value(TypeRegistry::i64, val)
    {
    }

    Value(uint64_t const val)
        : Value(TypeRegistry::u64, val)
    {
    }

    explicit Value(float const val)
        : Value(TypeRegistry::f32, val)
    {
    }

    explicit Value(double const val)
        : Value(TypeRegistry::f64, val)
    {
    }

    explicit Value(bool const val)
        : Value(TypeRegistry::boolean, val)
    {
    }

    Value(void *val)
        : Value(TypeRegistry::pointer, val)
    {
    }

    explicit Value(Atom atom)
        : Value(atom.type(), atom)
    {
    }

    // Value &operator=(Value const &) = default;

    [[nodiscard]] std::optional<Value> coerce(pType const &to_type) const;
};

template<typename T>
T as(Value const &val)
{
    return std::visit(overloads {
                          [](Atom const &atom) -> T {
                              return atom.as<T>();
                          },
                          [](Atoms const &) -> T {
                              UNREACHABLE();
                          } },
        val.payload);
}

template<>
inline std::wstring as(Value const &val)
{
    return std::visit(overloads {
                          [](Atom const &atom) -> std::wstring {
                              UNREACHABLE();
                          },
                          [&val](Atoms const &atoms) -> std::wstring {
                              assert(val.type == TypeRegistry::string);
                              auto       ptr = atoms[0].as<void *>();
                              auto const len = atoms[1].as<int64_t>();
                              auto const utf8 = std::string { (char *) ptr, static_cast<size_t>(len) };
                              return as_wstring(utf8);
                          } },
        val.payload);
}

template<>
inline Slice as(Value const &val)
{
    return std::visit(overloads {
                          [](Atom const &atom) -> Slice {
                              UNREACHABLE();
                          },
                          [&val](Atoms const &atoms) -> Slice {
                              assert(val.type->kind() == TypeKind::SliceType);
                              return Slice { atoms[0].as<void *>(), atoms[1].as<int64_t>() };
                          } },
        val.payload);
}

template<>
inline DynamicArray as(Value const &val)
{
    return std::visit(overloads {
                          [](Atom const &atom) -> DynamicArray {
                              UNREACHABLE();
                          },
                          [&val](Atoms const &atoms) -> DynamicArray {
                              assert(val.type->kind() == TypeKind::DynArray);
                              return DynamicArray { atoms[0].as<void*>(), atoms[1].as<int64_t>(), atoms[1].as<int64_t>() };
                          } },
        val.payload);
}

template<>
inline StaticArray as(Value const &val)
{
    return std::visit(overloads {
                          [](Atom const &atom) -> StaticArray {
                              UNREACHABLE();
                          },
                          [&val](Atoms const &atoms) -> StaticArray {
                              assert(val.type->kind() == TypeKind::Array);
                              return StaticArray { atoms[0].as<void *>(), atoms[1].as<int64_t>() };
                          } },
        val.payload);
}

template<typename T>
Value make_value(T const &val)
{
    return Value { val };
}

template<>
inline Value make_value(std::wstring const &val)
{
    auto const utf8 = as_utf8(val);
    interned_strings.insert(utf8);
    auto const it = interned_strings.find(utf8);
    assert(it != interned_strings.cend());
    std::string const &s = *it;
    return Value { TypeRegistry::string, Atoms { Atom { s.data() }, Atom { static_cast<int64_t>(s.length()) } } };
}

template<>
inline Value make_value(std::string const &val)
{
    return make_value(as_wstring(val));
}

template<typename T>
Value make_value(pType const &type, T const &val)
{
    static_assert(false);
}

template<>
inline Value make_value(pType const &type, Slice const &val)
{
    assert(type->kind() == TypeKind::SliceType);
    return Value { type, Atoms { Atom { val.ptr }, Atom { val.size } } };
}

template<>
inline Value make_value(pType const &type, DynamicArray const &val)
{
    return Value { type, Atoms { Atom { val.ptr }, Atom { val.size }, Atom { val.capacity } } };
}

template<>
inline Value make_value(pType const &type, StaticArray const &val)
{
    return Value { type, Atoms { Atom { val.ptr }, Atom { val.size } } };
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
    return Value { TypeRegistry::void_, Atoms {} };
}

Value evaluate(Value const &lhs, Operator op, Value const &rhs);
Value evaluate(Operator op, Value const &operand);

}

inline std::wostream &operator<<(std::wostream &os, Arwen::Atom const &atom)
{
    using namespace Util;
    using namespace Arwen;
    std::visit(overloads {
                   [&os](bool const b) -> void {
                       os << std::boolalpha << b;
                   },
                   [&os](auto const v) -> void {
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
    std::visit(overloads {
                   [&os](Atom const &atom) -> void {
                       os << atom;
                   },
                   [&os](Atoms const &atoms) -> void {
                       auto first { true };
                       for (auto const &atom : atoms) {
                           if (!first) {
                               os << ", ";
                           }
                           first = false;
                           os << atom;
                       }
                   },
               },
        value.payload);
    return os;
}
