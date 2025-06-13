/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cmath>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <string>
#include <variant>

#include <Util/Logging.h>

#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

using namespace Util;

static std::vector<pType> atom_types {};
Arena                     Value::arena;

pType Atom::type() const
{
    if (atom_types.empty()) {
        atom_types = {
            TypeRegistry::boolean,
#undef S
#define S(W)            \
    TypeRegistry::i##W, \
        TypeRegistry::u##W,
            BitWidths(S)
#undef S
                TypeRegistry::f32,
            TypeRegistry::f64,
            TypeRegistry::pointer
        };
    }
    return atom_types[payload.index()];
}

std::optional<Atom> Atom::coerce(pType const &to_type) const
{
    auto const &cur_type = type();
    if (cur_type == to_type) {
        return Atom { *this };
    }
    return std::visit(overloads {
                          [&cur_type, &to_type](std::integral auto const &v) -> std::optional<Atom> {
                              if (to_type->is<IntType>()) {
                                  auto const &[is_signed, width_bits, max_value, min_value] = std::get<IntType>(to_type->description);
                                  if (static_cast<uint64_t>(v) > max_value) {
                                      return {};
                                  }
                                  if (static_cast<int64_t>(v) < min_value) {
                                      return {};
                                  }
#undef S
#define S(W)                                             \
    if (width_bits == W) {                               \
        if (is_signed) {                                 \
            return Atom { static_cast<int##W##_t>(v) };  \
        } else {                                         \
            return Atom { static_cast<uint##W##_t>(v) }; \
        }                                                \
    }
                                  BitWidths(S)
#undef S
                              }
                              if (to_type->is<FloatType>()) {
                                  auto const &[width_bits] = std::get<FloatType>(to_type->description);
#undef S
#define S(W, T)                            \
    if (width_bits == W) {                 \
        return Atom { static_cast<T>(v) }; \
    }
                                  FloatBitWidths(S)
#undef S
                              }
                              if (to_type->is<PointerType>() && cur_type == TypeRegistry::u64) {
                                  return Atom { reinterpret_cast<void *>(v) };
                              }
                              return {};
                          },
                          [&cur_type, &to_type](std::floating_point auto const &v) -> std::optional<Atom> {
                              if (to_type->is<IntType>()) {
                                  auto const &[is_signed, width_bits, max_value, min_value] = std::get<IntType>(to_type->description);
                                  if (static_cast<uint64_t>(v) > max_value) {
                                      return {};
                                  }
                                  if (static_cast<int64_t>(v) < min_value) {
                                      return {};
                                  }
#undef S
#define S(W)                                             \
    if (width_bits == W) {                               \
        if (is_signed) {                                 \
            return Atom { static_cast<int##W##_t>(v) };  \
        } else {                                         \
            return Atom { static_cast<uint##W##_t>(v) }; \
        }                                                \
    }
                                  BitWidths(S)
#undef S
                              }
                              if (to_type->is<FloatType>()) {
                                  auto const &[width_bits] = std::get<FloatType>(to_type->description);
#undef S
#define S(W, T)                            \
    if (width_bits == W) {                 \
        return Atom { static_cast<T>(v) }; \
    }
                                  FloatBitWidths(S)
#undef S
                              }
                              return {};
                          },
                          [](auto const &) -> std::optional<Atom> {
                              return {};
                          } },
        payload);
}

std::wstring Atom::to_string() const
{
    return std::visit(overloads {
                          [](std::integral auto const &value) -> std::wstring {
                              return std::format(L"{}", value);
                          },
                          [](std::floating_point auto const &value) -> std::wstring {
                              return std::format(L"{}", value);
                          },
                          [](bool const &value) -> std::wstring {
                              return value ? L"true" : L"false";
                          },
                          [](Slice const &value) -> std::wstring {
                              return std::format(L"Slice[{}]", value.size);
                          },
                          [](DynamicArray const &value) -> std::wstring {
                              return std::format(L"DynamicArray[{},{}]", value.size, value.capacity);
                          },
                          [](StaticArray const &value) -> std::wstring {
                              return std::format(L"StaticArray[{}]", value.size);
                          },
                          [](void *const &value) -> std::wstring {
                              return std::format(L"void*[{}]", value);
                          } },
        payload);
}

static bool is_zero(Atom const &atom);

#undef S
#define S(O) static Atom evaluate_##O(Atom const &lhs, Atom const &rhs);
BinOps(S)
#undef S

    static bool is_zero(Atom const &atom)
{
    return std::visit(overloads {
                          [](std::integral auto const &value) -> bool {
                              return value == 0;
                          },
                          [](std::floating_point auto const &value) -> bool {
                              return value == 0.0;
                          },
                          [](bool const &value) -> bool {
                              return !value;
                          },
                          [](Slice const &value) -> bool {
                              return value.ptr == nullptr || value.size == 0;
                          },
                          [](DynamicArray const &value) -> bool {
                              return value.ptr == nullptr || value.size == 0;
                          },
                          [](StaticArray const &value) -> bool {
                              return value.ptr == nullptr;
                          },
                          [](void *const &value) -> bool {
                              return value == nullptr;
                          } },
        atom.payload);
}

template<typename Func>
static Atom evaluate_op(Atom const &lhs, Atom const &rhs, Func const &func)
{
    return std::visit(overloads {
                          [&func](std::integral auto lhs_value, std::integral auto rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [&func](std::integral auto lhs_value, std::floating_point auto rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [&func](std::floating_point auto lhs_value, std::integral auto rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [&func](std::floating_point auto lhs_value, std::floating_point auto rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [](auto lhs_value, auto rhs_value) -> Atom {
                              fatal("Operator only applicable to numbers");
                          } },
        lhs.payload, rhs.payload);
}

template<typename Func>
static Atom evaluate_bitwise_op(Atom const &lhs, Atom const &rhs, Func const &func)
{
    return std::visit(overloads {
                          [&func](std::integral auto lhs_value, std::integral auto rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [](auto lhs_value, auto rhs_value) -> Atom {
                              fatal("Operator only applicable to integers");
                          } },
        lhs.payload, rhs.payload);
}

template<typename Func>
static Atom evaluate_logical_op(Atom const &lhs, Atom const &rhs, Func const &func)
{
    return std::visit(overloads {
                          [&func](bool lhs_value, bool rhs_value) -> Atom {
                              return Atom { func(lhs_value, rhs_value) };
                          },
                          [](auto lhs_value, auto rhs_value) -> Atom {
                              fatal("Operator only applicable to booleans");
                          } },
        lhs.payload, rhs.payload);
}

Atom evaluate_AddressOf(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot take address of an atom");
}

Atom evaluate_Call(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot call a value");
}

Atom evaluate_Cast(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot cast a value");
}

Atom evaluate_Length(Atom const &, Atom const &)
{
    fatal("Cannot take length of an Atom");
}

Atom evaluate_MemberAccess(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot access members of a value");
}

Atom evaluate_Range(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot build a range value");
}

Atom evaluate_Sequence(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot build a sequence value");
}

Atom evaluate_Subscript(Atom const &lhs, Atom const &rhs)
{
    fatal("Cannot subscript a value");
}

Atom evaluate_Add(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x + y; });
}

Atom evaluate_Subtract(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x - y; });
}

Atom evaluate_Multiply(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x * y; });
}

Atom evaluate_Divide(Atom const &lhs, Atom const &rhs)
{
    if (is_zero(rhs)) {
        fatal("Division by zero");
    }
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x / y; });
}

Atom evaluate_Modulo(Atom const &lhs, Atom const &rhs)
{
    if (is_zero(rhs)) {
        fatal("Division by zero");
    }
    return std::visit(overloads {
                          [](std::integral auto lhs_value, std::integral auto rhs_value) -> Atom {
                              return Atom { lhs_value % rhs_value };
                          },
                          [](std::integral auto lhs_value, std::floating_point auto rhs_value) -> Atom {
                              return Atom { fmod(lhs_value, rhs_value) };
                          },
                          [](std::floating_point auto lhs_value, std::integral auto rhs_value) -> Atom {
                              return Atom { fmod(lhs_value, rhs_value) };
                          },
                          [](std::floating_point auto lhs_value, std::floating_point auto rhs_value) -> Atom {
                              return Atom { fmod(lhs_value, rhs_value) };
                          },
                          [](auto lhs_value, auto rhs_value) -> Atom {
                              fatal("Modulo operation only applicable to numbers");
                          } },
        lhs.payload, rhs.payload);
}

Atom evaluate_Equals(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x == y; });
}

Atom evaluate_NotEqual(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x != y; });
}

Atom evaluate_Less(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x < y; });
}

Atom evaluate_LessEqual(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x <= y; });
}

Atom evaluate_Greater(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x > y; });
}

Atom evaluate_GreaterEqual(Atom const &lhs, Atom const &rhs)
{
    return evaluate_op(lhs, rhs,
        [](auto x, auto y) { return x >= y; });
}

Atom evaluate_BinaryAnd(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, rhs,
        [](auto x, auto y) { return x & y; });
}

Atom evaluate_BinaryOr(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, rhs,
        [](auto x, auto y) { return x | y; });
}

Atom evaluate_BinaryXor(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, rhs,
        [](auto x, auto y) { return x ^ y; });
}

Atom evaluate_ShiftLeft(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, rhs,
        [](auto x, auto y) { return x << y; });
}

Atom evaluate_ShiftRight(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, rhs,
        [](auto x, auto y) { return x >> y; });
}

Atom evaluate_LogicalAnd(Atom const &lhs, Atom const &rhs)
{
    return evaluate_logical_op(lhs, rhs,
        [](auto x, auto y) { return x && y; });
}

Atom evaluate_LogicalOr(Atom const &lhs, Atom const &rhs)
{
    return evaluate_logical_op(lhs, rhs,
        [](auto x, auto y) { return x || y; });
}

Atom evaluate_Idempotent(Atom const &lhs, Atom const &)
{
    return lhs;
}

Atom evaluate_Negate(Atom const &lhs, Atom const &)
{
    return evaluate_op(lhs, Atom { (uint32_t) 0 },
        [](auto x, auto) { return -x; });
}

Atom evaluate_BinaryInvert(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, Atom { (uint32_t) 0 },
        [](auto x, auto) { return ~x; });
}

Atom evaluate_LogicalInvert(Atom const &lhs, Atom const &rhs)
{
    return evaluate_bitwise_op(lhs, Atom { false },
        [](auto x, auto) { return !x; });
}

Atom evaluate_Sizeof(Atom const &lhs, Atom const &)
{
    return Atom { lhs.type()->size_of() };
}

Atom evaluate(Atom const &lhs, Operator op, Atom const &rhs)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_##O(lhs, rhs);
        BinOps(S)
#undef S
            default : UNREACHABLE();
    }
}

Atom evaluate(Operator op, Atom const &operand)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_##O(operand, Atom { false });
        BinOps(S)
#undef S
            default : UNREACHABLE();
    }
}

Value::Value(Value &other)
    : type(other.type)
{
    std::visit(overloads {
                   [this](std::monostate const &) {
                       this->payload = std::monostate {};
                   },
                   [this](Atom const &atom) {
                       this->payload = atom;
                   },
                   [this](Atoms const &atoms) {
                       this->payload = atoms;
                   } },
        other.payload);
}

Value::Value(pType const &type)
    : type(type)
    , payload(std::monostate {})
{
}

Value::Value(int8_t const val)
    : Value(TypeRegistry::i8, val)
{
}

Value::Value(uint8_t const val)
    : Value(TypeRegistry::u8, val)
{
}

Value::Value(int16_t const val)
    : Value(TypeRegistry::i16, val)
{
}

Value::Value(uint16_t const val)
    : Value(TypeRegistry::u16, val)
{
}

Value::Value(int32_t const val)
    : Value(TypeRegistry::i32, val)
{
}

Value::Value(uint32_t const val)
    : Value(TypeRegistry::u32, val)
{
}

Value::Value(int64_t const val)
    : Value(TypeRegistry::i64, val)
{
}

Value::Value(uint64_t const val)
    : Value(TypeRegistry::u64, val)
{
}

Value::Value(float const val)
    : Value(TypeRegistry::f32, val)
{
}

Value::Value(double const val)
    : Value(TypeRegistry::f64, val)
{
}

Value::Value(bool const val)
    : Value(TypeRegistry::boolean, val)
{
}

Value::Value(void *val)
    : Value(TypeRegistry::pointer, val)
{
}

Value::Value(Atom atom)
    : Value(atom.type(), atom)
{
}

std::optional<Value> Value::coerce(pType const &to_type) const
{
    if (type == to_type) {
        return Value { *this };
    }
    if (type == TypeRegistry::string && to_type == TypeRegistry::cstring) {
        auto const [ptr, _] = as<Slice>(*this);
        return Value { TypeRegistry::cstring, Atom { ptr } };
    }
    if (type->is<DynArray>() && to_type->is<SliceType>() && std::get<DynArray>(type->description).array_of == std::get<SliceType>(type->description).slice_of) {
        auto const dyn_arr = as<DynamicArray>(*this);
        return Value { to_type, Slice { dyn_arr.ptr, dyn_arr.size } };
    }
    if (type->is<Array>() && to_type->is<SliceType>() && std::get<Array>(type->description).array_of == std::get<SliceType>(type->description).slice_of) {
        auto const arr = as<StaticArray>(*this);
        return Value { to_type, Slice { arr.ptr, arr.size } };
    }
    return std::visit(overloads {
                          [this, &to_type](Atom const &atom) -> std::optional<Value> {
                              if (auto const coerced_maybe = atom.coerce(to_type); coerced_maybe) {
                                  return Value { *coerced_maybe };
                              }
                              return {};
                          },
                          [](auto const &) -> std::optional<Value> {
                              fatal("Cannot apply operator to compound values");
                          } },
        payload);
}

std::wstring Value::to_string() const
{
    return std::visit(overloads {
                          [](std::monostate const &) -> std::wstring {
                              return L"``";
                          },
                          [](Atom const &atom) -> std::wstring {
                              return atom.to_string();
                          },
                          [](auto const &) -> std::wstring {
                              fatal("Cannot stringify compound values yet");
                          } },
        payload);
}

static Value evaluate_on_atoms(Value const &lhs, Operator op, Value const &rhs)
{
    return std::visit(overloads {
                          [&lhs, op](Atom const &lhs_atom, Atom const &rhs_atom) -> Value {
                              return Value { evaluate(lhs_atom, op, rhs_atom) };
                          },
                          [](auto, auto) -> Value {
                              fatal("Cannot apply operator to compound values");
                          } },
        lhs.payload, rhs.payload);
}

Value evaluate(Value const &lhs, Operator op, Value const &rhs)
{
    switch (op) {
    case Operator::Add: {
        if (lhs.type == TypeRegistry::string && rhs.type->kind() == TypeKind::IntType) {
            std::wstring ret {};
            auto         lhs_value = lhs.as_wstring();
            auto         rhs_value = as<uint64_t>(rhs);
            for (auto count = 0; count < rhs_value; ++count) {
                ret += lhs_value;
            }
            return make_value(ret);
        }
        if (lhs.type == TypeRegistry::string && rhs.type == TypeRegistry::string) {
            return make_value(lhs.as_wstring() + rhs.as_wstring());
        }
    } break;
    case Operator::Length: {
        switch (lhs.type->kind()) {
        case TypeKind::SliceType: {
            auto slice = as<Slice>(lhs);
            return make_value(slice.size);
        }
        case TypeKind::DynArray: {
            auto dyn_arr = as<DynamicArray>(lhs);
            return make_value(dyn_arr.size);
        }
        case TypeKind::Array: {
            auto arr = as<StaticArray>(lhs);
            return make_value(arr.size);
        }
        case TypeKind::ZeroTerminatedArray: {
            auto is_zero = [](char *ptr, size_t bytes) {
                for (auto ix = 0; ix < bytes; ++ix) {
                    if (ptr[ix] != 0)
                        return false;
                }
                return true;
            };
            int64_t len = 0;
            for (auto ptr = (char *) as<void *>(lhs); !is_zero(ptr, lhs.type->size_of()); ++len) {
                ptr += lhs.type->size_of();
            }
            return len;
        }
        default:
        }
    }
    case Operator::Sizeof:
        return Value { static_cast<int64_t>(lhs.type->size_of()) };
    default:
    }
    return evaluate_on_atoms(lhs, op, rhs);
}

Value evaluate(Operator op, Value const &operand)
{
    return evaluate(operand, op, Value { 0 });
}
}
