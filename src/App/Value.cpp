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

static bool is_zero(Atom const &atom);

#undef S
#define S(O) static Atom evaluate_##O(Atom const &lhs, Atom const &rhs);
BinOps(S)
#undef S

    static bool is_zero(Atom const &atom)
{
    return std::visit(overloads {
                          [](std::integral auto value) -> bool {
                              return value == 0;
                          },
                          [](std::floating_point auto value) -> bool {
                              return value == 0.0;
                          },
                          [](bool value) -> bool {
                              return !value;
                          },
                          [](void *value) -> bool {
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

std::optional<Value> Value::coerce(pType const &to_type) const
{
    if (type == to_type) {
        return Value { *this };
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
            auto         lhs_value = as<std::wstring>(lhs);
            auto         rhs_value = as<uint64_t>(rhs);
            for (auto count = 0; count < rhs_value; ++count) {
                ret += lhs_value;
            }
            return make_value(ret);
        }
        if (lhs.type == TypeRegistry::string && rhs.type == TypeRegistry::string) {
            return make_value(as<std::wstring>(lhs) + as<std::wstring>(rhs));
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
    default:
    }
    return evaluate_on_atoms(lhs, op, rhs);
}

Value evaluate(Operator op, Value const &operand)
{
    return evaluate(operand, op, Value { 0 });
}
}
