/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cmath>
#include <concepts>
#include <cstdint>
#include <string>
#include <variant>

#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <Interp/Interpreter.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

Value::Value(pType const &type)
    : type(type)
{
    if (type->is<TypeAlias>()) {
        auto  def = std::get<TypeAlias>(type->description);
        Value v { def.alias_of };
        this->type = v.type;
        this->payload = v.payload;
        return;
    }
    if (type->is<BoolType>()) {
        payload.emplace<PayloadValue>(PayloadValue { false });
        return;
    }
    if (type->is<IntType>()) {
        auto def = std::get<IntType>(type->description);
        if (def.is_signed) {
            payload.emplace<PayloadValue>(PayloadValue { static_cast<int64_t>(0) });
        } else {
            payload.emplace<PayloadValue>(PayloadValue { static_cast<uint64_t>(0) });
        }
        return;
    }
    if (type->is<FloatType>()) {
        auto def = std::get<FloatType>(type->description);
        payload.emplace<PayloadValue>(PayloadValue { 0.0 });
        return;
    }
    if (type == TypeRegistry::string) {
        payload.emplace<PayloadValue>(PayloadValue { L"" });
        return;
    }
    if (type == TypeRegistry::void_) {
        payload.emplace<PayloadValue>(PayloadValue { std::monostate {} });
        return;
    }
    fatal(L"Unsupported interpreter value type `{}`", type->to_string());
}

Value::Value(Value const &value)
    : type(value.type)
    , payload(value.payload)
{
}

bool Value::is_zero() const
{
    return std::visit(overloads {
                          [](PayloadValue const &payload) -> bool {
                              return std::visit(overloads {
                                                    [](std::integral auto const &v) -> bool {
                                                        return v == 0;
                                                    },
                                                    [](std::floating_point auto const &v) -> bool {
                                                        return v == 0.0;
                                                    },
                                                    [](auto const &v) -> bool {
                                                        fatal("Cannot check zero value of non-numbers");
                                                    } },
                                  payload);
                          },
                          [](auto const &payload) -> bool {
                              fatal("Cannot check zero value of non-numbers");
                          } },
        payload);
}

Value Value::evaluate_Call(Value const &)
{
    fatal("Cannot call a value");
}

Value Value::evaluate_Cast(Value const &)
{
    fatal("Cannot cast a value");
}

Value Value::evaluate_MemberAccess(Value const &)
{
    fatal("Cannot access members of a value");
}

Value Value::evaluate_Range(Value const &)
{
    fatal("Cannot build a range value");
}

Value Value::evaluate_Sequence(Value const &)
{
    fatal("Cannot build a sequence value");
}

Value Value::evaluate_Subscript(Value const &)
{
    fatal("Cannot subscript a value");
}

Value Value::evaluate_Add(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x + y; });
}

Value Value::evaluate_Subtract(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x - y; });
}

Value Value::evaluate_Multiply(Value const &rhs)
{
    return std::visit(overloads {
                          [this, &rhs](PayloadValue const &lhs_payload, PayloadValue const &rhs_payload) -> Value {
                              return std::visit(overloads {
                                                    [](std::wstring const &lhs_value, std::unsigned_integral auto const &rhs_value) -> Value {
                                                        std::wstring ret {};
                                                        for (auto count = 0; count < rhs_value; ++count) {
                                                            ret += lhs_value;
                                                        }
                                                        return Value { ret };
                                                    },
                                                    [](std::integral auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                        return Value { lhs_value * rhs_value };
                                                    },
                                                    [](std::integral auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                        return Value { lhs_value * rhs_value };
                                                    },
                                                    [](std::floating_point auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                        return Value { lhs_value * rhs_value };
                                                    },
                                                    [](std::floating_point auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                        return Value { lhs_value * rhs_value };
                                                    },
                                                    [](auto const &lhs_value, auto const &rhs_value) -> Value {
                                                        fatal("Cannot calculate modulo of non-numbers");
                                                    } },
                                  lhs_payload, rhs_payload);
                          },
                          [](auto const &lhs_payload, auto const &rhs_payload) -> Value {
                              fatal("Cannot calculate modulo of non-numbers");
                          } },
        payload, rhs.payload);
}

Value Value::evaluate_Divide(Value const &rhs)
{
    if (rhs.is_zero()) {
        fatal("Division by zero");
    }
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x / y; });
}

Value Value::evaluate_Modulo(Value const &rhs)
{
    if (rhs.is_zero()) {
        fatal("Division by zero");
    }
    return std::visit(overloads {
                          [this, &rhs](PayloadValue const &lhs_payload, PayloadValue const &rhs_payload) -> Value {
                              return std::visit(overloads {
                                                    [](std::integral auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                        return Value { lhs_value % rhs_value };
                                                    },
                                                    [](std::integral auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                        return Value { fmod(lhs_value, rhs_value) };
                                                    },
                                                    [](std::floating_point auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                        return Value { fmod(lhs_value, rhs_value) };
                                                    },
                                                    [](std::floating_point auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                        return Value { fmod(lhs_value, rhs_value) };
                                                    },
                                                    [](auto const &lhs_value, auto const &rhs_value) -> Value {
                                                        fatal("Cannot calculate modulo of non-numbers");
                                                    } },
                                  lhs_payload, rhs_payload);
                          },
                          [](auto const &lhs_payload, auto const &rhs_payload) -> Value {
                              fatal("Cannot calculate modulo of non-numbers");
                          } },
        payload, rhs.payload);
}

Value Value::evaluate_Equals(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x == y; });
}

Value Value::evaluate_NotEqual(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x != y; });
}

Value Value::evaluate_Less(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x < y; });
}

Value Value::evaluate_LessEqual(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x <= y; });
}

Value Value::evaluate_Greater(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x > y; });
}

Value Value::evaluate_GreaterEqual(Value const &rhs)
{
    return evaluate_op(
        rhs,
        [](auto x, auto y) { return x >= y; });
}

Value Value::evaluate_BinaryAnd(Value const &rhs)
{
    return evaluate_binary_op(
        rhs,
        [](auto x, auto y) { return x & y; });
}

Value Value::evaluate_BinaryOr(Value const &rhs)
{
    return evaluate_binary_op(
        rhs,
        [](auto x, auto y) { return x | y; });
}

Value Value::evaluate_BinaryXor(Value const &rhs)
{
    return evaluate_binary_op(
        rhs,
        [](auto x, auto y) { return x ^ y; });
}

Value Value::evaluate_ShiftLeft(Value const &rhs)
{
    return evaluate_binary_op(
        rhs,
        [](auto x, auto y) { return x << y; });
}

Value Value::evaluate_ShiftRight(Value const &rhs)
{
    return evaluate_binary_op(
        rhs,
        [](auto x, auto y) { return x >> y; });
}

Value Value::evaluate_LogicalAnd(Value const &rhs)
{
    return evaluate_logical_op(
        rhs,
        [](auto x, auto y) { return x && y; });
}

Value Value::evaluate_LogicalOr(Value const &rhs)
{
    return evaluate_logical_op(
        rhs,
        [](auto x, auto y) { return x || y; });
}

Value Value::evaluate_Idempotent(Value const &)
{
    return Value { *this };
}

Value Value::evaluate_Negate(Value const &)
{
    return std::visit(overloads {
                          [](PayloadValue const &payload) -> Value {
                              return std::visit(overloads {
                                                    [](std::unsigned_integral auto const &value) -> Value {
                                                        if (value > std::numeric_limits<int64_t>::max()) {
                                                            fatal("Cannot invert integer larger than int64_t::max");
                                                        }
                                                        return Value { -value };
                                                    },
                                                    [](std::signed_integral auto const &value) -> Value {
                                                        return Value { -value };
                                                    },
                                                    [](std::floating_point auto const &value) -> Value {
                                                        return Value { -value };
                                                    },
                                                    [](auto const &value) -> Value {
                                                        fatal("Cannot negate non-numbers");
                                                    } },
                                  payload);
                          },
                          [](auto const &payload) -> Value {
                              fatal("Cannot negate non-numbers");
                          } },
        payload);
}

Value Value::evaluate_BinaryInvert(Value const &)
{
    return std::visit(overloads {
                          [](PayloadValue const &payload) -> Value {
                              return std::visit(overloads {
                                                    [](std::integral auto const &value) -> Value {
                                                        return Value { ~value };
                                                    },
                                                    [](auto const &value) -> Value {
                                                        fatal("Cannot invert non-integers");
                                                    } },
                                  payload);
                          },
                          [](auto const &payload) -> Value {
                              fatal("Cannot invert non-integers");
                          } },
        payload);
}

Value Value::evaluate_LogicalInvert(Value const &)
{
    return std::visit(overloads {
                          [](PayloadValue const &payload) -> Value {
                              return std::visit(overloads {
                                                    [](bool const &value) -> Value {
                                                        return Value { !value };
                                                    },
                                                    [](auto const &value) -> Value {
                                                        fatal("Cannot invert non-booleans");
                                                    } },
                                  payload);
                          },
                          [](auto const &payload) -> Value {
                              fatal("Cannot invert non-booleans");
                          } },
        payload);
}

Value Value::evaluate(Operator op, Value const &rhs)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_##O(rhs);
        BinOps(S)
#undef S
            default : UNREACHABLE();
    }
}

std::wstring Value::to_string() const
{
    auto payload_to_string = [](Value::PayloadValue const &value) -> std::wstring {
        return std::visit(overloads {
                              [](std::monostate const &) -> std::wstring {
                                  return L"void";
                              },
                              [](std::wstring const &value) -> std::wstring {
                                  return value;
                              },
                              [](bool const &value) -> std::wstring {
                                  return (value) ? L"true" : L"false";
                              },
                              [](std::integral auto const &value) -> std::wstring {
                                  return std::format(L"{}", value);
                              },
                              [](std::floating_point auto const &value) -> std::wstring {
                                  return std::format(L"{}", value);
                              } },
            value);
    };

    return std::visit(overloads {
                          [&payload_to_string](PayloadValue const &payload) -> std::wstring {
                              return payload_to_string(payload);
                          },
                          [&payload_to_string](PayloadValues const &values) -> std::wstring {
                              std::wstring ret { 'L' };
                              bool         first { true };
                              for (auto const &value : values) {
                                  if (first) {
                                      ret += ' ';
                                  }
                                  ret += payload_to_string(value);
                                  ret += ' ';
                              }
                              ret += ']';
                              return ret;
                          } },
        payload);
}
}
