/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <cstdint>
#include <string>
#include <variant>

#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

struct Value {
    using PayloadValue = std::variant<std::monostate, std::wstring, bool, uint64_t, int64_t, double>;
    using PayloadValues = std::vector<PayloadValue>;
    using Payload = std::variant<PayloadValue, PayloadValues>;

    Value(pType const &type);
    Value(Value const &);

    pType   type { TypeRegistry::void_ };
    Payload payload;

    Value() = default;

    template<typename T>
    Value(T val)
        : Value(TypeRegistry::undetermined)
    {
        fatal("Unexpected payload type");
    }

    template<std::unsigned_integral T>
    Value(T val)
        : Value(TypeRegistry::u64)
    {
        payload.emplace<PayloadValue>(PayloadValue { static_cast<uint64_t>(val) });
    }

    template<std::signed_integral T>
    Value(T val)
        : Value(TypeRegistry::i64)
    {
        payload.emplace<PayloadValue>(PayloadValue { static_cast<int64_t>(val) });
    }

    template<std::floating_point T>
    Value(T val)
        : Value(TypeRegistry::f64)
    {
        payload.emplace<PayloadValue>(PayloadValue { val });
    }

    template<>
    Value(bool val)
        : Value(TypeRegistry::string)
    {
        payload.emplace<PayloadValue>(PayloadValue { val });
    }

    template<>
    Value(std::wstring val)
        : Value(TypeRegistry::string)
    {
        payload.emplace<PayloadValue>(PayloadValue { std::move(val) });
    }

    Value &operator=(Value const &) = default;

    template<typename Func>
    Value evaluate_op(Value const &rhs, Func const &func)
    {
        return std::visit(overloads {
                              [this, &rhs, &func](PayloadValue const &lhs_payload, PayloadValue const &rhs_payload) -> Value {
                                  return std::visit(overloads {
                                                        [&func](std::integral auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                            return func(lhs_value, rhs_value);
                                                        },
                                                        [&func](std::integral auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                            return func(lhs_value, rhs_value);
                                                        },
                                                        [&func](std::floating_point auto const &lhs_value, std::floating_point auto const &rhs_value) -> Value {
                                                            return func(lhs_value, rhs_value);
                                                        },
                                                        [&func](std::floating_point auto const &lhs_value, std::integral auto const &rhs_value) -> Value {
                                                            return func(lhs_value, rhs_value);
                                                        },
                                                        [](auto const &lhs_value, auto const &rhs_value) -> Value {
                                                            fatal("Cannot apply operator to non-numbers");
                                                        } },
                                      lhs_payload, rhs_payload);
                              },
                              [](auto const &lhs_payload, auto const &rhs_payload) -> Value {
                                  fatal("Cannot apply operator to non-numbers");
                              } },
            payload, rhs.payload);
    }

    template<typename Func>
    Value evaluate_binary_op(Value const &rhs, Func const &func)
    {
        return std::visit(overloads {
                              [&func](PayloadValue const &lhs_payload, PayloadValue const &rhs_payload) -> Value {
                                  return std::visit(overloads {
                                                        [&func](std::integral auto const &lhs, std::integral auto const &rhs) -> Value {
                                                            return func(lhs, rhs);
                                                        },
                                                        [&func](auto const &lhs, auto const &rhs) -> Value {
                                                            fatal("Cannot apply operator to non-integers");
                                                        } },
                                      lhs_payload, rhs_payload);
                              },
                              [&func](auto const &lhs, auto const &rhs) -> Value {
                                  fatal("Cannot apply operator to non-integers");
                              } },
            payload, rhs.payload);
    }

    template<typename Func>
    Value evaluate_logical_op(Value const &rhs, Func const &func)
    {
        return std::visit(overloads {
                              [&func](PayloadValue const &lhs_payload, PayloadValue const &rhs_payload) -> Value {
                                  return std::visit(overloads {
                                                        [&func](bool const &lhs, bool const &rhs) -> Value {
                                                            return func(lhs, rhs);
                                                        },
                                                        [&func](auto const &lhs, auto const &rhs) -> Value {
                                                            fatal("Cannot apply operator to non-booleans");
                                                        } },
                                      lhs_payload, rhs_payload);
                              },
                              [&func](auto const &lhs, auto const &rhs) -> Value {
                                  fatal("Cannot apply operator to non-booleans");
                              } },
            payload, rhs.payload);
    }

    std::wstring to_string() const;
    bool         is_zero() const;
    Value        evaluate(Operator op, Value const &rhs);

#undef S
#define S(O) Value evaluate_##O(Value const &);
    BinOps(S)
#undef S
};

struct Scope {
    struct Interpreter           *interpreter;
    Scope                        *parent { nullptr };
    pSyntaxNode                   owner;
    std::map<std::wstring, Value> values;

    void         execute(pSyntaxNode const &node);
    Value const &value(std::wstring const &name) const;
    void         reassign(std::wstring const &name, Value value);
    pSyntaxNode  name(std::wstring const &name) const;
};

struct Interpreter {
    std::vector<Scope>          scopes;
    std::vector<Value>          stack;
    std::optional<std::wstring> break_;
    std::optional<std::wstring> continue_;

    Interpreter();
    Interpreter(Interpreter &) = delete;
    Interpreter(Interpreter &&) = delete;

    Scope &execute(pSyntaxNode const &node);
    Scope &new_scope(pSyntaxNode const &node);
};

}
