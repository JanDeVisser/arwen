/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <cstdlib>
#include <type_traits>

#include <Util/Align.h>
#include <Util/Arena.h>
#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen::Interpreter {

using namespace Arwen;

struct Void {
};

struct Reference {
    intptr_t address;
    pType    type;
};

struct Stack {
    char    *stack;
    intptr_t cap;
    intptr_t top;

    Stack();
    ~Stack();
    Stack(Stack &) = delete;
    Stack(Stack &&) = delete;

    void     ensure(size_t size);
    intptr_t reserve(size_t size);
    intptr_t push(void *ptr, size_t size);
    void     store(void *src, intptr_t offset, size_t size);
    void     load(void *dest, intptr_t offset, size_t size);
    void     discard(size_t size, size_t return_size = 0);
    void     pop(void *dest, size_t size);
    void     copy(intptr_t dest, intptr_t src, size_t size);
    void     copy_and_pop(intptr_t dest, size_t size);
    void     push_copy(intptr_t src, size_t size);
    intptr_t evaluate(pType const &lhs_type, Operator op, pType const &rhs_type);
    intptr_t evaluate_unary(pType const &operand, Operator op);
};

template<typename T>
void store(Stack &stack, T val, intptr_t offset)
{
    stack.store(reinterpret_cast<void *>(&val), offset, sizeof(T));
}

template<>
inline void store(Stack &stack, Void val, intptr_t offset)
{
}

template<typename T>
intptr_t push(Stack &stack, T val)
{
    return stack.push(reinterpret_cast<void *>(&val), sizeof(T));
}

template<>
inline intptr_t push(Stack &stack, Void val)
{
    return stack.top;
}

template<>
inline intptr_t push(Stack &stack, Value val)
{
    return std::visit(
        overloads {
            [&stack, &val](IntType const &descr) -> intptr_t {
                switch (descr.width_bits) {
                case 8:
                    return (descr.is_signed) ? push<int8_t>(stack, as<int8_t>(val)) : push<uint8_t>(stack, as<uint8_t>(val));
                case 16:
                    return (descr.is_signed) ? push<int16_t>(stack, as<int16_t>(val)) : push<uint16_t>(stack, as<uint32_t>(val));
                case 32:
                    return (descr.is_signed) ? push<int32_t>(stack, as<int32_t>(val)) : push<uint32_t>(stack, as<uint32_t>(val));
                case 64:
                    return (descr.is_signed) ? push<int64_t>(stack, as<int64_t>(val)) : push<uint64_t>(stack, as<uint64_t>(val));
                default:
                    UNREACHABLE();
                }
            },
            [&stack, &val](FloatType const &descr) -> intptr_t {
                switch (descr.width_bits) {
                case 32:
                    return push<float>(stack, as<float>(val));
                case 64:
                    return push<double>(stack, as<double>(val));
                default:
                    UNREACHABLE();
                }
            },
            [&stack, &val](BoolType const &) -> intptr_t {
                return push<bool>(stack, as<bool>(val));
            },
            [&stack, &val](SliceType const &) -> intptr_t {
                return push<Slice>(stack, as<Slice>(val));
            },
            [&stack, &val](VoidType const &) -> intptr_t {
                return push<Void>(stack, Void {});
            },
            [](auto const &) -> intptr_t {
                NYI("push<Value>");
            } },
        val.type->description);
}

template<typename T>
T get(Stack &stack, intptr_t offset)
{
    T ret;
    stack.load(reinterpret_cast<void *>(&ret), offset, sizeof(T));
}

template<>
inline Void get(Stack &stack, intptr_t offset)
{
    return Void {};
}

template<typename T>
T pop(Stack &stack)
{
    T ret;
    stack.pop(reinterpret_cast<void *>(&ret), sizeof(T));
    return ret;
}

template<>
inline Void pop(Stack &stack)
{
    return Void {};
}

template<typename T>
void copy(Stack &stack, intptr_t dest, intptr_t src)
{
    stack.copy(dest, src, sizeof(T));
}

template<>
inline void copy<Void>(Stack &stack, intptr_t dest, intptr_t src)
{
}

template<typename T>
void copy_and_pop(Stack &stack, intptr_t dest)
{
    stack.copy_and_pop(dest, sizeof(T));
}

template<>
inline void copy_and_pop<Void>(Stack &stack, intptr_t dest)
{
}

template<typename T>
void push_copy(Stack &stack, intptr_t src)
{
    stack.push_copy(src, sizeof(T));
}

template<>
inline void push_copy<Void>(Stack &stack, intptr_t src)
{
}

}
