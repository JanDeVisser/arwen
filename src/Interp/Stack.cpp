/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Type.h"
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <ios>
#include <sstream>

#include <Util/Align.h>
#include <Util/Arena.h>

#include <Interp/Stack.h>

namespace Arwen::Interpreter {

Stack::Stack()
    : stack(static_cast<char *>(malloc(1024 * 1024)))
    , cap(1024 * 1024)
    , top(0)
{
    assert(stack != nullptr);
    memset(stack, 0xAA, cap);
}

Stack::~Stack()
{
    free(stack);
}

void Stack::ensure(size_t size)
{
    size_t aligned = alignat(size, 8);
    if (top + aligned > cap) {
        fatal("Stack Overflow");
    }
}

intptr_t Stack::reserve(size_t size)
{
    ensure(size);
    auto offset { top };
    top += alignat(size, 8);
    return offset;
}

intptr_t Stack::push(void *ptr, size_t size)
{
    auto offset = reserve(size);
    store(ptr, offset, size);
    return offset;
}

void Stack::store(void *src, intptr_t offset, size_t size)
{
    std::stringstream bytes;
    char const       *chars = static_cast<char const *>(src);
    for (auto ix = 0; ix < size; ++ix) {
        bytes << " 0x" << std::hex << std::setw(2) << std::setfill('0') << (chars[ix] & 0xFF);
    }
    trace("Storing in stack {} bytes at {}:{}", size, offset, bytes.view());
    memcpy(stack + offset, src, size);
}

void Stack::load(void *dest, intptr_t offset, size_t size)
{
    memcpy(dest, stack + offset, size);
    std::stringstream bytes;
    char const       *chars = static_cast<char const *>(dest);
    for (auto ix = 0; ix < size; ++ix) {
        bytes << " 0x" << std::hex << std::setw(2) << std::setfill('0') << (chars[ix] & 0xFF);
    }
    trace("Loading from stack {} bytes at {}:{}", size, offset, bytes.view());
}

void Stack::discard(size_t size, size_t return_size)
{
    assert(return_size <= size);
    assert(size % 8 == 0);
    if (size != 0) {
        trace("Discarding {} byte frame from stack, sliding down {} bytes", size, return_size);
        if (return_size > 0) {
            auto aligned_return_size { alignat(return_size, 8) };
            if (aligned_return_size == size) {
                return;
            }
            store(stack + (top - aligned_return_size), top - size - aligned_return_size, return_size);
            top -= size - aligned_return_size;
        } else {
            top -= size;
        }
        trace("Top is now {}", top);
    }
}

void Stack::pop(void *dest, size_t size)
{
    auto offset = top - alignat(size, 8);
    load(dest, offset, size);
    top = offset;
    trace("Top is now {}", top);
}

void Stack::copy(intptr_t dest, intptr_t src, size_t size)
{
    std::stringstream bytes;
    char const       *chars = static_cast<char const *>(stack + src);
    for (auto ix = 0; ix < size; ++ix) {
        bytes << " 0x" << std::hex << std::setw(2) << std::setfill('0') << (chars[ix] & 0xFF);
    }
    trace("Copying in stack {} bytes from offset {} to {}:{}", size, src, dest, bytes.view());
    assert(dest + size < src || dest > src + size);
    memcpy(stack + dest, stack + src, size);
}

void Stack::copy_and_pop(intptr_t dest, size_t size)
{
    auto offset = top - alignat(size, 8);
    copy(dest, offset, size);
    top = offset;
    trace("Top is now {}", top);
}

void Stack::push_copy(intptr_t src, size_t size)
{
    push(stack + src, size);
}

template<class T>
concept numeric = std::is_integral_v<T> || std::is_floating_point_v<T>;

template<class T>
concept numeric_or_bool = std::is_integral_v<T> || std::is_floating_point_v<T> || std::is_same_v<T, bool>;

#undef S
#define S(O)                                                                              \
    template<typename LHS, typename RHS>                                                  \
    intptr_t evaluate_##O(Stack &stack, TypeDescription const &, TypeDescription const &) \
    {                                                                                     \
        UNREACHABLE();                                                                    \
    }                                                                                     \
    template<typename Operand>                                                            \
    intptr_t evaluate_unary_##O(Stack &stack, TypeDescription const &)                    \
    {                                                                                     \
        UNREACHABLE();                                                                    \
    }
Operators(S)
#undef S

    template<>
    intptr_t evaluate_unary_AddressOf<Reference>(Stack &stack, TypeDescription const &)
{
    auto operand = pop<uint64_t>(stack);
    return push<void *>(stack, stack.stack + operand);
}

template<std::integral Operand>
intptr_t evaluate_unary_BinaryInvert(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto operand = pop<Operand>(stack);
    return push<Operand>(stack, ~operand);
}

template<>
intptr_t evaluate_unary_Length<Slice>(Stack &stack, TypeDescription const &)
{
    auto operand = pop<Slice>(stack);
    return push<uint64_t>(stack, operand.size);
}

template<>
intptr_t evaluate_unary_Length<DynamicArray>(Stack &stack, TypeDescription const &)
{
    auto operand = pop<DynamicArray>(stack);
    return push<uint64_t>(stack, operand.size);
}

template<>
intptr_t evaluate_unary_Length<StaticArray>(Stack &stack, TypeDescription const &)
{
    auto operand = pop<StaticArray>(stack);
    return push<uint64_t>(stack, operand.size);
}

template<>
intptr_t evaluate_unary_LogicalInvert<bool>(Stack &stack, TypeDescription const &)
{
    auto operand = pop<bool>(stack);
    return push<bool>(stack, !operand);
}

template<numeric Operand>
intptr_t evaluate_unary_Negate(Stack &stack, TypeDescription const &)
{
    auto operand = pop<Operand>(stack);
    return push<Operand>(stack, -operand);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Add(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(lhs + rhs));
}

template<>
intptr_t evaluate_Add<DynamicArray, DynamicArray>(Stack &stack, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    auto rhs = pop<DynamicArray>(stack);
    auto lhs = pop<DynamicArray>(stack);
    auto lhs_arr_descr = std::get<DynArray>(lhs_descr);
    auto rhs_arr_descr = std::get<DynArray>(rhs_descr);
    assert(lhs_arr_descr.array_of == rhs_arr_descr.array_of);
    DynamicArray ret {};
    dynarr_append(&ret, AS_SLICE(lhs), lhs_arr_descr.array_of->size_of());
    dynarr_append(&ret, AS_SLICE(rhs), lhs_arr_descr.array_of->size_of());
    return push<DynamicArray>(stack, ret);
}

template<>
intptr_t evaluate_Add<Slice, Slice>(Stack &stack, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    auto rhs = pop<Slice>(stack);
    auto lhs = pop<Slice>(stack);
    auto lhs_slice_descr = std::get<SliceType>(lhs_descr);
    auto rhs_slice_descr = std::get<SliceType>(rhs_descr);
    assert(lhs_slice_descr.slice_of == rhs_slice_descr.slice_of);
    DynamicArray ret {};
    dynarr_append(&ret, lhs, lhs_slice_descr.slice_of->size_of());
    dynarr_append(&ret, rhs, lhs_slice_descr.slice_of->size_of());
    return push<DynamicArray>(stack, ret);
}

template<>
intptr_t evaluate_Add<Slice, DynamicArray>(Stack &stack, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    auto rhs = pop<DynamicArray>(stack);
    auto lhs = pop<Slice>(stack);
    auto lhs_slice_descr = std::get<SliceType>(lhs_descr);
    auto rhs_arr_descr = std::get<DynArray>(rhs_descr);
    assert(lhs_slice_descr.slice_of == rhs_arr_descr.array_of);
    DynamicArray ret {};
    dynarr_append(&ret, lhs, lhs_slice_descr.slice_of->size_of());
    dynarr_append(&ret, AS_SLICE(rhs), lhs_slice_descr.slice_of->size_of());
    return push<DynamicArray>(stack, ret);
}

template<>
intptr_t evaluate_Add<DynamicArray, Slice>(Stack &stack, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    auto rhs = pop<Slice>(stack);
    auto lhs = pop<DynamicArray>(stack);
    auto lhs_arr_descr = std::get<DynArray>(lhs_descr);
    auto rhs_slice_descr = std::get<SliceType>(rhs_descr);
    assert(rhs_slice_descr.slice_of == lhs_arr_descr.array_of);
    DynamicArray ret {};
    dynarr_append(&ret, AS_SLICE(lhs), rhs_slice_descr.slice_of->size_of());
    dynarr_append(&ret, rhs, rhs_slice_descr.slice_of->size_of());
    return push<DynamicArray>(stack, ret);
}

template<std::integral LHS, std::integral RHS>
intptr_t evaluate_BinaryAnd(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, lhs & rhs);
}

template<std::integral LHS, std::integral RHS>
intptr_t evaluate_BinaryOr(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, lhs | rhs);
}

template<std::integral LHS, std::integral RHS>
intptr_t evaluate_BinaryXor(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, lhs ^ rhs);
}

template<numeric_or_bool LHS, numeric_or_bool RHS = LHS>
intptr_t evaluate_Equals(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs == rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Greater(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs > rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_GreaterEqual(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs >= rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Multiply(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(lhs * rhs));
}

template<numeric_or_bool LHS, numeric_or_bool RHS = LHS>
intptr_t evaluate_NotEqual(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs != rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Less(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs < rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_LessEqual(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<bool>(stack, lhs <= rhs);
}

template<>
intptr_t evaluate_LogicalAnd<bool, bool>(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<bool>(stack);
    auto lhs = pop<bool>(stack);
    return push<bool>(stack, lhs && rhs);
}

template<>
intptr_t evaluate_LogicalOr<bool, bool>(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<bool>(stack);
    auto lhs = pop<bool>(stack);
    return push<bool>(stack, lhs || rhs);
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Divide(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    if (rhs == 0) {
        fatal("Division by zero");
    }
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(lhs / rhs));
}

template<std::integral LHS, std::integral RHS>
intptr_t evaluate_Modulo(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    if (rhs == 0) {
        fatal("Division by zero");
    }
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(lhs % rhs));
}

template<numeric LHS, numeric RHS>
    requires(std::is_floating_point_v<LHS> || std::is_floating_point_v<RHS>)
intptr_t evaluate_Modulo(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    if (rhs == 0) {
        fatal("Division by zero");
    }
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(fmod(lhs, rhs)));
}

template<numeric LHS, numeric RHS = LHS>
intptr_t evaluate_Subtract(Stack &stack, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(stack);
    auto lhs = pop<LHS>(stack);
    return push<LHS>(stack, static_cast<LHS>(lhs - rhs));
}

template<typename LHS, typename RHS = LHS>
intptr_t evaluate_op(Stack &stack, Operator op, TypeDescription const &lhs, TypeDescription const &rhs)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_##O<LHS, RHS>(stack, lhs, rhs);
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

template<typename Operand>
intptr_t evaluate_unary_op(Stack &stack, Operator op, TypeDescription const &descr)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return evaluate_unary_##O<Operand>(stack, descr);
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

template<typename OperandDescr>
intptr_t evaluate(Stack &, OperandDescr const &, Operator)
{
    UNREACHABLE();
}

template<>
intptr_t evaluate(Stack &stack, IntType const &operand, Operator op)
{
    switch (operand.width_bits + static_cast<int>(operand.is_signed)) {
#undef S
#define S(W)                                                       \
    case W:                                                        \
        return evaluate_unary_op<uint##W##_t>(stack, op, operand); \
    case W + 1:                                                    \
        return evaluate_unary_op<int##W##_t>(stack, op, operand);
        BitWidths(S)
#undef S
            default : UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, FloatType const &operand, Operator op)
{
    switch (operand.width_bits) {
    case 32:
        return evaluate_unary_op<float>(stack, op, operand);
    case 64:
        return evaluate_unary_op<double>(stack, op, operand);
    default:
        UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, BoolType const &operand, Operator op)
{
    return evaluate_unary_op<bool>(stack, op, operand);
}

template<>
intptr_t evaluate(Stack &stack, SliceType const &operand, Operator op)
{
    return evaluate_unary_op<Slice>(stack, op, operand);
}

template<>
intptr_t evaluate(Stack &stack, ReferenceType const &operand, Operator op)
{
    return evaluate_unary_op<Reference>(stack, op, operand);
}

template<>
intptr_t evaluate(Stack &stack, DynArray const &operand, Operator op)
{
    return evaluate_unary_op<DynamicArray>(stack, op, operand);
}

template<>
intptr_t evaluate(Stack &stack, Array const &operand, Operator op)
{
    return evaluate_unary_op<StaticArray>(stack, op, operand);
}

template<typename LHSDescr, typename RHSDescr>
intptr_t evaluate(Stack &stack, LHSDescr const &lhs, Operator op, RHSDescr const &rhs)
{
    return evaluate_op<LHSDescr, RHSDescr>(stack, op, lhs, rhs);
}

template<typename LHSDescr>
intptr_t evaluate(Stack &stack, LHSDescr const &lhs, Operator op, VoidType const &)
{
    return evaluate(stack, lhs, op);
}

template<>
intptr_t evaluate(Stack &stack, IntType const &lhs, Operator op, IntType const &rhs)
{
    switch (lhs.width_bits + static_cast<int>(lhs.is_signed)) {
    case 8:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                           \
    case W:                                                            \
        return evaluate_op<uint8_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                        \
        return evaluate_op<uint8_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 9:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                          \
    case W:                                                           \
        return evaluate_op<int8_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                       \
        return evaluate_op<int8_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 16:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                            \
    case W:                                                             \
        return evaluate_op<uint16_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                         \
        return evaluate_op<uint16_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 17:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                           \
    case W:                                                            \
        return evaluate_op<int16_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                        \
        return evaluate_op<int16_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 32:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                            \
    case W:                                                             \
        return evaluate_op<uint32_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                         \
        return evaluate_op<uint32_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 33:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                           \
    case W:                                                            \
        return evaluate_op<int32_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                        \
        return evaluate_op<int32_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 64:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                            \
    case W:                                                             \
        return evaluate_op<uint64_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                         \
        return evaluate_op<uint64_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    case 65:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                           \
    case W:                                                            \
        return evaluate_op<int64_t, uint##W##_t>(stack, op, lhs, rhs); \
    case W + 1:                                                        \
        return evaluate_op<int64_t, int##W##_t>(stack, op, lhs, rhs);
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
    default:
        UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, IntType const &lhs, Operator op, FloatType const &rhs)
{
    switch (lhs.width_bits + static_cast<int>(lhs.is_signed)) {
    case 8:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<uint8_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<uint8_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 9:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<int8_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<int8_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 16:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<uint16_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<uint16_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 17:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<int16_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<int16_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 32:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<uint32_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<uint32_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 33:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<int32_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<int32_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 64:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<uint64_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<uint64_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 65:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<int64_t, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<int64_t, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    default:
        UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, FloatType const &lhs, Operator op, IntType const &rhs)
{
    switch (lhs.width_bits) {
    case 32:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
        case 8:
            return evaluate_op<float, uint8_t>(stack, op, lhs, rhs);
        case 9:
            return evaluate_op<float, int8_t>(stack, op, lhs, rhs);
        case 16:
            return evaluate_op<float, uint16_t>(stack, op, lhs, rhs);
        case 17:
            return evaluate_op<float, int16_t>(stack, op, lhs, rhs);
        case 32:
            return evaluate_op<float, float>(stack, op, lhs, rhs);
        case 33:
            return evaluate_op<float, int32_t>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<float, uint64_t>(stack, op, lhs, rhs);
        case 65:
            return evaluate_op<float, int64_t>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 64:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
        case 8:
            return evaluate_op<double, uint8_t>(stack, op, lhs, rhs);
        case 9:
            return evaluate_op<double, int8_t>(stack, op, lhs, rhs);
        case 16:
            return evaluate_op<double, uint16_t>(stack, op, lhs, rhs);
        case 17:
            return evaluate_op<double, int16_t>(stack, op, lhs, rhs);
        case 32:
            return evaluate_op<double, uint32_t>(stack, op, lhs, rhs);
        case 33:
            return evaluate_op<double, int32_t>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<double, uint64_t>(stack, op, lhs, rhs);
        case 65:
            return evaluate_op<double, int64_t>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    default:
        UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, FloatType const &lhs, Operator op, FloatType const &rhs)
{
    switch (lhs.width_bits) {
    case 32:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<float, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<float, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    case 64:
        switch (rhs.width_bits) {
        case 32:
            return evaluate_op<double, float>(stack, op, lhs, rhs);
        case 64:
            return evaluate_op<double, double>(stack, op, lhs, rhs);
        default:
            UNREACHABLE();
        }
    default:
        UNREACHABLE();
    }
}

template<>
intptr_t evaluate(Stack &stack, BoolType const &lhs, Operator op, BoolType const &rhs)
{
    return evaluate_op<bool, bool>(stack, op, lhs, rhs);
}

template<>
intptr_t evaluate(Stack &stack, SliceType const &lhs, Operator op, SliceType const &rhs)
{
    return evaluate_op<Slice, Slice>(stack, op, lhs, rhs);
}

template<>
intptr_t evaluate(Stack &stack, DynArray const &lhs, Operator op, DynArray const &rhs)
{
    return evaluate_op<DynamicArray, DynamicArray>(stack, op, lhs, rhs);
}

template<>
intptr_t evaluate(Stack &stack, DynArray const &lhs, Operator op, SliceType const &rhs)
{
    return evaluate_op<DynamicArray, Slice>(stack, op, lhs, rhs);
}

template<>
intptr_t evaluate(Stack &stack, DynArray const &lhs, Operator op, Array const &rhs)
{
    return evaluate_op<DynamicArray, StaticArray>(stack, op, lhs, rhs);
}

intptr_t Stack::evaluate(pType const &lhs_type, Operator op, pType const &rhs_type)
{
    return std::visit(
        [this, op](auto const &lhs_descr, auto const &rhs_descr) -> intptr_t {
            return Interpreter::evaluate(*this, lhs_descr, op, rhs_descr);
        },
        lhs_type->description, rhs_type->description);
}

intptr_t Stack::evaluate_unary(pType const &operand, Operator op)
{
    return std::visit(
        [this, op](auto const &descr) -> intptr_t {
            return Interpreter::evaluate(*this, descr, op);
        },
        operand->description);
}

}
