/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <cstdint>
#include <type_traits>

#include <App/Type.h>

#include <Arch/Linux/x86_64/X86_64.h>

namespace Arwen::X86_64 {

using namespace Arwen;

void relational_op(Function &function, std::wstring branch)
{
    function.add_text(std::format(
        LR"(
    cmp     x0,x1
    {}      1f
    mov     w0,wzr
    b       2f
1:
    mov     w0,#0x01
2:
)",
        branch));
    push<bool>(function);
}

void relational_eq_op(Function &function, std::wstring branch)
{
    function.add_text(std::format(
        LR"(
    cmp     x0,x1
    {}      1f
    b.eq    1f
    mov     w0,wzr
    b       2f
1:
    mov     w0,#0x01
2:
)",
        branch));
    push<bool>(function);
}

#undef S
#define S(O)                                                                                \
    template<typename LHS, typename RHS>                                                    \
    void generate_##O(Function &function, TypeDescription const &, TypeDescription const &) \
    {                                                                                       \
        UNREACHABLE();                                                                      \
    }                                                                                       \
    template<typename Operand>                                                              \
    void generate_unary_##O(Function &function, TypeDescription const &)                    \
    {                                                                                       \
        UNREACHABLE();                                                                      \
    }
Operators(S)
#undef S

    //     template<>
    //     void generate_unary_AddressOf<Reference>(Function &function, TypeDescription const &)
    // {
    //     auto operand = pop<uint64_t>(function);
    //     push<void *>(function, function.function + operand);
    // }

    template<std::integral Operand>
    void generate_unary_BinaryInvert(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<Operand>(function);
    function.add_instruction(L"mvn", L"x0,x0");
    push<Operand>(function);
}

template<>
void generate_unary_Length<Slice>(Function &function, TypeDescription const &)
{
    pop<Slice>(function);
    function.add_instruction(L"mov", L"x0,x1"); // a is 0b00000001 (a was true) or 0b00000000 (a was false
    push<uint64_t>(function);
}

template<>
void generate_unary_Length<DynamicArray>(Function &function, TypeDescription const &)
{
    pop<DynamicArray>(function);
    function.add_instruction(L"mov", L"x0,x1"); // a is 0b00000001 (a was true) or 0b00000000 (a was false
    push<uint64_t>(function);
}

template<>
void generate_unary_Length<StaticArray>(Function &function, TypeDescription const &)
{
    pop<StaticArray>(function);
    function.add_instruction(L"mov", L"x0,x1"); // a is 0b00000001 (a was true) or 0b00000000 (a was false
    push<uint64_t>(function);
}

template<>
void generate_unary_LogicalInvert<bool>(Function &function, TypeDescription const &)
{
    pop<bool>(function);
    function.add_instruction(L"eor", L"w0,w0,#0x01"); // a is 0b00000001 (a was true) or 0b00000000 (a was false
    push<bool>(function);
}

template<std::signed_integral Operand>
requires (!std::is_same_v<Operand, int64_t>)
void generate_unary_Negate(Function &function, TypeDescription const &)
{
    pop<Operand>(function);
    function.add_instruction(L"neg", L"w0,w0");
    push<Operand>(function);
}

template<>
void generate_unary_Negate<int64_t>(Function &function, TypeDescription const &)
{
    pop<int64_t>(function);
    function.add_instruction(L"neg", L"x0,x0");
    push<int64_t>(function);
}

template<std::integral LHS, std::integral RHS = LHS>
void generate_Add(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"add", L"x0,x0,x1");
    push<LHS>(function);
}

template<>
void generate_Add<DynamicArray, DynamicArray>(Function &function, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    pop<DynamicArray>(function, 19);
    pop<DynamicArray>(function, 1);
    function.add_text(
        LR"(
mov     x0,xzr
mov     x1,xzr
stp     x0,x1[sp,-16]!
stp     x0,x1[sp,-16]!
mov     x0,sp
mov     x3,4
bl      dynarr_append
mov     x0,sp
mov     x3,4
mov     x1,x19
mov     x2,x20
bl      dynarr_append
ldp     x0,x1,[sp],16
ldr     x2,[sp],16)");
    push<DynamicArray>(function);
}

template<>
void generate_Add<Slice, Slice>(Function &function, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    pop<DynamicArray>(function, 19);
    pop<Slice>(function, 1);
    function.add_text(
        LR"(
mov     x0,xzr
mov     x4,xzr
stp     x0,x4[sp,-16]!
stp     x0,x4[sp,-16]!
mov     x0,sp
mov     x3,4
bl      dynarr_append
mov     x0,sp
mov     x3,4
mov     x1,x19
mov     x2,x20
bl      dynarr_append
ldp     x0,x1,[sp],16
ldr     x2,[sp],16)");
    push<DynamicArray>(function);
}

template<>
void generate_Add<Slice, DynamicArray>(Function &function, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    pop<DynamicArray>(function, 19);
    pop<Slice>(function, 1);
    function.add_text(
        LR"(
mov     x0,xzr
mov     x1,xzr
stp     x0,x1[sp,-16]!
stp     x0,x1[sp,-16]!
mov     x0,sp
mov     x3,4
bl      dynarr_append
mov     x0,sp
mov     x3,4
mov     x1,x19
mov     x2,x20
bl      dynarr_append
ldp     x0,x1,[sp],16
ldr     x2,[sp],16)");
    push<DynamicArray>(function);
}

template<>
void generate_Add<DynamicArray, Slice>(Function &function, TypeDescription const &lhs_descr, TypeDescription const &rhs_descr)
{
    pop<Slice>(function, 19);
    pop<DynamicArray>(function, 1);
    function.add_text(
        LR"(
mov     x0,xzr
mov     x1,xzr
stp     x0,x1[sp,-16]!
stp     x0,x1[sp,-16]!
mov     x0,sp
mov     x3,4
bl      dynarr_append
mov     x0,sp
mov     x3,4
mov     x1,x19
mov     x2,x20
bl      dynarr_append
ldp     x0,x1,[sp],16
ldr     x2,[sp],16)");
    push<DynamicArray>(function);
}

template<std::integral LHS, std::integral RHS>
void generate_BinaryAnd(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"and", L"x0,x0,x1");
    push<LHS>(function);
}

template<std::integral LHS, std::integral RHS>
void generate_BinaryOr(Function &function, TypeDescription const &, TypeDescription const &)
{
    auto rhs = pop<RHS>(function, 1);
    auto lhs = pop<LHS>(function, 0);
    function.add_instruction(L"orr", L"x0,x0,x1");
    push<LHS>(function);
}

template<std::integral LHS, std::integral RHS>
void generate_BinaryXor(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"eor", L"x0,x0,x1");
    push<LHS>(function);
}

template<numeric_or_bool LHS, numeric_or_bool RHS = LHS>
void generate_Equals(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_op(function, L"b.eq");
}

template<numeric LHS, numeric RHS = LHS>
void generate_Greater(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_op(function, L"b.gt");
}

template<numeric LHS, numeric RHS = LHS>
void generate_GreaterEqual(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_eq_op(function, L"b.gt");
}

template<numeric LHS, numeric RHS = LHS>
void generate_Multiply(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"mul", L"x0,x0,x1");
    push<LHS>(function);
}

template<numeric_or_bool LHS, numeric_or_bool RHS = LHS>
void generate_NotEqual(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_op(function, L"b.ne");
}

template<numeric LHS, numeric RHS = LHS>
void generate_Less(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_op(function, L"b.lt");
}

template<numeric LHS, numeric RHS = LHS>
void generate_LessEqual(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    relational_eq_op(function, L"b.lt");
}

template<>
void generate_LogicalAnd<bool, bool>(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<bool>(function, 1);
    pop<bool>(function, 0);
    function.add_instruction(L"and", L"w0,w0,w1");
    push<bool>(function);
}

template<>
void generate_LogicalOr<bool, bool>(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<bool>(function, 1);
    pop<bool>(function, 0);
    function.add_instruction(L"orr", L"w0,w0,w1");
    push<bool>(function);
}

template<std::signed_integral LHS, std::signed_integral RHS = LHS>
void generate_Divide(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"sdiv", L"x0,x0,x1");
    push<LHS>(function);
}

template<std::unsigned_integral LHS, std::unsigned_integral RHS = LHS>
void generate_Divide(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"udiv", L"x0,x0,x1");
    push<LHS>(function);
}

template<std::signed_integral LHS, std::signed_integral RHS>
void generate_Modulo(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"sdiv", L"x2,x0,x1");
    function.add_instruction(L"msub", L"x0,x2,x1,x0");
    push<LHS>(function);
}

template<std::unsigned_integral LHS, std::unsigned_integral RHS>
void generate_Modulo(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"udiv", L"x2,x0,x1");
    function.add_instruction(L"msub", L"x0,x2,x1,x0");
    push<LHS>(function);
}

template<numeric LHS, numeric RHS>
    requires(std::is_floating_point_v<LHS> || std::is_floating_point_v<RHS>)
void generate_Modulo(Function &function, TypeDescription const &, TypeDescription const &)
{
    NYI("float % float");
}

template<numeric LHS, numeric RHS = LHS>
void generate_Subtract(Function &function, TypeDescription const &, TypeDescription const &)
{
    pop<RHS>(function, 1);
    pop<LHS>(function, 0);
    function.add_instruction(L"sub", L"x0,x0,x1");
    push<LHS>(function);
}

template<typename LHS, typename RHS = LHS>
void generate_op(Function &function, Operator op, TypeDescription const &lhs, TypeDescription const &rhs)
{
    switch (op) {
#undef S
#define S(O)                                        \
    case Operator::O:                               \
        generate_##O<LHS, RHS>(function, lhs, rhs); \
        break;
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

template<typename Operand>
void generate_unary_op(Function &function, Operator op, TypeDescription const &descr)
{
    switch (op) {
#undef S
#define S(O)                                          \
    case Operator::O:                                 \
        generate_unary_##O<Operand>(function, descr); \
        break;
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

template<typename OperandDescr>
void generate_unary(Function &, OperandDescr const &, Operator)
{
    UNREACHABLE();
}

template<>
void generate_unary(Function &function, IntType const &operand, Operator op)
{
    switch (operand.width_bits + static_cast<int>(operand.is_signed)) {
#undef S
#define S(W)                                                   \
    case W:                                                    \
        generate_unary_op<uint##W##_t>(function, op, operand); \
        break;                                                 \
    case W + 1:                                                \
        generate_unary_op<int##W##_t>(function, op, operand);  \
        break;
        BitWidths(S)
#undef S
            default : UNREACHABLE();
    }
}

template<>
void generate_unary(Function &function, FloatType const &operand, Operator op)
{
    switch (operand.width_bits) {
    case 32:
        generate_unary_op<float>(function, op, operand);
        break;
    case 64:
        generate_unary_op<double>(function, op, operand);
        break;
    default:
        UNREACHABLE();
    }
}

template<>
void generate_unary(Function &function, BoolType const &operand, Operator op)
{
    generate_unary_op<bool>(function, op, operand);
}

template<>
void generate_unary(Function &function, SliceType const &operand, Operator op)
{
    generate_unary_op<Slice>(function, op, operand);
}

// template<>
// void generate_unary(Function &function, ReferenceType const &operand, Operator op)
// {
//     generate_unary_op<Reference>(function, op, operand);
// }

template<>
void generate_unary(Function &function, DynArray const &operand, Operator op)
{
    generate_unary_op<DynamicArray>(function, op, operand);
}

template<>
void generate_unary(Function &function, Array const &operand, Operator op)
{
    generate_unary_op<StaticArray>(function, op, operand);
}

template<typename LHSDescr, typename RHSDescr>
void generate(Function &function, LHSDescr const &lhs, Operator op, RHSDescr const &rhs)
{
    generate_op<LHSDescr, RHSDescr>(function, op, lhs, rhs);
}

template<typename LHSDescr>
void generate(Function &function, LHSDescr const &lhs, Operator op, VoidType const &)
{
    generate_unary(function, lhs, op);
}

template<>
void generate(Function &function, IntType const &lhs, Operator op, IntType const &rhs)
{
    switch (lhs.width_bits + static_cast<int>(lhs.is_signed)) {
    case 8:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                       \
    case W:                                                        \
        generate_op<uint8_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                     \
    case W + 1:                                                    \
        generate_op<uint8_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 9:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                      \
    case W:                                                       \
        generate_op<int8_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                    \
    case W + 1:                                                   \
        generate_op<int8_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 16:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                        \
    case W:                                                         \
        generate_op<uint16_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                      \
    case W + 1:                                                     \
        generate_op<uint16_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 17:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                       \
    case W:                                                        \
        generate_op<int16_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                     \
    case W + 1:                                                    \
        generate_op<int16_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 32:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                        \
    case W:                                                         \
        generate_op<uint32_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                      \
    case W + 1:                                                     \
        generate_op<uint32_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 33:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                       \
    case W:                                                        \
        generate_op<int32_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                     \
    case W + 1:                                                    \
        generate_op<int32_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 64:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                        \
    case W:                                                         \
        generate_op<uint64_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                      \
    case W + 1:                                                     \
        generate_op<uint64_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    case 65:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
#undef S
#define S(W)                                                       \
    case W:                                                        \
        generate_op<int64_t, uint##W##_t>(function, op, lhs, rhs); \
        break;                                                     \
    case W + 1:                                                    \
        generate_op<int64_t, int##W##_t>(function, op, lhs, rhs);  \
        break;
            BitWidths(S)
#undef S
                default : UNREACHABLE();
        }
        break;
    default:
        UNREACHABLE();
    }
}

template<>
void generate(Function &function, IntType const &lhs, Operator op, FloatType const &rhs)
{
    switch (lhs.width_bits + static_cast<int>(lhs.is_signed)) {
    case 8:
        switch (rhs.width_bits) {
        case 32:
            generate_op<uint8_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<uint8_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 9:
        switch (rhs.width_bits) {
        case 32:
            generate_op<int8_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<int8_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 16:
        switch (rhs.width_bits) {
        case 32:
            generate_op<uint16_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<uint16_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 17:
        switch (rhs.width_bits) {
        case 32:
            generate_op<int16_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<int16_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 32:
        switch (rhs.width_bits) {
        case 32:
            generate_op<uint32_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<uint32_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 33:
        switch (rhs.width_bits) {
        case 32:
            generate_op<int32_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<int32_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 64:
        switch (rhs.width_bits) {
        case 32:
            generate_op<uint64_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<uint64_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 65:
        switch (rhs.width_bits) {
        case 32:
            generate_op<int64_t, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<int64_t, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    default:
        UNREACHABLE();
    }
}

template<>
void generate(Function &function, FloatType const &lhs, Operator op, IntType const &rhs)
{
    switch (lhs.width_bits) {
    case 32:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
        case 8:
            generate_op<float, uint8_t>(function, op, lhs, rhs);
            break;
        case 9:
            generate_op<float, int8_t>(function, op, lhs, rhs);
            break;
        case 16:
            generate_op<float, uint16_t>(function, op, lhs, rhs);
            break;
        case 17:
            generate_op<float, int16_t>(function, op, lhs, rhs);
            break;
        case 32:
            generate_op<float, float>(function, op, lhs, rhs);
            break;
        case 33:
            generate_op<float, int32_t>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<float, uint64_t>(function, op, lhs, rhs);
            break;
        case 65:
            generate_op<float, int64_t>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 64:
        switch (rhs.width_bits + static_cast<int>(rhs.is_signed)) {
        case 8:
            generate_op<double, uint8_t>(function, op, lhs, rhs);
            break;
        case 9:
            generate_op<double, int8_t>(function, op, lhs, rhs);
            break;
        case 16:
            generate_op<double, uint16_t>(function, op, lhs, rhs);
            break;
        case 17:
            generate_op<double, int16_t>(function, op, lhs, rhs);
            break;
        case 32:
            generate_op<double, uint32_t>(function, op, lhs, rhs);
            break;
        case 33:
            generate_op<double, int32_t>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<double, uint64_t>(function, op, lhs, rhs);
            break;
        case 65:
            generate_op<double, int64_t>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    default:
        UNREACHABLE();
    }
}

template<>
void generate(Function &function, FloatType const &lhs, Operator op, FloatType const &rhs)
{
    switch (lhs.width_bits) {
    case 32:
        switch (rhs.width_bits) {
        case 32:
            generate_op<float, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<float, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    case 64:
        switch (rhs.width_bits) {
        case 32:
            generate_op<double, float>(function, op, lhs, rhs);
            break;
        case 64:
            generate_op<double, double>(function, op, lhs, rhs);
            break;
        default:
            UNREACHABLE();
        }
        break;
    default:
        UNREACHABLE();
    }
}

template<>
void generate(Function &function, BoolType const &lhs, Operator op, BoolType const &rhs)
{
    generate_op<bool, bool>(function, op, lhs, rhs);
}

template<>
void generate(Function &function, SliceType const &lhs, Operator op, SliceType const &rhs)
{
    generate_op<Slice, Slice>(function, op, lhs, rhs);
}

template<>
void generate(Function &function, DynArray const &lhs, Operator op, DynArray const &rhs)
{
    generate_op<DynamicArray, DynamicArray>(function, op, lhs, rhs);
}

template<>
void generate(Function &function, DynArray const &lhs, Operator op, SliceType const &rhs)
{
    generate_op<DynamicArray, Slice>(function, op, lhs, rhs);
}

template<>
void generate(Function &function, DynArray const &lhs, Operator op, Array const &rhs)
{
    generate_op<DynamicArray, StaticArray>(function, op, lhs, rhs);
}

void generate_binop(Function &function, pType const &lhs_type, Operator op, pType const &rhs_type)
{
    std::visit(
        [&function, op](auto const &lhs_descr, auto const &rhs_descr) -> void {
            generate(function, lhs_descr, op, rhs_descr);
        },
        lhs_type->description, rhs_type->description);
}

void generate_unary(Function &function, pType const &operand, Operator op)
{
    std::visit(
        [&function, op](auto const &descr) -> void {
            generate_unary(function, descr, op);
        },
        operand->description);
}

}
