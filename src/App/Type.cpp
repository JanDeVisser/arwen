/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Type.h>
#include <cstdint>
#include <limits>

namespace Arwen {

IntType   IntType::u8 { false, 8, std::numeric_limits<uint8_t>::max(), std::numeric_limits<uint8_t>::min() };
IntType   IntType::u16 { false, 16, std::numeric_limits<uint16_t>::max(), std::numeric_limits<uint16_t>::min() };
IntType   IntType::u32 { false, 32, std::numeric_limits<uint32_t>::max(), std::numeric_limits<uint32_t>::min() };
IntType   IntType::u64 { false, 64, std::numeric_limits<uint64_t>::max(), std::numeric_limits<uint64_t>::min() };
IntType   IntType::i8 { true, 8, std::numeric_limits<int8_t>::max(), std::numeric_limits<int8_t>::min() };
IntType   IntType::i16 { true, 16, std::numeric_limits<int16_t>::max(), std::numeric_limits<int16_t>::min() };
IntType   IntType::i32 { true, 32, std::numeric_limits<int32_t>::max(), std::numeric_limits<int32_t>::min() };
IntType   IntType::i64 { true, 64, std::numeric_limits<int64_t>::max(), std::numeric_limits<int64_t>::min() };
FloatType FloatType::f32 { 32 };
FloatType FloatType::f64 { 64 };
BoolType  BoolType::boolean {};

TypeRegistry TypeRegistry::s_registry {};

TypeRegistry::TypeRegistry()
{
    types.emplace(L"u8", make_type(L"u8", IntType::u8));
    types.emplace(L"u16", make_type(L"u16", IntType::u16));
    types.emplace(L"u32", make_type(L"u32", IntType::u32));
    types.emplace(L"u64", make_type(L"u64", IntType::u64));
    types.emplace(L"i8", make_type(L"i8", IntType::i8));
    types.emplace(L"i16", make_type(L"i16", IntType::i16));
    types.emplace(L"i32", make_type(L"i32", IntType::i32));
    types.emplace(L"i64", make_type(L"i64", IntType::i64));
    types.emplace(L"f32", make_type(L"f32", FloatType::f32));
    types.emplace(L"f64", make_type(L"f64", FloatType::f64));
    types.emplace(L"bool", make_type(L"bool", BoolType::boolean));
    types.emplace(L"string", make_type(L"string", SliceType { types[L"u32"] }));
    types.emplace(L"char", make_type(L"char", TypeAlias { types[L"u32"] }));
}

TypeRegistry &TypeRegistry::the()
{
    return s_registry;
}

}
