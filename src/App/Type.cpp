/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <format>
#include <limits>

#include <App/Type.h>
#include <Util/Logging.h>

namespace Arwen {

using namespace Util;

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
VoidType  VoidType::void_type {};

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
    types.emplace(L"cstring", make_type(L"string", ZeroTerminatedArray { types[L"u8"] }));
    types.emplace(L"char", make_type(L"char", TypeAlias { types[L"u32"] }));
    types.emplace(L"void", make_type(L"void", VoidType {}));
}

TypeRegistry &TypeRegistry::the()
{
    return s_registry;
}

pType TypeRegistry::slice_of(pType type)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&type](SliceType const descr) -> bool {
                               return descr.slice_of == type;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"[]{}", type->name), SliceType { type });
    types.emplace(ret->name, ret);
    return ret;
}

pType TypeRegistry::zero_terminated_array_of(pType type)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&type](ZeroTerminatedArray const &descr) -> bool {
                               return descr.array_of == type;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"[0]{}", type->name), ZeroTerminatedArray { type });
    types.emplace(ret->name, ret);
    return ret;
}

pType TypeRegistry::array_of(pType type, size_t size)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&type, &size](Array const &descr) -> bool {
                               return descr.array_of == type && descr.size == size;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"[{}]{}", size, type->name), Array { type, size });
    types.emplace(ret->name, ret);
    return ret;
}

pType TypeRegistry::optional_of(pType type)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&type](OptionalType const descr) -> bool {
                               return descr.type == type;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"{}?", type->name), OptionalType { type });
    types.emplace(ret->name, ret);
    return ret;
}

pType TypeRegistry::error_of(pType success, pType error)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&success, &error](ErrorType const descr) -> bool {
                               return descr.success == success && descr.error == error;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"{}/{}", success->name, error->name), ErrorType { success, error });
    types.emplace(ret->name, ret);
    return ret;
}

}
