/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "Util/TokenLocation.h"
#include <cstdint>
#include <format>
#include <limits>

#include <Util/Logging.h>
#include <Util/StringUtil.h>

#include <App/Type.h>
#include <string_view>

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

TypeRegistry TypeRegistry::s_registry {};

pType TypeRegistry::u8 { make_type(L"u8", IntType::u8) };
pType TypeRegistry::u16 { make_type(L"u16", IntType::u16) };
pType TypeRegistry::u32 { make_type(L"u32", IntType::u32) };
pType TypeRegistry::u64 { make_type(L"u64", IntType::u64) };
pType TypeRegistry::i8 { make_type(L"i8", IntType::i8) };
pType TypeRegistry::i16 { make_type(L"i16", IntType::i16) };
pType TypeRegistry::i32 { make_type(L"i32", IntType::i32) };
pType TypeRegistry::i64 { make_type(L"i64", IntType::i64) };
pType TypeRegistry::f32 { make_type(L"f32", FloatType::f32) };
pType TypeRegistry::f64 { make_type(L"f64", FloatType::f64) };
pType TypeRegistry::boolean { make_type(L"bool", BoolType {}) };
pType TypeRegistry::string { make_type(L"string", SliceType { TypeRegistry::u32 }) };
pType TypeRegistry::cstring { make_type(L"string", ZeroTerminatedArray { TypeRegistry::u8 }) };
pType TypeRegistry::character { make_type(L"char", TypeAlias { TypeRegistry::u32 }) };
pType TypeRegistry::void_ { make_type(L"void", VoidType {}) };
pType TypeRegistry::ambiguous { make_type(L"%ambiguous", Undetermined {}) };
pType TypeRegistry::undetermined { make_type(L"%undetermined", Undetermined {}) };

TypeRegistry::TypeRegistry()
{
    types.emplace(L"u8", u8);
    types.emplace(L"u16", u16);
    types.emplace(L"u32", u32);
    types.emplace(L"u64", u64);
    types.emplace(L"i8", i8);
    types.emplace(L"i16", i16);
    types.emplace(L"i32", i32);
    types.emplace(L"i64", i64);
    types.emplace(L"f32", f32);
    types.emplace(L"f64", f64);
    types.emplace(L"bool", boolean);
    types.emplace(L"string", string);
    types.emplace(L"cstring", cstring);
    types.emplace(L"char", character);
    types.emplace(L"void", void_);
    types.emplace(L"%ambiguous", ambiguous);
    types.emplace(L"%undetermined", undetermined);
    types.emplace(L"byte", make_type(L"byte", TypeAlias { i8 }));
    types.emplace(L"short", make_type(L"short", TypeAlias { i16 }));
    types.emplace(L"int", make_type(L"int", TypeAlias { i32 }));
    types.emplace(L"long", make_type(L"long", TypeAlias { i64 }));
    types.emplace(L"float", make_type(L"float", TypeAlias { f32 }));
    types.emplace(L"double", make_type(L"double", TypeAlias { f64 }));
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

pType TypeRegistry::function_of(std::vector<pType> parameters, pType result)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&parameters, &result](FunctionType const descr) -> bool {
                               return descr.parameters == parameters && descr.result == result;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }

    auto ret = make_type(
        std::format(
            L"func({}) {}",
            join(
                parameters,
                std::wstring_view { L"," },
                [](pType const &t) -> std::wstring_view { return std::wstring_view { t->name }; }),
            result->name),
        FunctionType { parameters, result });
    types.emplace(ret->name, ret);
    return ret;
}

pType TypeRegistry::typelist_of(std::vector<pType> typelist)
{
    for (auto const &[name, t] : types) {
        if (std::visit(overloads {
                           [&typelist](TypeList const descr) -> bool {
                               return descr.types == typelist;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }

    auto ret = make_type(
        std::format(
            L"({})",
            join(
                typelist,
                std::wstring_view { L"," },
                [](pType const &t) -> std::wstring_view { return std::wstring_view { t->name }; })),
        TypeList { typelist });
    types.emplace(ret->name, ret);
    return ret;
}

pType make_error(TokenLocation location, std::wstring msg)
{
    return make_type(
        BindErrors { { {
            std::move(location),
            std::move(msg),
            } } } );
}

}
