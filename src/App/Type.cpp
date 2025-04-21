/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <algorithm>
#include <cstdint>
#include <format>
#include <limits>
#include <ostream>
#include <string_view>

#include <Util/Logging.h>
#include <Util/StringUtil.h>
#include <Util/TokenLocation.h>

#include <App/Type.h>

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

pType TypeRegistry::u8 { nullptr };
pType TypeRegistry::u16 { nullptr };
pType TypeRegistry::u32 { nullptr };
pType TypeRegistry::u64 { nullptr };
pType TypeRegistry::i8 { nullptr };
pType TypeRegistry::i16 { nullptr };
pType TypeRegistry::i32 { nullptr };
pType TypeRegistry::i64 { nullptr };
pType TypeRegistry::f32 { nullptr };
pType TypeRegistry::f64 { nullptr };
pType TypeRegistry::boolean { nullptr };
pType TypeRegistry::string { nullptr };
pType TypeRegistry::cstring { nullptr };
pType TypeRegistry::character { nullptr };
pType TypeRegistry::void_ { nullptr };
pType TypeRegistry::ambiguous { nullptr };
pType TypeRegistry::undetermined { nullptr };

TypeRegistry TypeRegistry::s_registry {};

std::wstring type_name(pType const &type)
{
    return (type != nullptr) ? type->name : L"nullptr";
}

std::wstring FunctionType::to_string() const
{
    return std::format(
        L"Func({}) {}",
        join(parameters, std::wstring_view { L", " }, [](pType const &t) { return type_name(t); }),
        result->name);
}

std::wstring TypeList::to_string() const
{
    return std::format(L"({})", join(types, std::wstring_view { L", " }, [](pType const &t) { return type_name(t); }));
}

std::wstring SliceType::to_string() const
{
    return std::format(L"SliceOf({})", type_name(slice_of));
}

std::wstring ZeroTerminatedArray::to_string() const
{
    return std::format(L"ZeroTerminatedArrayOf({})", type_name(array_of));
}

std::wstring Array::to_string() const
{
    return std::format(L"ArrayOf({})", type_name(array_of));
}

std::wstring RangeType::to_string() const
{
    return std::format(L"RangeOf({})", type_name(range_of));
}

std::wstring TypeAlias::to_string() const
{
    return std::format(L"AliasOf({})", type_name(alias_of));
}

std::wstring EnumType::to_string() const
{
    return std::format(L"Enum({} values)", values.size());
}

std::wstring StructType::to_string() const
{
    return std::format(L"Struct({} fields)", fields.size());
}

std::wstring OptionalType::to_string() const
{
    return std::format(L"OptionalOf({})", type_name(type));
}

std::wstring ErrorType::to_string() const
{
    return std::format(L"Error({}/{})", type_name(success), type_name(error));
}

TypeRegistry::TypeRegistry()
{
    u8 = make_type(L"u8", IntType::u8);
    u16 = make_type(L"u16", IntType::u16);
    u32 = make_type(L"u32", IntType::u32);
    u64 = make_type(L"u64", IntType::u64);
    i8 = make_type(L"i8", IntType::i8);
    i16 = make_type(L"i16", IntType::i16);
    i32 = make_type(L"i32", IntType::i32);
    i64 = make_type(L"i64", IntType::i64);
    f32 = make_type(L"f32", FloatType::f32);
    f64 = make_type(L"f64", FloatType::f64);
    boolean = make_type(L"bool", BoolType {});
    string = make_type(L"string", SliceType { TypeRegistry::u32 });
    cstring = make_type(L"cstring", ZeroTerminatedArray { TypeRegistry::u8 });
    character = make_type(L"char", TypeAlias { TypeRegistry::u32 });
    void_ = make_type(L"void", VoidType {});
    ambiguous = make_type(L"%ambiguous", Ambiguous {});
    undetermined = make_type(L"%undetermined", Undetermined {});

    types.emplace_back(ambiguous);
    types.emplace_back(undetermined);
    types.emplace_back(boolean);
    types.emplace_back(character);
    types.emplace_back(cstring);
    types.emplace_back(f32);
    types.emplace_back(f64);
    types.emplace_back(i16);
    types.emplace_back(i32);
    types.emplace_back(i64);
    types.emplace_back(i8);
    types.emplace_back(string);
    types.emplace_back(u16);
    types.emplace_back(u32);
    types.emplace_back(u64);
    types.emplace_back(u8);
    types.emplace_back(void_);
    types.emplace_back(make_type(L"byte", TypeAlias { i8 }));
    types.emplace_back(make_type(L"short", TypeAlias { i16 }));
    types.emplace_back(make_type(L"int", TypeAlias { i32 }));
    types.emplace_back(make_type(L"long", TypeAlias { i64 }));
    types.emplace_back(make_type(L"float", TypeAlias { f32 }));
    types.emplace_back(make_type(L"double", TypeAlias { f64 }));
}

TypeRegistry &TypeRegistry::the()
{
    return s_registry;
}

pType TypeRegistry::slice_of(pType type)
{
    assert(type != nullptr);
    std::wcout << L"slice_of(" << type->name << L")\n";
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::zero_terminated_array_of(pType type)
{
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::array_of(pType type, size_t size)
{
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::optional_of(pType type)
{
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::error_of(pType success, pType error)
{
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::function_of(std::vector<pType> const &parameters, pType result)
{
    for (auto const &p : parameters) {
        std::wcout << ((p == nullptr) ? std::wstring { L"nullptr" } : p->name) << std::endl;
    }
    for (auto const &t : types) {
        if (std::visit(overloads {
                           [&parameters, &result](FunctionType const &descr) -> bool {
                               return descr.parameters == parameters && descr.result == result;
                           },
                           [](auto const &x) -> bool {
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
                [](pType const &t) -> std::wstring { return t->name; }),
            result->name),
        FunctionType { parameters, result });
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::typelist_of(std::vector<pType> const &typelist)
{
    for (auto const &t : types) {
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
    types.emplace_back(ret);
    return ret;
}

pType make_error(TokenLocation location, std::wstring msg)
{
    return make_type(
        BindErrors { { {
            std::move(location),
            std::move(msg),
        } } });
}

}
