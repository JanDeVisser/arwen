/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <algorithm>
#include <cstdint>
#include <format>
#include <functional>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

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
pType TypeRegistry::pointer { nullptr };
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

intptr_t TypeList::size_of() const
{
    intptr_t ret { 0 };
    std::for_each(
        types.begin(),
        types.end(), [&ret](pType const &t) {
            ret = alignat(ret, t->align_of()) + t->size_of();
        });
    return ret;
}

intptr_t TypeList::align_of() const
{
    intptr_t ret { 0 };
    std::for_each(
        types.begin(),
        types.end(), [&ret](pType const &t) {
            ret = std::max(ret, t->align_of());
        });
    return ret;
}

std::wstring ReferenceType::to_string() const
{
    return std::format(L"&{}", type_name(referencing));
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
    return std::format(L"ArrayOf({}[{}])", type_name(array_of), size);
}

intptr_t Array::size_of() const
{
    return size * array_of->size_of();
}

intptr_t Array::align_of() const
{
    return array_of->align_of();
}

std::wstring DynArray::to_string() const
{
    return std::format(L"DynArrayOf({})", type_name(array_of));
}

std::wstring RangeType::to_string() const
{
    return std::format(L"RangeOf({})", type_name(range_of));
}

intptr_t RangeType::size_of() const
{
    return 3 * range_of->size_of();
}

intptr_t RangeType::align_of() const
{
    return range_of->align_of();
}

std::wstring TypeAlias::to_string() const
{
    return std::format(L"AliasOf({})", type_name(alias_of));
}

intptr_t TypeAlias::size_of() const
{
    return alias_of->size_of();
}

intptr_t TypeAlias::align_of() const
{
    return alias_of->align_of();
}

std::wstring EnumType::to_string() const
{
    return std::format(L"Enum({} values)", values.size());
}


intptr_t EnumType::size_of() const
{
    intptr_t maxsize { 0 };
    std::for_each(
        values.begin(),
        values.end(),
        [&maxsize, this](Value const &value) -> void {
            if (value.payload != nullptr) {
                maxsize = std::max(alignat(value.payload->size_of(), align_of()), maxsize);
            }
        });
    return alignat(underlying_type->size_of(), align_of()) + maxsize;
}

intptr_t EnumType::align_of() const
{
    intptr_t ret { underlying_type->align_of() };
    std::for_each(
        values.begin(),
        values.end(),
        [&ret](Value const &value) -> void {
            ret = std::max((value.payload != nullptr) ? value.payload->align_of() : 0, ret);
        });
    return ret;
}

std::wstring StructType::to_string() const
{
    return std::format(L"Struct({} fields)", fields.size());
}

intptr_t StructType::size_of() const
{
    intptr_t size { 0 };
    std::for_each(
        fields.begin(),
        fields.end(),
        [&size](Field const &fld) -> void {
            size = alignat(size, fld.type->align_of()) + fld.type->size_of();
        });
    return size;
}

intptr_t StructType::align_of() const
{
    intptr_t ret { 0 };
    std::for_each(
        fields.begin(),
        fields.end(),
        [&ret](Field const &fld) -> void {
            ret = std::max(fld.type->align_of(), ret);
        });
    return ret;
}

std::wstring OptionalType::to_string() const
{
    return std::format(L"OptionalOf({})", type_name(type));
}

intptr_t OptionalType::size_of() const
{
    return TypeRegistry::boolean->size_of() + type->size_of();
}

intptr_t OptionalType::align_of() const
{
    return type->align_of();
}

std::wstring ErrorType::to_string() const
{
    return std::format(L"Error({}/{})", type_name(success), type_name(error));
}

intptr_t ErrorType::size_of() const
{
    return TypeRegistry::boolean->size_of() + std::max(success->size_of(), error->size_of());
}

intptr_t ErrorType::align_of() const
{
    return std::max(success->align_of(), error->align_of());
}

std::map<std::wstring, pType> Type::infer_generic_arguments(pType const &param_type) const
{
    std::function<void(std::map<std::wstring, pType> &, Type const &, Type const &)> infer;
    infer = [&infer](std::map<std::wstring, pType> &mapping, Type const &arg, Type const &param) {
        std::visit(overloads {
                       [&mapping, &infer](OptionalType const &d, OptionalType const &other) -> void {
                           infer(mapping, *d.type, *other.type);
                       },
                       [&mapping, &infer](ErrorType const &d, ErrorType const &other) -> void {
                           infer(mapping, *d.success, *other.success);
                           infer(mapping, *d.error, *other.error);
                       },
                       [&mapping, &infer](SliceType const &d, SliceType const &other) -> void {
                           infer(mapping, *d.slice_of, *other.slice_of);
                       },
                       [&mapping, &infer](Array const &d, Array const &other) -> void {
                           infer(mapping, *d.array_of, *other.array_of);
                       },
                       [&mapping, &infer](DynArray const &d, DynArray const &other) -> void {
                           infer(mapping, *d.array_of, *other.array_of);
                       },
                       [&mapping, &infer](ZeroTerminatedArray const &d, ZeroTerminatedArray const &other) -> void {
                           infer(mapping, *d.array_of, *other.array_of);
                       },
                       [&mapping, &infer](RangeType const &d, RangeType const &other) -> void {
                           infer(mapping, *d.range_of, *other.range_of);
                       },
                       [&mapping, &infer, &arg](auto const &d, TypeAlias const &other) -> void {
                           infer(mapping, arg, *other.alias_of);
                       },
                       [&mapping, &arg](auto const &d, GenericParameter const &generic) -> void {
                           mapping[generic.name] = arg.shared_from_this();
                       },
                       [](auto const &d, auto const &other) -> void {
                       } },
            arg.description, param.description);
    };
    if (is<TypeAlias>()) {
        return std::get<TypeAlias>(description).alias_of->infer_generic_arguments(param_type);
    }
    std::map<std::wstring, pType> ret;
    infer(ret, *this, *param_type);
    return ret;
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
    pointer = make_type(L"pointer", PointerType {});
    ambiguous = make_type(L"%ambiguous", Ambiguous {});
    undetermined = make_type(L"%undetermined", Undetermined {});
}

TypeRegistry &TypeRegistry::the()
{
    return s_registry;
}

pType TypeRegistry::generic_parameter(std::wstring name)
{
    for (auto const &t : types) {
        if (std::visit(overloads {
                           [&name](GenericParameter const descr) -> bool {
                               return descr.name == name;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"{}", name), GenericParameter { std::move(name) });
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::referencing(pType type)
{
    assert(type != nullptr);
    for (auto const &t : types) {
        if (std::visit(overloads {
                           [&type](ReferenceType const descr) -> bool {
                               return descr.referencing == type;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"&{}", type->name), ReferenceType { type });
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::alias_for(pType type)
{
    assert(type != nullptr);
    auto ret = make_type(std::format(L"AliasOf({})", type->name), TypeAlias { type });
    types.emplace_back(ret);
    return ret;
}

pType TypeRegistry::slice_of(pType type)
{
    assert(type != nullptr);
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

pType TypeRegistry::dyn_array_of(pType type)
{
    for (auto const &t : types) {
        if (std::visit(overloads {
                           [&type](DynArray const &descr) -> bool {
                               return descr.array_of == type;
                           },
                           [](auto const &) -> bool {
                               return false;
                           } },
                t->description)) {
            return t;
        }
    }
    auto ret = make_type(std::format(L"[*]{}", type->name), DynArray { type });
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
