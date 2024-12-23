/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <Lib.h>
#include <Logging.h>
#include <Type/Type.h>

namespace Arwen {

bool Type::is_numeric() const
{
    if (typespec.tag() != TypeKind::Primitive) {
        return false;
    }
    return is_integer() || is_float();
}

bool Type::is_integer() const
{
    if (typespec.tag() != TypeKind::Primitive) {
        return false;
    }
    auto const &p = typespec.get<TypeKind::Primitive>().type;
    return (p >= PrimitiveType::I8) && (p <= PrimitiveType::U64);
}

bool Type::is_signed() const
{
    if (typespec.tag() != TypeKind::Primitive || !is_integer()) {
        return false;
    }
    auto const &p = typespec.get<TypeKind::Primitive>().type;
    return (p == PrimitiveType::I8) || (p == PrimitiveType::I16) || (p == PrimitiveType::I32) || (p == PrimitiveType::I64);
}

bool Type::is_unsigned() const
{
    return is_integer() && !is_signed();
}

bool Type::is_float() const
{
    if (typespec.tag() != TypeKind::Primitive) {
        return false;
    }
    auto const &p = typespec.get<TypeKind::Primitive>().type;
    return (p == PrimitiveType::Double) || (p == PrimitiveType::Float);
}

Type const &Type::decay() const
{
    if (typespec.tag() == TypeKind::Alias) {
        return TypeRegistry::the()[typespec.get<TypeKind::Alias>().alias_of].decay();
    }
    return *this;
}

bool Type::is_assignable_to(Type const &other) const
{
    auto const &registry = TypeRegistry::the();
    auto const &t = decay();
    auto const &o = other.decay();
    if (o.typespec.tag() == TypeKind::Pseudo) {
        switch (o.typespec.get<TypeKind::Pseudo>().type) {
        case PseudoType::Any:
            return true;
        case PseudoType::Numeric:
            return t.is_numeric();
        case PseudoType::Integer:
            return t.is_integer();
        case PseudoType::SignedInt:
            return t.is_signed();
        case PseudoType::UnsignedInt:
            return t.is_unsigned();
        case PseudoType::FloatingPoint:
            return t.is_float();
        default:
            UNREACHABLE();
        }
    }
    return t.ref == o.ref;
}

template<class TypeImpl>
u64 size(TypeReference, TypeImpl const &)
{
    fatal("Can't determine size of {}", typeid(TypeImpl).name());
}

template<class TypeImpl>
u64 alignment(TypeReference, TypeImpl const &)
{
    fatal("Can't determine alignment of {}", typeid(TypeImpl).name());
}

template<>
u64 size(TypeReference, Primitive const &primitive)
{
    return primitive.size;
}

template<>
u64 alignment(TypeReference, Primitive const &primitive)
{
    return primitive.alignment;
}

template<>
u64 size(TypeReference, Pseudo const &primitive)
{
    return 0;
}

template<>
u64 alignment(TypeReference, Pseudo const &primitive)
{
    return 0;
}

template<>
u64 size(TypeReference, Array const &)
{
    auto const &u64type = TypeRegistry::the()[U64Type];
    auto        ret = TypeRegistry::the()[PtrType].size();
    return align_at(ret, u64type.alignment()) + u64type.size();
}

template<>
u64 alignment(TypeReference, Array const &)
{
    return TypeRegistry::the()[PtrType].alignment();
}

template<>
u64 size(TypeReference, Enum const &enumeration)
{
    auto const &type = TypeRegistry::the()[(enumeration.base_type) ? *enumeration.base_type : U64Type].decay();
    return type.size();
}

template<>
u64 alignment(TypeReference, Enum const &enumeration)
{
    auto const &type = TypeRegistry::the()[(enumeration.base_type) ? *enumeration.base_type : U64Type].decay();
    return type.alignment();
}

template<>
u64 size(TypeReference, Object const &object)
{
    u64 sz = 0;
    for (auto const &[_, type_ref] : object.fields) {
        auto const &type = TypeRegistry::the()[type_ref].decay();
        sz = align_at(sz, type.alignment());
        sz += type.size();
    }
    return sz;
}

template<>
u64 alignment(TypeReference, Object const &object)
{
    u64 sz = 0;
    if (object.fields.empty()) {
        return 0;
    }
    auto const &type = TypeRegistry::the()[object.fields[0].second].decay();
    return type.alignment();
}

template<>
u64 size(TypeReference, Pointer const &)
{
    return TypeRegistry::the()[PtrType].size();
}

template<>
u64 alignment(TypeReference, Pointer const &)
{
    return TypeRegistry::the()[PtrType].alignment();
}

template<>
u64 size(TypeReference, PointerToArray const &)
{
    return TypeRegistry::the()[PtrType].size();
}

template<>
u64 alignment(TypeReference, PointerToArray const &)
{
    return TypeRegistry::the()[PtrType].alignment();
}

template<>
u64 size(TypeReference, Slice const &)
{
    auto const &u64type = TypeRegistry::the()[U64Type];
    auto        ret = TypeRegistry::the()[PtrType].size();
    return align_at(ret, u64type.alignment()) + u64type.size();
}

template<>
u64 alignment(TypeReference, Slice const &)
{
    return TypeRegistry::the()[PtrType].alignment();
}

template<>
u64 size(TypeReference, Range const &range)
{
    auto range_type = TypeRegistry::the()[range.range_type].decay();
    return 2*align_at(range_type.size(), range_type.alignment());
}

template<>
u64 alignment(TypeReference, Range const &range)
{
    return TypeRegistry::the()[range.range_type].decay().alignment();
}

template<>
u64 size(TypeReference, Union const &u)
{
    auto const &base_type = TypeRegistry::the()[(u.base_type) ? *u.base_type : U64Type].decay();
    auto        base_size = base_type.size();

    u64 max_sz = 0;
    for (auto const &[_, type_ref] : u.fields) {
        auto const &type = TypeRegistry::the()[type_ref].decay();
        max_sz = std::max(max_sz, align_at(base_size, type.alignment()) + type.size());
    }
    return (base_type.size() + max_sz) * u.fields.size();
}

template<>
u64 alignment(TypeReference, Union const &u)
{
    auto const &base_type = TypeRegistry::the()[(u.base_type) ? *u.base_type : U64Type].decay();
    return base_type.alignment();
}

u64 Type::size() const
{
    return std::visit(
        [&](auto &impl) -> u64 {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::size(ref, impl);
        },
        typespec.payload());
}

u64 Type::alignment() const
{
    return std::visit(
        [&](auto &impl) -> u64 {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::alignment(ref, impl);
        },
        typespec.payload());
}

TypeRegistry::TypeRegistry()
{
#undef S
#define S(T, L, ...) register_type(   \
    Type {                            \
        .name = #L,                   \
        .typespec = TypeSpec {        \
            TypeKind::Primitive,      \
            Primitive {               \
                PrimitiveType::T,     \
                sizeof(__VA_ARGS__),  \
                alignof(__VA_ARGS__), \
            },                        \
        } });
    PrimitiveTypes(S)
#undef S
        register_type(Type {
            .name = "string",
            .typespec = TypeSpec {
                TypeKind::Slice,
                Slice { U8Type },
            },
        });
#undef S
#define S(T, L, ...) register_type( \
    Type {                          \
        .name = #L,                 \
        .typespec = TypeSpec {      \
            TypeKind::Pseudo,       \
            Pseudo {                \
                PseudoType::T,      \
            },                      \
        } });
    PseudoTypes(S)
#undef S
        register_type(Type {
            .name = "int",
            .typespec = TypeSpec {
                TypeKind::Alias,
                Alias { static_cast<TypeReference>(PrimitiveType::I32) },
            },
        });
}

TypeReference TypeRegistry::register_type(Type t)
{
    t.ref = types.size();
    index.emplace(t.name, t.ref);
    types.emplace_back(std::move(t));
    auto const &tt = types.back();
    return t.ref;
}

std::optional<TypeReference> TypeRegistry::find(std::string_view name)
{
    std::string n { name };
    if (auto it = index.find(n); it != index.end()) {
        return types[it->second].ref;
    }
    return {};
}

bool TypeRegistry::exists(std::string_view name) const
{
    std::string n { name };
    return index.contains(n);
}

bool TypeRegistry::has(TypeReference ref) const
{
    return ref < types.size();
}

Type const &TypeRegistry::operator[](BasicType t) const
{
    return types[static_cast<size_t>(t)];
}

Type const &TypeRegistry::operator[](TypeReference ref) const
{
    assert(ref < types.size());
    return types[ref];
}

Type const &TypeRegistry::operator[](std::string_view name) const
{
    std::string n { name };
    assert(index.contains(n));
    return types[index.at(n)];
}

std::optional<TypeReference> TypeRegistry::resolve_array(TypeReference element_type)
{
    if (element_type >= types.size()) {
        return {};
    }
    for (auto const &t : types) {
        if (t.typespec.tag() != TypeKind::Array) {
            continue;
        }
        auto const &arr = t.typespec.get<TypeKind::Array>();
        if (arr.element_type == element_type) {
            return t.ref;
        }
    }
    return register_type({ .name = std::format("[x]{}", types[element_type].name),
        .typespec = TypeSpec {
            TypeKind::Array,
            Array {
                .element_type = element_type,
            },
        } });
}

std::optional<TypeReference> TypeRegistry::resolve_slice(TypeReference element_type)
{
    if (element_type >= types.size()) {
        return {};
    }
    for (auto const &t : types) {
        if (t.typespec.tag() != TypeKind::Slice) {
            continue;
        }
        auto const &arr = t.typespec.get<TypeKind::Slice>();
        if (arr.element_type == element_type) {
            return t.ref;
        }
    }
    return register_type({ .name = std::format("[]{}", types[element_type].name),
        .typespec = TypeSpec {
            TypeKind::Slice,
            Slice {
                .element_type = element_type,
            },
        } });
}

std::optional<TypeReference> TypeRegistry::resolve_range(TypeReference range_type)
{
    if (range_type >= types.size() || !types[range_type].is_iterable()) {
        return {};
    }
    for (auto const &t : types) {
        if (t.typespec.tag() != TypeKind::Range) {
            continue;
        }
        auto const &arr = t.typespec.get<TypeKind::Range>();
        if (arr.range_type == range_type) {
            return t.ref;
        }
    }
    return register_type({ .name = std::format("[..]{}", types[range_type].name),
        .typespec = TypeSpec {
            TypeKind::Range,
            Range {
                .range_type = range_type,
            },
        } });
}

std::optional<TypeReference> TypeRegistry::resolve_pointer(TypeReference element_type)
{
    if (element_type >= types.size()) {
        return {};
    }
    for (auto const &t : types) {
        if (t.typespec.tag() != TypeKind::Pointer) {
            continue;
        }
        auto const &arr = t.typespec.get<TypeKind::Pointer>();
        if (arr.element_type == element_type) {
            return t.ref;
        }
    }
    return register_type(Type {
        .name = std::format("*{}", types[element_type].name),
        .typespec = TypeSpec {
            TypeKind::Pointer,
            Pointer {
                .element_type = element_type,
            },
        },
    });
}

std::optional<TypeReference> TypeRegistry::resolve_object(Object const &obj)
{
    for (auto const &fld : obj.fields) {
        if (fld.second >= types.size()) {
            return {};
        }
    }
    for (auto const &t : types) {
        auto matches = std::visit(
            overload {
                [&obj](Object const &o) -> bool {
                    return o.fields == obj.fields;
                },
                [](auto const &) -> bool {
                    return false;
                },
            },
            t.typespec.payload());
        if (matches) {
            return t.ref;
        }
    }
    std::string name = "Object{";
    auto        first { true };
    for (auto const &fld : obj.fields) {
        auto const &t = types[fld.second];
        if (!first) {
            name += ",";
        }
        name += std::format("{}: {}", fld.first, t.name);
        first = false;
    }
    name += "}";
    return register_type(Type {
        .name = name,
        .typespec = TypeSpec {
            TypeKind::Object,
            obj,
        },
    });
}

TypeRegistry &TypeRegistry::the()
{
    static TypeRegistry the_;
    return the_;
}

TypeReference pointer_type(TypeReference type)
{
    auto ret = TypeRegistry::the().resolve_pointer(type);
    assert(ret.has_value());
    return *ret;
}

}
