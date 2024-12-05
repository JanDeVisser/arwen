/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <cstdio>
#include <format>
#include <optional>
#include <string>
#include <string_view>
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
                .name = "string",
                .typespec = TypeSpec {
                    TypeKind::Slice,
                    Slice { static_cast<TypeReference>(PrimitiveType::U8) },
                },
            });
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

}
