/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <map>
#include <string_view>
#include <vector>

#include <Lib.h>

namespace Arwen {

#define BuiltinTypes(S)     \
    S(Bool, "bool")         \
    S(Float, "float")       \
    S(Function, "function") \
    S(Int, "int")           \
    S(U8, "u8")             \
    S(I8, "i8")             \
    S(U16, "u16")           \
    S(I16, "i16")           \
    S(U32, "u32")           \
    S(I32, "i32")           \
    S(U64, "u64")           \
    S(I64, "i64")           \
    S(Void, "void")

enum class BuiltinType {
#undef S
#define S(T, L) T,
    BuiltinTypes(S)
#undef S
};

template<>
inline std::string_view to_string(BuiltinType const &t)
{
    switch (t) {
#undef S
#define S(T, L)          \
    case BuiltinType::T: \
        return L;
        BuiltinTypes(S)
#undef S
    }
}

template<>
inline std::optional<BuiltinType> decode(std::string_view s, ...)
{
#undef S
#define S(T, L) \
    if (s == L) \
        return BuiltinType::T;
    BuiltinTypes(S)
#undef S
        UNREACHABLE();
}

using TypeReference = size_t;

struct Alias {
    TypeReference alias_of;
};

struct Array {
    TypeReference         element_type;
    std::optional<size_t> size {};
};

struct Enum {
    std::optional<TypeReference>                 base_type;
    std::vector<std::pair<std::string, ssize_t>> values;
};

struct Object {
    std::vector<std::pair<std::string, TypeReference>> fields;
};

struct Pointer {
    TypeReference element_type;
};

struct PointerToArray {
    TypeReference          element_type;
    std::optional<uint8_t> sentinel;
};

struct Slice {
    TypeReference element_type;
};

struct Union {
    std::optional<TypeReference>                       base_type;
    std::vector<std::pair<std::string, TypeReference>> fields;
};

using TypeSpec = std::variant<
    Alias,
    BuiltinType,
    Array,
    Enum,
    Object,
    Pointer,
    PointerToArray,
    Slice,
    Union>;

struct Type {
    std::string   name;
    TypeReference ref;
    TypeSpec      typespec;
};

struct TypeRegistry {
    std::optional<TypeReference> find(std::string_view name);
    [[nodiscard]] bool           exists(std::string_view name) const;
    [[nodiscard]] bool           has(TypeReference ref) const;
    [[nodiscard]] Type const    &operator[](BuiltinType t) const;
    [[nodiscard]] Type const    &operator[](TypeReference ref) const;
    [[nodiscard]] Type const    &operator[](std::string_view name) const;
    //    std::optional<TypeReference> resolve_array(TypeReference element_type, std::optional<size_t> size);
    std::optional<TypeReference> resolve_pointer(TypeReference element_type);
    std::optional<TypeReference> resolve_object(Object const &obj);
    static TypeRegistry         &the();

private:
    TypeRegistry();
    TypeReference register_type(Type t);

    std::map<std::string, size_t> index;
    std::vector<Type>             types;
};

}

template<>
struct std::formatter<Arwen::BuiltinType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::BuiltinType const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(t);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::Type, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::Type const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << t.name;
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
