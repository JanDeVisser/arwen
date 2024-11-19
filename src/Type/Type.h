/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <Lib.h>
#include <Logging.h>
#include <TaggedUnion.h>

namespace Arwen {

using u8 = uint8_t;
using i8 = int8_t;
using u16 = uint16_t;
using i16 = int16_t;
using u32 = uint32_t;
using i32 = int32_t;
using u64 = uint64_t;
using i64 = int64_t;
using f32 = float;
using f64 = double;

#define PrimitiveTypes(S)         \
    S(Null, null, std::monostate) \
    S(Bool, bool, bool)           \
    S(Double, f64, f64)           \
    S(Float, f32, f32)            \
    S(Int, int, int)              \
    S(U8, u8, u8)                 \
    S(I8, i8, i8)                 \
    S(U16, u16, u16)              \
    S(I16, i16, i16)              \
    S(U32, u32, u32)              \
    S(I32, i32, i32)              \
    S(U64, u64, u64)              \
    S(I64, i64, i64)

#define PseudoTypes(S)                 \
    S(Aggregate, aggregate, aggregate) \
    S(Any, any, any)                   \
    S(Function, function, function)    \
    S(Numeric, numeric, numeric)       \
    S(Self, self, self)                \
    S(Void, void, void *)

#define BasicTypes(S) \
    PrimitiveTypes(S) \
        PseudoTypes(S)

enum class PrimitiveType {
#undef S
#define S(T, L, ...) T = __COUNTER__,
    PrimitiveTypes(S)
#undef S
};

enum class PseudoType {
#undef S
#define S(T, L, ...) T = __COUNTER__,
    PseudoTypes(S)
#undef S
};

enum class BasicType {
#undef S
#define S(T, L, ...) T,
    BasicTypes(S)
#undef S
};

template<>
inline std::string_view to_string(BasicType const &t)
{
    switch (t) {
#undef S
#define S(T, L, ...)   \
    case BasicType::T: \
        return #L;
        BasicTypes(S)
#undef S
    }
}

template<>
inline std::string_view to_string(PrimitiveType const &t)
{
    return to_string(static_cast<BasicType>(t));
}

template<>
inline std::string_view to_string(PseudoType const &t)
{
    return to_string(static_cast<BasicType>(t));
}

template<>
inline std::optional<BasicType> decode(std::string_view s, ...)
{
#undef S
#define S(T, L, ...) \
    if (s == #L)     \
        return BasicType::T;
    BasicTypes(S)
#undef S
        UNREACHABLE();
}

template<>
inline std::optional<PrimitiveType> decode(std::string_view s, ...)
{
    if (auto decoded = decode<BasicType>(s); decoded) {
        if (*decoded <= BasicType::I64) {
            return static_cast<PrimitiveType>(*decoded);
        }
    }
    return {};
}

template<>
inline std::optional<PseudoType> decode(std::string_view s, ...)
{
    if (auto decoded = decode<BasicType>(s); decoded) {
        if (*decoded >= BasicType::Any) {
            return static_cast<PseudoType>(*decoded);
        }
    }
    return {};
}

using TypeReference = size_t;

#define TypeKinds(S) \
    S(Primitive) \
    S(Pseudo) \
    S(Alias) \
    S(Array) \
    S(Enum) \
    S(Object) \
    S(Pointer) \
    S(PointerToArray) \
    S(Slice) \
    S(Union) \

enum class TypeKind {
#undef S
#define S(K) K,
    TypeKinds(S)
#undef S
};

struct Primitive {
    PrimitiveType type;
    size_t    size;
    size_t    alignment;
};

struct Pseudo {
    PseudoType type;
};

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

using TypeSpec = TaggedUnion<TypeKind,
#undef S
#define S(K) K,
    TypeKinds(S)
#undef S
    std::monostate>;

struct Type {
    std::string   name;
    TypeReference ref;
    TypeSpec      typespec;

    bool is_assignable_to(TypeReference other)
    {
        return ref == other;
    }
};

struct TypeRegistry {
    std::optional<TypeReference> find(std::string_view name);
    [[nodiscard]] bool           exists(std::string_view name) const;
    [[nodiscard]] bool           has(TypeReference ref) const;
    [[nodiscard]] Type const    &operator[](BasicType t) const;
    [[nodiscard]] Type const    &operator[](PrimitiveType t) const { return operator[](static_cast<BasicType>(t)); }
    [[nodiscard]] Type const    &operator[](PseudoType t) const { return operator[](static_cast<BasicType>(t)); }
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
struct std::formatter<Arwen::BasicType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::BasicType const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(t);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::PrimitiveType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::PrimitiveType const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("{}", static_cast<Arwen::BasicType>(t));
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::PseudoType, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::PseudoType const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << std::format("{}", static_cast<Arwen::BasicType>(t));
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
