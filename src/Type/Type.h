/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cassert>
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
#include <SimpleFormat.h>
#include <TaggedUnion.h>

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

namespace Arwen {

using TypeReference = size_t;

#define PrimitiveTypes(S)         \
    S(Void, void, std::monostate) \
    S(I8, i8, i8)                 \
    S(U8, u8, u8)                 \
    S(I16, i16, i16)              \
    S(U16, u16, u16)              \
    S(I32, i32, i32)              \
    S(U32, u32, u32)              \
    S(I64, i64, i64)              \
    S(U64, u64, u64)              \
    S(Bool, bool, bool)           \
    S(Float, f32, f32)            \
    S(Double, f64, f64)           \
    S(Ptr, ptr, void *)

#define IntegerTypes(S)   \
    S(I8, i8, 1, true)    \
    S(U8, u8, 1, false)   \
    S(I16, i16, 2, true)  \
    S(U16, u16, 2, false) \
    S(I32, i32, 4, true)  \
    S(U32, u32, 4, false) \
    S(I64, i64, 8, true)  \
    S(U64, u64, 8, false)

#define NumericTypes(S) \
    S(I8, i8)           \
    S(U8, u8)           \
    S(I16, i16)         \
    S(U16, u16)         \
    S(I32, i32)         \
    S(U32, u32)         \
    S(I64, i64)         \
    S(U64, u64)         \
    S(Float, f32)       \
    S(Double, f64)

#define PseudoTypes(S)                             \
    S(Aggregate, aggregate, aggregate)             \
    S(Any, any, any)                               \
    S(Function, function, function)                \
    S(Numeric, numeric, numeric)                   \
    S(Integer, integer, integer)                   \
    S(SignedInt, signed, signed)                   \
    S(UnsignedInt, unsigned, unsigned)             \
    S(FloatingPoint, floatingpoint, floatingpoint) \
    S(Self, self, self)                            \
    S(Lhs, lhs, rhs)                               \
    S(Rhs, lhs, rhs)

#define BasicTypes(S)             \
    PrimitiveTypes(S)             \
        S(String, string, string) \
            PseudoTypes(S)

#define BuiltinTypes(S) \
    BasicTypes(S)       \
        S(Int, int, int)

enum class PrimitiveType : TypeReference {
#undef S
#define S(T, L, ...) T = __COUNTER__,
    PrimitiveTypes(S)
#undef S
};

#define STRING __COUNTER__

enum class PseudoType : TypeReference {
#undef S
#define S(T, L, ...) T = __COUNTER__,
    PseudoTypes(S)
#undef S
};

enum class BasicType : TypeReference {
#undef S
#define S(T, L, ...) T,
    BasicTypes(S)
#undef S
};

enum class BuiltinType : TypeReference {
#undef S
#define S(T, L, ...) T,
    BuiltinTypes(S)
#undef S
};

#undef S
#define S(T, L, ...) constexpr static TypeReference T##Type = static_cast<TypeReference>(BuiltinType::T);
BuiltinTypes(S)
#undef S

    template<typename T>
    constexpr inline TypeReference type_of()
{
    UNREACHABLE();
    return VoidType;
}

#undef S
#define S(T, L, ...) \
    template<>       \
    inline TypeReference type_of<__VA_ARGS__>() { return T##Type; }
PrimitiveTypes(S)
#undef S

    TypeReference pointer_type(TypeReference type);

#undef S
#define S(T, L, ...)                              \
    template<>                                    \
    inline TypeReference type_of<__VA_ARGS__ *>() \
    {                                             \
        return pointer_type(T##Type);             \
    }
PrimitiveTypes(S)
#undef S

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
#define S(T, L, ...)    \
    if (iequals(s, #L)) \
        return BasicType::T;
    BasicTypes(S)
#undef S
        UNREACHABLE();
}

template<>
inline std::optional<PrimitiveType> decode(std::string_view s, ...)
{
    if (auto decoded = decode<BasicType>(s); decoded) {
        if (*decoded <= BasicType::Ptr) {
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

#define TypeKinds(S)  \
    S(Primitive)      \
    S(Pseudo)         \
    S(Alias)          \
    S(Array)          \
    S(Enum)           \
    S(Object)         \
    S(Pointer)        \
    S(PointerToArray) \
    S(Slice)          \
    S(Range)          \
    S(Union)

enum class TypeKind {
#undef S
#define S(K) K,
    TypeKinds(S)
#undef S
};

struct Primitive {
    PrimitiveType type;
    size_t        size;
    size_t        alignment;
};

struct Pseudo {
    PseudoType type;
};

struct Alias {
    TypeReference alias_of;
};

struct Array {
    TypeReference element_type;
};

struct Enum {
    std::optional<TypeReference>                 base_type;
    std::vector<std::pair<std::string, int64_t>> values;
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

struct Range {
    TypeReference range_type;
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

    [[nodiscard]] bool        is_numeric() const;
    [[nodiscard]] bool        is_integer() const;
    [[nodiscard]] bool        is_signed() const;
    [[nodiscard]] bool        is_unsigned() const;
    [[nodiscard]] bool        is_float() const;
    [[nodiscard]] bool        is_raw_pointer() const { return ref == PtrType; }
    [[nodiscard]] bool        is_iterable() const { return is_integer(); }
    [[nodiscard]] Type const &decay() const;
    [[nodiscard]] bool        is_assignable_to(Type const &other) const;
    [[nodiscard]] u64         size() const;
    [[nodiscard]] u64         alignment() const;
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
    [[nodiscard]] size_t         count() const { return types.size(); }
    std::optional<TypeReference> resolve_array(TypeReference element_type);
    std::optional<TypeReference> resolve_pointer(TypeReference element_type);
    std::optional<TypeReference> resolve_object(Object const &obj);
    std::optional<TypeReference> resolve_range(TypeReference range_type);
    std::optional<TypeReference> resolve_slice(TypeReference element_type);
    static TypeRegistry         &the();

private:
    TypeRegistry();
    TypeReference register_type(Type t);

    std::map<std::string, size_t> index;
    std::vector<Type>             types;
};

inline bool is_a(TypeReference concrete, BasicType abstract)
{
    auto const &registry = TypeRegistry::the();
    assert(registry.has(concrete));
    return registry[concrete].is_assignable_to(registry[abstract]);
}

inline u64 align_at(u64 value, u64 alignment)
{
    if (value % alignment) {
        return value + (alignment - (value % alignment));
    }
    return value;
}

inline u64 align_down(u64 value, u64 alignment)
{
    if (value % alignment) {
        return value - (alignment - (value % alignment));
    }
    return value;
}

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
struct std::formatter<Arwen::TypeKind, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::TypeKind const &t, FmtContext &ctx) const
    {
        std::ostringstream out;
        switch (t) {
#undef S
#define S(K)                 \
    case Arwen::TypeKind::K: \
        out << #K;           \
        break;
            TypeKinds(S)
#undef S
                default : UNREACHABLE();
        }
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
