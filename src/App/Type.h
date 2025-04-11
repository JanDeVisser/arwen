/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace Arwen {

struct IntType {
    bool     is_signed;
    uint8_t  width_bits;
    uint64_t max_value;
    int64_t  min_value;

    static IntType u8;
    static IntType u16;
    static IntType u32;
    static IntType u64;
    static IntType i8;
    static IntType i16;
    static IntType i32;
    static IntType i64;
};

struct FloatType {
    uint8_t width_bits;

    static FloatType f32;
    static FloatType f64;
};

struct BoolType {
    static BoolType boolean;
};

struct VoidType {
    static VoidType void_type;
};

using pType = std::shared_ptr<struct Type>;

struct SliceType {
    pType slice_of;
};

struct ZeroTerminatedArray {
    pType array_of;
};

struct Array {
    pType  array_of;
    size_t size;
};

struct TypeAlias {
    pType alias_of;
};

struct EnumType {
    struct Value {
        std::wstring label;
        int64_t      value;
        pType        payload;
    };

    pType              underlying_type;
    std::vector<Value> values;
};

struct StructType {
    struct Field {
        std::wstring name;
        pType        type;
    };
    std::vector<Field> fields;
};

struct OptionalType {
    pType type;
};

struct ErrorType {
    pType success;
    pType error;
};

using TypeDescription = std::variant<
    VoidType,
    IntType,
    FloatType,
    BoolType,
    SliceType,
    ZeroTerminatedArray,
    Array,
    TypeAlias,
    EnumType,
    OptionalType,
    ErrorType,
    StructType>;

struct Type : std::enable_shared_from_this<Type> {
    std::wstring    name;
    TypeDescription description;

    template<typename DescrType>
    Type(std::wstring n, DescrType descr)
        : name(std::move(n))
        , description(std::move(descr))
    {
    }
};

template<typename DescrType>
pType make_type(std::wstring n, DescrType descr)
{
    return std::make_shared<Type>(std::move(n), std::move(descr));
}

struct TypeRegistry {
    std::map<std::wstring, pType> types;

    static TypeRegistry &the();
    pType slice_of(pType type);
    pType zero_terminated_array_of(pType type);
    pType array_of(pType type, size_t size);
    pType optional_of(pType type);
    pType error_of(pType success, pType error);

private:
    TypeRegistry();
    static TypeRegistry s_registry;
};

}
