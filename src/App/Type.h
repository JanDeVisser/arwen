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

using pType = std::shared_ptr<struct Type>;

struct SliceType {
    pType slice_of;
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

    std::wstring       name;
    pType              underlying_type;
    std::vector<Value> values;
};

struct StructType {
    struct Field {
        std::wstring name;
        pType        type;
    };
    std::wstring       name;
    std::vector<Field> fields;
};

using TypeDescription = std::variant<
    IntType,
    FloatType,
    BoolType,
    SliceType,
    TypeAlias,
    EnumType,
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

private:
    TypeRegistry();
    static TypeRegistry s_registry;
};

}
