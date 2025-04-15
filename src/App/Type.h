/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include "Util/Utf8.h"
#include <cstdint>
#include <format>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include <App/Operator.h>

namespace Arwen {

using pType = std::shared_ptr<struct Type>;

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
};

struct VoidType {
};

struct NamespaceType {
};

struct FunctionType {
    std::vector<pType> parameters;
    pType              result;
};

struct TypeList {
    std::vector<pType> types;
};

struct Undetermined {
};

struct Ambiguous {
};

struct BindErrors {
    std::vector<ArwenError> errors;
};

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

struct RangeType {
    pType range_of;
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
    using Values = std::vector<Value>;

    pType  underlying_type;
    Values values;
};

struct StructType {
    struct Field {
        std::wstring name;
        pType        type;
    };

    using Fields = std::vector<Field>;

    Fields fields;
};

struct OptionalType {
    pType type;
};

struct ErrorType {
    pType success;
    pType error;
};

using TypeDescription = std::variant<
    Undetermined,
    Ambiguous,
    BindErrors,
    VoidType,
    NamespaceType,
    FunctionType,
    TypeList,
    IntType,
    FloatType,
    BoolType,
    SliceType,
    ZeroTerminatedArray,
    Array,
    RangeType,
    TypeAlias,
    EnumType,
    OptionalType,
    ErrorType,
    StructType>;

struct Type : std::enable_shared_from_this<Type> {
    std::wstring                  name;
    TypeDescription               description;
    std::map<std::wstring, pType> arguments {};

    template<typename DescrType>
    Type(std::wstring n, DescrType descr)
        : name(std::move(n))
        , description(std::move(descr))
    {
    }

    template<typename DescrType>
    bool is() const
    {
        return std::holds_alternative<DescrType>(description);
    }
};

struct TypeRegistry {
    std::map<std::wstring, pType> types;

    static TypeRegistry &the();
    pType                slice_of(pType type);
    pType                zero_terminated_array_of(pType type);
    pType                array_of(pType type, size_t size);
    pType                optional_of(pType type);
    pType                error_of(pType success, pType error);
    pType                function_of(std::vector<pType> parameters, pType result);
    pType                typelist_of(std::vector<pType> typelist);

    static pType u8;
    static pType u16;
    static pType u32;
    static pType u64;
    static pType i8;
    static pType i16;
    static pType i32;
    static pType i64;
    static pType f32;
    static pType f64;
    static pType boolean;
    static pType string;
    static pType cstring;
    static pType character;
    static pType void_;
    static pType undetermined;
    static pType ambiguous;

private:
    TypeRegistry();
    static TypeRegistry s_registry;
};

template<typename DescrType>
pType make_type(std::wstring n, DescrType descr)
{
    auto ret = std::make_shared<Type>(std::move(n), std::move(descr));
    TypeRegistry::the().types[ret->name] = ret;
    return ret;
}

template<typename DescrType>
pType make_type(DescrType descr)
{
    auto ret = std::make_shared<Type>(std::format(L"anon-{}", TypeRegistry::the().types.size()), std::move(descr));
    TypeRegistry::the().types[ret->name] = ret;
    return ret;
}

pType make_error(TokenLocation location, std::wstring msg);

template<typename... Args>
pType make_error(TokenLocation location, std::format_string<Args...> const message, Args &&...args)
{
    return make_error(std::move(location), as_wstring(std::vformat(message.get(), std::make_format_args(args...))));
}

template<typename... Args>
pType make_error(TokenLocation location, std::wformat_string<Args...> const message, Args &&...args)
{
    return make_error(std::move(location), std::vformat(message.get(), std::make_wformat_args(args...)));
}

}
