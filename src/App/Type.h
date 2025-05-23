/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstdint>
#include <format>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Utf8.h>

#include <App/Operator.h>

namespace Arwen {

#define TypeKinds(S)       \
    S(Undetermined)        \
    S(Ambiguous)           \
    S(BindErrors)          \
    S(VoidType)            \
    S(PointerType)         \
    S(NamespaceType)       \
    S(FunctionType)        \
    S(TypeList)            \
    S(GenericParameter)    \
    S(IntType)             \
    S(FloatType)           \
    S(BoolType)            \
    S(ReferenceType)       \
    S(SliceType)           \
    S(ZeroTerminatedArray) \
    S(Array)               \
    S(DynArray)            \
    S(RangeType)           \
    S(TypeAlias)           \
    S(EnumType)            \
    S(OptionalType)        \
    S(ErrorType)           \
    S(StructType)

enum class TypeKind {
#undef S
#define S(K) K,
    TypeKinds(S)
#undef S
};

using pType = std::shared_ptr<const struct Type>;
// using pConstType = std::shared_ptr<const struct Type>;

struct Slice {
    void   *ptr { nullptr };
    int64_t size { 0 };
};

struct DynamicArray {
    void   *ptr { nullptr };
    int64_t size { 0 };
    int64_t capacity { 0 };
};

struct StaticArray {
    void   *ptr { nullptr };
    int64_t size { 0 };
};

#define BitWidths(S) \
    S(8)             \
    S(16)            \
    S(32)            \
    S(64)

#define FloatBitWidths(S) \
    S(32, float)          \
    S(64, double)

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

    std::wstring to_string() const
    {
        return std::format(L"{:c}{}", (is_signed) ? 'i' : 'u', width_bits);
    }

    intptr_t size_of() const
    {
        return width_bits / 8;
    }

    intptr_t align_of() const
    {
        return width_bits / 8;
    }
};

struct FloatType {
    uint8_t width_bits;

    static FloatType f32;
    static FloatType f64;

    std::wstring to_string() const
    {
        return std::format(L"f{}", width_bits);
    }

    intptr_t size_of() const
    {
        return width_bits / 8;
    }

    intptr_t align_of() const
    {
        return width_bits / 8;
    }
};

struct BoolType {
    std::wstring to_string() const
    {
        return L"Bool";
    }

    intptr_t size_of() const
    {
        return 1;
    }

    intptr_t align_of() const
    {
        return 1;
    }
};

struct VoidType {
    std::wstring to_string() const
    {
        return L"Void";
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct PointerType {
    std::wstring to_string() const
    {
        return L"Pointer";
    }

    intptr_t size_of() const
    {
        return sizeof(void *);
    }

    intptr_t align_of() const
    {
        return alignof(void *);
    }
};

struct NamespaceType {
    std::wstring to_string() const
    {
        return L"Namespace";
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct FunctionType {
    std::vector<pType> parameters;
    pType              result;

    std::wstring to_string() const;

    intptr_t size_of() const
    {
        return sizeof(void *);
    }

    intptr_t align_of() const
    {
        return alignof(void *);
    }
};

struct TypeList {
    std::vector<pType> types;

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

struct Undetermined {
    std::wstring to_string() const
    {
        return L"Undetermined";
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct Ambiguous {
    std::wstring to_string() const
    {
        return L"Ambiguous";
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct BindErrors {
    std::vector<ArwenError> errors;
    std::wstring            to_string() const
    {
        return L"BindErrors";
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct GenericParameter {
    std::wstring name;

    std::wstring to_string() const
    {
        return std::format(L"<{}>", name);
    }

    intptr_t size_of() const
    {
        return 0;
    }

    intptr_t align_of() const
    {
        return 0;
    }
};

struct ReferenceType {
    pType        referencing;
    std::wstring to_string() const;

    intptr_t size_of() const
    {
        return sizeof(void *);
    }

    intptr_t align_of() const
    {
        return alignof(void *);
    }
};

struct SliceType {
    pType        slice_of;
    std::wstring to_string() const;

    intptr_t size_of() const
    {
        return sizeof(Slice);
    }

    intptr_t align_of() const
    {
        return alignof(Slice);
    }
};

struct ZeroTerminatedArray {
    pType        array_of;
    std::wstring to_string() const;

    intptr_t size_of() const
    {
        return sizeof(void *);
    }

    intptr_t align_of() const
    {
        return alignof(void *);
    }
};

struct Array {
    pType  array_of;
    size_t size;

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

struct DynArray {
    pType        array_of;
    std::wstring to_string() const;

    intptr_t size_of() const
    {
        return sizeof(DynamicArray);
    }

    intptr_t align_of() const
    {
        return alignof(DynamicArray);
    }
};

struct RangeType {
    pType range_of;

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

struct TypeAlias {
    pType        alias_of;
    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
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

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

struct StructType {
    struct Field {
        std::wstring name;
        pType        type;
    };

    using Fields = std::vector<Field>;

    Fields       fields;
    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;

    auto field(std::wstring_view field_name)
    {
        return std::find_if(fields.begin(), fields.end(), [&field_name](StructType::Field const &fld) -> bool {
            return fld.name == field_name;
        });
    }
};

struct OptionalType {
    pType type;

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

struct ErrorType {
    pType success;
    pType error;

    std::wstring to_string() const;
    intptr_t     size_of() const;
    intptr_t     align_of() const;
};

using TypeDescription = std::variant<
#undef S
#define S(K) K,
    TypeKinds(S)
#undef S
        std::monostate>;

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

    bool is(TypeKind kind) const
    {
        return description.index() == static_cast<int>(kind);
    }

    TypeKind kind() const
    {
        return static_cast<TypeKind>(description.index());
    }

    std::wstring to_string() const
    {
        return std::format(
            L"{}: {}",
            name,
            std::visit(overloads {
                           [](std::monostate const &) -> std::wstring {
                               UNREACHABLE();
                               return L"";
                           },
                           [](auto const &descr) -> std::wstring {
                               return descr.to_string();
                           } },
                description));
    }

    intptr_t size_of() const
    {
        return std::visit(overloads {
                              [](std::monostate const &) -> intptr_t {
                                  UNREACHABLE();
                                  return 0;
                              },
                              [](auto const &descr) -> intptr_t {
                                  return descr.size_of();
                              } },
            description);
    }

    intptr_t align_of() const
    {
        return std::visit(overloads {
                              [](std::monostate const &) -> intptr_t {
                                  UNREACHABLE();
                                  return 0;
                              },
                              [](auto const &descr) -> intptr_t {
                                  return descr.align_of();
                              } },
            description);
    }

    std::map<std::wstring, pType> infer_generic_arguments(pType const &param_type) const;
};

struct TypeRegistry {
    std::vector<pType> types;

    static TypeRegistry &the();
    pType                generic_parameter(std::wstring name);
    pType                referencing(pType type);
    pType                alias_for(pType type);
    pType                slice_of(pType type);
    pType                zero_terminated_array_of(pType type);
    pType                array_of(pType type, size_t size);
    pType                dyn_array_of(pType type);
    pType                optional_of(pType type);
    pType                error_of(pType success, pType error);
    pType                function_of(std::vector<pType> const &parameters, pType result);
    pType                typelist_of(std::vector<pType> const &typelist);

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
    static pType pointer;
    static pType undetermined;
    static pType ambiguous;

private:
    TypeRegistry();
    static TypeRegistry s_registry;
};

template<typename DescrType>
pType make_type(std::wstring n, DescrType descr)
{
    // std::wcout << "Registering type " << n << ' ' << descr.to_string() << std::endl;
    auto ret = std::make_shared<Type const>(std::move(n), std::move(descr));
    TypeRegistry::the().types.push_back(ret);
    return ret;
}

template<typename DescrType>
pType make_type(DescrType descr)
{
    return make_type(std::format(L"anon-{}", TypeRegistry::the().types.size()), std::move(descr));
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
