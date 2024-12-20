/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <format>
#include <iostream>
#include <ostream>

#include <AST/AST.h>
#include <Binder/Binder.h>
#include <Logging.h>

namespace Arwen {

#undef STRUCT
#define STRUCT BoundConstant

template<>
inline BoundNodeReference bind<BoolConstant>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<BoolConstant>(ast_node.impl);
    auto        ref = add_node<BoundConstant>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.value = ast_impl.value;
    return ref;
}

template<>
inline BoundNodeReference bind<FloatConstant>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<FloatConstant>(ast_node.impl);
    auto        ref = add_node<BoundConstant>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.value = ast_impl.value;
    return ref;
}

template<>
inline BoundNodeReference bind<IntConstant>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<IntConstant>(ast_node.impl);
    auto        ref = add_node<BoundConstant>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.value = ast_impl.value;
    return ref;
}

template<>
inline BoundNodeReference bind<StringConstant>(Binder &binder, NodeReference ast_ref, BoundNodeReference parent)
{
    auto const &ast_node = binder.ast[ast_ref];
    auto const &ast_impl = std::get<StringConstant>(ast_node.impl);
    auto        ref = add_node<BoundConstant>(binder, ast_node.ref, ast_node.location, parent);
    IMPL.value = ast_impl.value;
    return ref;
}

template<>
inline BoundNodeReference rebind<BoundConstant>(Binder &binder, BoundNodeReference ref)
{
    binder[ref].type = IMPL.value.type();
    return ref;
}

template<>
inline TypeAlternatives alternatives<BoundConstant>(Binder &binder, BoundNodeReference ref, TypeReference)
{
    TypeAlternatives result;
    auto             type = binder.registry[IMPL.value.type()];
    result.preferred = *binder[ref].type;
    if (binder.registry[IMPL.value.type()].is_float()) {
        result.alternatives.push_back(FloatType);
        result.alternatives.push_back(DoubleType);
        double v = IMPL.value.as<double>();
#undef S
#define S(T, CType, Size, Signed)                 \
    if (v <= std::numeric_limits<CType>::max()) { \
        result.alternatives.push_back(T##Type);   \
    }
        IntegerTypes(S)
#undef S
    }
    if (type.is_integer()) {
        result.alternatives.push_back(FloatType);
        result.alternatives.push_back(DoubleType);
        if (type.is_signed()) {
            i64 v = IMPL.value.as<i64>();
#undef S
#define S(T, CType, Size, Signed)                                                               \
    if (Signed) {                                                                               \
        if (v >= std::numeric_limits<CType>::min() && v <= std::numeric_limits<CType>::max()) { \
            result.alternatives.push_back(T##Type);                                             \
        }                                                                                       \
    } else {                                                                                    \
        if (v >= 0 && v <= std::numeric_limits<CType>::max()) {                                 \
            result.alternatives.push_back(T##Type);                                             \
        }                                                                                       \
    }
            IntegerTypes(S)
#undef S
        } else {
            u64 v = IMPL.value.as<u64>();
#undef S
#define S(T, CType, Size, Signed)                 \
    if (v <= std::numeric_limits<CType>::max()) { \
        result.alternatives.push_back(T##Type);   \
    }
            IntegerTypes(S)
#undef S
        }
    }
    return result;
}

template<>
inline BoundNodeReference accept<BoundConstant>(Binder &binder, BoundNodeReference ref, TypeReference type)
{
    binder[ref].type = type;
    switch (type) {
#undef S
#define S(T, CType, Size, Signed)            \
    case T##Type:                            \
        IMPL.value = IMPL.value.as<CType>(); \
        break;
    IntegerTypes(S)
#undef S
        case FloatType:
        IMPL.value = IMPL.value.as<float>();
        break;
    case DoubleType:
        IMPL.value = IMPL.value.as<double>();
        break;
    default:
        UNREACHABLE();
    }
    return ref;
}

template<>
inline void to_string(std::ostream &out, Binder &, BoundConstant const &impl)
{
    std::cout << std::format("{}", impl.value);
}

}
