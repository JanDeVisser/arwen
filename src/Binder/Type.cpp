/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Binder/Type.h>

namespace Arwen {

TypeRegistry::TypeRegistry() {
#undef S
#define S(T, L) register_type(Type { .name = L, .typespec = BuiltinType::T });
    BuiltinTypes(S)
#undef S
}

TypeReference TypeRegistry::register_type(Type t)
{
    t.ref = types.size();
    index.emplace(t.name, t.ref);
    types.emplace_back(std::move(t));
    return t.ref;
}

std::optional<TypeReference> TypeRegistry::find(std::string_view name)
{
    if (auto it = index.find(name); it != index.end()) {
        return types[it->second].ref;
    }
    return {};
}

bool TypeRegistry::exists(std::string_view name) const
{
    return index.contains(name);
}

bool TypeRegistry::has(TypeReference ref) const
{
    return ref < types.size();
}

Type const &TypeRegistry::operator[](BuiltinType t) const
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
    assert(index.contains(name));
    return types[index.at(name)];
}

#if 0

std::optional<TypeReference> TypeRegistry::resolve_array(TypeReference element_type, std::optional<size_t> size)
{
    if (element_type >= types.size()) {
        return {};
    }
    for (auto const &t : types) {
        if (!std::holds_alternative<Array>(t.typespec)) {
            continue;
        }
        auto const &arr = std::get<Array>(t.typespec);
        if (arr.element_type == element_type && arr.size == size) {
            return t.ref;
        }
    }
    return register_type({
        .name = std::format("[{}]{}", (size) ? *size : 0, types[element_type].name),
        .typespec = Array {
            .element_type = element_type,
            .size = size,
        },
    });
}

std::optional<TypeReference> TypeRegistry::resolve_pointer(TypeReference element_type)
{
    if (element_type >= types.size()) {
        return {};
    }
    for (auto const &t : types) {
        if (!std::holds_alternative<Pointer>(t.typespec)) {
            continue;
        }
        auto const &arr = std::get<Pointer>(t.typespec);
        if (arr.element_type == element_type) {
            return t.ref;
        }
    }
    return register_type({
        .name = std::format("*{}", types[element_type].name),
        .typespec = Pointer {
            .element_type = element_type,
        },
    });
}

#endif

TypeRegistry &TypeRegistry::the()
{
    static TypeRegistry the_;
    return the_;
}

}
