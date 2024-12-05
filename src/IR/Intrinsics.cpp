//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <IR/Execute.h>
#include <IR/Intrinsics.h>
#include <Type/Type.h>
#include <Type/Value.h>
#include <string_view>

namespace Arwen::IR {

void exit(Scope &scope)
{
    scope.exit();
}

void len(Scope &scope)
{
    auto value = scope.pop();
    assert(TypeRegistry::the()[value.type()].decay().typespec.tag() == TypeKind::Slice);
    auto sv = value.value<std::string_view>();
    scope.push(Value { static_cast<u64>(sv.length()) });
}

void ptr(Scope &scope)
{
    auto value = scope.pop();
    assert(TypeRegistry::the()[value.type()].decay().typespec.tag() == TypeKind::Slice);
    auto sv = value.value<std::string_view>();
    scope.push(Value { static_cast<void const *>(sv.data()) });
}

void make_string(Scope &scope)
{
    auto pointer = scope.pop();
    auto length = scope.pop();
    auto const *p = static_cast<char const*>(pointer.value<void const*>());
    std::string_view s { p, length.as<u64>() };
    scope.push(Value { s });
}

}
