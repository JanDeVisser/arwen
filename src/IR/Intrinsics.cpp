//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <IR/Execute.h>
#include <IR/Intrinsics.h>
#include <Type/Type.h>
#include <Type/Value.h>

namespace Arwen::IR {

void exit(Scope &scope)
{
    scope.exit();
}

void len(Scope &scope)
{
    auto value = scope.pop<SliceValue>();
    scope.push<u64>(value.len);
}

void ptr(Scope &scope)
{
    auto value = scope.pop<SliceValue>();
    scope.push<u8*>(value.ptr);
}

void make_string(Scope &scope)
{
    auto pointer = scope.pop<u8*>();
    auto length = scope.pop<u64>();
    scope.push<SliceValue>({ length, pointer });
}

}
