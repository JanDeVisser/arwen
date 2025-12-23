/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <expected>
#include <string>

#include <App/SyntaxNode.h>

namespace Arwen::QBE {

enum class AllocType {
    Unknown,
    Value,
    Reference,
};

struct Alloc {
    AllocType          type = AllocType::Unknown;
    std::optional<int> var = {};

    static Alloc value(int var)
    {
        return (var > 0) ? Alloc { AllocType::Value, var } : Alloc {};
    }

    static Alloc ref(int var)
    {
        return Alloc { AllocType::Reference, var };
    }
};

using GenResult = std::expected<Alloc, std::wstring>;
GenResult generate_qbe(ASTNode const &node);

}
