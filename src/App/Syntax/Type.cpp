/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/SyntaxNode.h>
#include <iostream>
#include <memory>
#include <string>

namespace Arwen {

TypeSpecification::TypeSpecification(std::wstring name, TypeSpecifications arguments, pTypeSpecification error_type, TypeFlag flags)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , name(std::move(name))
    , arguments(std::move(arguments))
    , error_type(std::move(error_type))
    , flags(flags)
{
}

TypeSpecification::TypeSpecification(std::wstring name, pTypeSpecification error_type, TypeFlag flags)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , name(std::move(name))
    , error_type(std::move(error_type))
    , flags(flags)
{
}

TypeSpecification::TypeSpecification(std::wstring name, TypeFlag flags, pTypeSpecification error_type)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , name(std::move(name))
    , error_type(std::move(error_type))
    , flags(flags)
{
}

pSyntaxNode TypeSpecification::normalize(Parser &parser)
{
    TypeSpecifications args;
    for (auto const& arg : arguments) {
        args.push_back(std::dynamic_pointer_cast<TypeSpecification>(arg->normalize(parser)));
    }
    auto err = (error_type != nullptr) ? std::dynamic_pointer_cast<TypeSpecification>(error_type->normalize(parser)) : nullptr;
    return make_node<TypeSpecification>(location, name, args, err, flags);
}

pBoundNode TypeSpecification::bind()
{
    return nullptr;
}

void TypeSpecification::header()
{
    std::wcout << to_string();
}

std::wstring TypeSpecification::to_string()
{
    std::wstring ret {};
    if ((flags & TypeFlag::Slice) != TypeFlag::None) {
        ret += L"[]";
    } else if ((flags & TypeFlag::NullTerminatedArray) != TypeFlag::None) {
        ret += L"[0]";
    } else if ((flags & TypeFlag::Array) != TypeFlag::None) {
        ret += L"[*]";
    }
    ret += name;
    if (!arguments.empty()) {
        wchar_t sep = '<';
        for (auto const &arg : arguments) {
            ret += sep;
            sep = ',';
            ret += arg->to_string();
        }
        ret += '>';
    }
    if (error_type != nullptr) {
        ret += '/';
        ret += error_type->to_string();
    }
    if ((flags & TypeFlag::Optional) != TypeFlag::None) {
        ret += '?';
    }
    return ret;
}

}
