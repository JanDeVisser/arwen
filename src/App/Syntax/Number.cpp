/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <string_view>

#include <Util/Utf8.h>

#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

BoolConstant::BoolConstant(bool value)
    : ConstantExpression(SyntaxNodeType::BoolConstant)
    , value(value)
{
}

pType BoolConstant::bind(Parser &parser)
{
    return TypeRegistry::boolean;
}

void BoolConstant::header()
{
    std::wcout << ((value) ? L"True" : L"False");
}

pSyntaxNode BoolConstant::evaluate_LogicalInvert(pConstantExpression const &)
{
    return make_node<BoolConstant>(location, !value);
}

Number::Number(std::wstring_view number, NumberType type)
    : ConstantExpression(SyntaxNodeType::Number)
    , number(number)
    , number_type(type)
{
}

pType Number::bind(Parser &parser)
{
    return nullptr;
}

void Number::header()
{
    std::wcout << number << L" ";
    std::cout << NumberType_name(number_type);
}

pSyntaxNode Number::normalize(struct Parser &)
{
    if (number_type == NumberType::Decimal) {
        return make_node<Decimal>(location, number);
    }
    return make_node<Integer>(location, number);
}

}
