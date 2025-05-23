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
#include <App/Value.h>

namespace Arwen {

Constant::Constant(Value value)
    : SyntaxNode(SyntaxNodeType::Constant)
    , bound_value(std::move(value))
{
}

std::wostream &Constant::header(std::wostream &os)
{
    assert(bound_value.has_value());
    os << bound_value.value();
    return os;
}

pType Constant::bind(Parser &parser)
{
    assert(bound_value.has_value());
    return bound_value->type;
}

BoolConstant::BoolConstant(bool value)
    : SyntaxNode(SyntaxNodeType::BoolConstant)
    , value(value)
{
}

pSyntaxNode BoolConstant::normalize(Parser &)
{
    return make_node<Constant>(location, Value { value });
}

std::wostream &BoolConstant::header(std::wostream &os)
{
    return os << ((value) ? L"True" : L"False");
}

Number::Number(std::wstring_view number, NumberType type)
    : SyntaxNode(SyntaxNodeType::Number)
    , number(number)
    , number_type(type)
{
}

std::wostream &Number::header(std::wostream &os)
{
    return os << number << L" " << as_wstring(NumberType_name(number_type));
}

pSyntaxNode Number::normalize(Parser &)
{
    switch (number_type) {
    case NumberType::Decimal: {
        char      *end_ptr;
        auto const narrow_string = as_utf8(number);
        auto const value = strtod(narrow_string.data(), &end_ptr);
        assert(end_ptr != narrow_string.data());
        return make_node<Constant>(location, Value { value });
    }
    default: {
        using T = int64_t;
        auto const value = string_to_integer<T>(number)
                               .or_else([this]() -> std::optional<T> {
                                   std::wcerr << "Could not convert string '" << number << "' to integer. This is unexpected" << std::endl;
                                   abort();
                                   return { 0 };
                               })
                               .value();
        return make_node<Constant>(location, Value { value });
    }
    }
}

QuotedString::QuotedString(std::wstring_view str, QuoteType type)
    : SyntaxNode(SyntaxNodeType::QuotedString)
    , string(str)
    , quote_type(type)
{
}

std::wostream &QuotedString::header(std::wostream &os)
{
    return os << string;
}

pSyntaxNode QuotedString::normalize(Parser &parser)
{
    switch (quote_type) {
    case QuoteType::DoubleQuote:
        return make_node<Constant>(location, make_value(string.substr(0, string.length() - 1).substr(1)));
    case QuoteType::SingleQuote:
        return make_node<Constant>(location, Value { string[1] });
    default:
        UNREACHABLE();
    }
}

}
