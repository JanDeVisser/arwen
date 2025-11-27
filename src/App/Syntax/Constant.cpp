/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <string_view>

#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

Constant::Constant(Value value)
    : bound_value(std::move(value))
{
}

std::wostream &Constant::header(ASTNode const &, std::wostream &os)
{
    assert(bound_value.has_value());
    os << bound_value.value();
    return os;
}

pType Constant::bind(ASTNode const &n)
{
    assert(bound_value.has_value());
    return bound_value->type;
}

BoolConstant::BoolConstant(bool value)
    : value(value)
{
}

ASTNode BoolConstant::normalize(ASTNode const &n)
{
    return make_node<Constant>(n, Value { value });
}

std::wostream &BoolConstant::header(ASTNode const &, std::wostream &os)
{
    return os << ((value) ? L"True" : L"False");
}

Nullptr::Nullptr()
{
}

ASTNode Nullptr::normalize(ASTNode const &n)
{
    return make_node<Constant>(n, Value { nullptr });
}

std::wostream &Nullptr::header(ASTNode const &, std::wostream &os)
{
    return os << L"Null";
}

Number::Number(std::wstring_view number, NumberType type)
    : number(number)
    , number_type(type)
{
}

std::wostream &Number::header(ASTNode const &, std::wostream &os)
{
    return os << number << L" " << as_wstring(NumberType_name(number_type));
}

ASTNode Number::normalize(ASTNode const &n)
{
    switch (number_type) {
    case NumberType::Decimal: {
        char      *end_ptr;
        auto const narrow_string = as_utf8(number);
        auto const value = strtod(narrow_string.data(), &end_ptr);
        assert(end_ptr != narrow_string.data());
        return make_node<Constant>(n, Value { value });
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
        return make_node<Constant>(n, Value { value });
    }
    }
}

QuotedString::QuotedString(std::wstring_view str, QuoteType type)
    : string(str)
    , quote_type(type)
{
}

std::wostream &QuotedString::header(ASTNode const &, std::wostream &os)
{
    return os << string;
}

ASTNode QuotedString::normalize(ASTNode const &n)
{
    switch (quote_type) {
    case QuoteType::DoubleQuote: {
        assert(string[0] = '"' && string.back() == '"');
        std::wstring s;
        bool         escape { false };
        for (auto const ch : string.substr(0, string.length() - 1).substr(1)) {
            if (escape) {
                switch (ch) {
                case 'n':
                    s += '\n';
                    break;
                case 'r':
                    s += '\r';
                    break;
                case 't':
                    s += '\t';
                default:
                    s += ch;
                }
                escape = false;
            } else {
                if (ch == '\\') {
                    escape = true;
                } else {
                    s += ch;
                }
            }
        }
        return make_node<Constant>(n, make_value(s));
    }
    case QuoteType::SingleQuote:
        return make_node<Constant>(n, Value { string[1] });
    default:
        UNREACHABLE();
    }
}
}
