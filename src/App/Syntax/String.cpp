/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

QuotedString::QuotedString(std::wstring_view str, QuoteType type)
    : SyntaxNode(SyntaxNodeType::QuotedString)
    , string(str)
    , quote_type(type)
{
}

pType QuotedString::bind(Parser &parser)
{
    return parser.bind_error(location, L"QuotedString node should have been elided");
}

std::wostream &QuotedString::header(std::wostream &os)
{
    return os << string;
}

pSyntaxNode QuotedString::normalize(Parser &parser)
{
    switch (quote_type) {
    case QuoteType::DoubleQuote:
        return make_node<DoubleQuotedString>(location, string, true);
    case QuoteType::SingleQuote:
        return make_node<SingleQuotedString>(location, string, true);
    default:
        UNREACHABLE();
    }
}

DoubleQuotedString::DoubleQuotedString(std::wstring_view string, bool strip_quotes)
    : ConstantExpression(SyntaxNodeType::DoubleQuotedString)
    , string(strip_quotes ? string.substr(0, string.length() - 1).substr(1) : string)
{
}

pType DoubleQuotedString::bind(Parser &parser)
{
    return TypeRegistry::string;
}

std::wostream &DoubleQuotedString::header(std::wostream &os)
{
    return os << string;
}

pSyntaxNode DoubleQuotedString::evaluate_Add(pConstantExpression const &rhs)
{
    if (auto rhs_string = std::dynamic_pointer_cast<DoubleQuotedString>(rhs); rhs_string != nullptr) {
        return make_node<DoubleQuotedString>(location + rhs->location, string + rhs_string->string, false);
    }
    return nullptr;
}

SingleQuotedString::SingleQuotedString(std::wstring_view string, bool strip_quotes)
    : ConstantExpression(SyntaxNodeType::SingleQuotedString)
    , string(strip_quotes ? string.substr(0, string.length() - 1).substr(1) : string)
{
}

pType SingleQuotedString::bind(Parser &parser)
{
    return TypeRegistry::character;
}

std::wostream &SingleQuotedString::header(std::wostream &os)
{
    return os << string;
}

}
