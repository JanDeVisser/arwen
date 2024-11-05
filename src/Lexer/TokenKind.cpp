/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Lexer/Lexer.h>

namespace Arwen {

KindTag TokenKind::tag() const
{
    return m_tag;
}

char TokenKind::symbol() const
{
    assert(tag() == KindTag::Symbol);
    return payload.symbol;
}

char TokenKind::quote() const
{
    assert(tag() == KindTag::String);
    return payload.symbol;
}

std::string_view TokenKind::keyword() const
{
    assert(tag() == KindTag::Keyword);
    return payload.sv;
}

std::string_view TokenKind::comment_marker() const
{
    assert(tag() == KindTag::Comment);
    return payload.sv;
}

NumberType TokenKind::number_type() const
{
    assert(tag() == KindTag::Number);
    return payload.number_type;
}

bool TokenKind::operator<(TokenKind const &rhs) const
{
    if (tag() != rhs.tag()) {
        return static_cast<int>(tag()) < static_cast<int>(rhs.tag());
    }
    switch (tag()) {
    case Arwen::KindTag::Comment:
        return comment_marker() < rhs.comment_marker();
    case Arwen::KindTag::Keyword:
        return keyword() < rhs.keyword();
    case Arwen::KindTag::Number:
        return static_cast<int>(number_type()) < static_cast<int>(rhs.number_type());
    case Arwen::KindTag::String:
        return quote() < rhs.quote();
    case Arwen::KindTag::Symbol:
        return symbol() < rhs.symbol();
    default:
        return false;
    }
}

bool TokenKind::operator==(TokenKind const &rhs) const
{
    if (tag() != rhs.tag()) {
        return false;
    }
    switch (tag()) {
    case Arwen::KindTag::Comment:
        return comment_marker() == rhs.comment_marker();
    case Arwen::KindTag::Keyword:
        return keyword() == rhs.keyword();
    case Arwen::KindTag::Number:
        return number_type() == rhs.number_type();
    case Arwen::KindTag::String:
        return quote() == rhs.quote();
    case Arwen::KindTag::Symbol:
        return symbol() == rhs.symbol();
    default:
        return true;
    }
}

}
