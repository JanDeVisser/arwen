/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Lexer/Lexer.h>

namespace Arwen {

Token::Token(TokenKind kind, std::string_view text)
    : location()
    , text(text)
    , kind(kind)
{
}

[[nodiscard]] KindTag Token::tag() const
{
    return kind.tag();
}

bool Token::is(KindTag k) const
{
    return tag() == k;
}

bool Token::operator==(Token const &rhs) const
{
    if (kind != rhs.kind) {
        return false;
    }
    return location == rhs.location;
}

bool Token::operator<(Token const &rhs) const
{
    if (kind != rhs.kind) {
        return kind < rhs.kind;
    }
    return location < rhs.location;
}

}
