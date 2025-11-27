/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <string>

#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

PublicDeclaration::PublicDeclaration(std::wstring name, ASTNode declaration)
    : name(std::move(name))
    , declaration(std::move(declaration))
{
    assert(this->declaration != nullptr);
}

ASTNode PublicDeclaration::normalize(ASTNode const &n)
{
    declaration = declaration->normalize();
    return n;
}

ASTNode PublicDeclaration::stamp(ASTNode const &n)
{
    declaration = declaration->stamp();
    return n;
}

pType PublicDeclaration::bind(ASTNode const &n)
{
    return declaration->bind();
}

void PublicDeclaration::dump_node(ASTNode const &, int indent)
{
    declaration->dump(indent + 4);
}

}
