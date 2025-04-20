/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <string>

#include <Util/Utf8.h>

#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

PublicDeclaration::PublicDeclaration(std::wstring name, pSyntaxNode declaration)
    : SyntaxNode(SyntaxNodeType::PublicDeclaration)
    , name(std::move(name))
    , declaration(std::move(declaration))
{
    assert(this->declaration != nullptr);
}

pSyntaxNode PublicDeclaration::normalize(Parser &parser)
{
    return make_node<PublicDeclaration>(location, name, declaration->normalize(parser));
}

pType PublicDeclaration::bind(Parser &parser)
{
    return bind_node(declaration, parser);
}

void PublicDeclaration::dump_node(int indent)
{
    declaration->dump(indent + 4);
}

}
