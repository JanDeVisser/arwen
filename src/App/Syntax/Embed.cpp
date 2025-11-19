/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <Util/IO.h>
#include <Util/Logging.h>
#include <Util/Utf8.h>

namespace Arwen {

using namespace Util;

Embed::Embed(std::wstring_view file_name)
    : file_name(file_name)
{
}

ASTNode Embed::normalize(ASTNode const &n)
{
    auto fname = as_utf8(file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        return make_node<Constant>(n, make_value(contents));
    } else {
        n.error("Could not open `{}`: {}", fname, contents_maybe.error().to_string());
        return nullptr;
    }
}

std::wostream &Embed::header(ASTNode const &, std::wostream &os)
{
    return os << file_name;
}

}
