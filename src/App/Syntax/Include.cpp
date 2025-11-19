/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>

#include <Util/IO.h>
#include <Util/Logging.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>

namespace Arwen {

using namespace Util;

Include::Include(std::wstring_view file_name)
    : file_name(file_name)
{
}

ASTNode Include::normalize(ASTNode const &n)
{
    auto fname = as_utf8(file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        auto node = parse<Block>(*(n.repo), fname, std::move(contents));
        if (node) {
	    node->location = n->location;
            return node->normalize();
        }
    } else {
        n.error(L"Could not open include file `{}`", file_name);
    }
    return nullptr;
}

std::wostream &Include::header(ASTNode const &, std::wostream &os)
{
    return os << file_name;
}

}
