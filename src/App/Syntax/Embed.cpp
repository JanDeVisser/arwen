/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Operator.h>
#include <App/Parser.h>
#include <Util/IO.h>
#include <Util/Logging.h>
#include <Util/Utf8.h>

namespace Arwen {

using namespace Util;

Embed::Embed(std::wstring_view file_name)
    : SyntaxNode(SyntaxNodeType::Embed)
    , file_name(file_name)
{
}

pSyntaxNode Embed::normalize()
{
    auto fname = as_utf8(file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        return make_node<DoubleQuotedString>(location, contents, false);
    } else {
        std::cerr << "Could not open '" << fname << "': " << contents_maybe.error().to_string() << std::endl;
        return nullptr;
    }
}

pBoundNode Embed::bind()
{
    std::cerr << "Cannot bind '@embed' statements; these should be eliminated during normalizing" << std::endl;
    return nullptr;
}

void Embed::header()
{
    std::wcout << file_name;
}

}
