/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Utf8.h>
#include <App/Operator.h>
#include <App/Parser.h>
#include <Util/IO.h>
#include <Util/Logging.h>

namespace Arwen {

using namespace Util;

Include::Include(std::wstring_view file_name)
    : SyntaxNode(SyntaxNodeType::Include)
    , file_name(file_name)
{
}

pSyntaxNode Include::normalize()
{
    auto fname = as_utf8(file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        Parser      parser;
        auto        node = parser.parse_file(contents);
        if (node) {
            node = node->normalize();
            if (node) {
                node->location = location;
            }
            return node;
        }
        std::cerr << "Syntax error" << std::endl;
    } else {
        std::cerr << "Could not open '" << fname << "': " << contents_maybe.error().to_string() << std::endl;
    }
    return nullptr;
}

pBoundNode Include::bind()
{
    std::cerr << "Cannot bind '@include' statements; these should be eliminated during normalizing" << std::endl;
    return nullptr;
}

void Include::header()
{
    std::wcout << file_name;
}

}
