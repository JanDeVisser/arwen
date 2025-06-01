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
#include <cstddef>

namespace Arwen {

using namespace Util;

Include::Include(std::wstring_view file_name)
    : SyntaxNode(SyntaxNodeType::Include)
    , file_name(file_name)
{
}

pSyntaxNode Include::normalize(Parser &parser)
{
    auto fname = as_utf8(file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        Parser      include_parser;
        include_parser.level = parser.level;
        auto node = include_parser.parse_file(std::move(contents), parser.namespaces.back());
        if (include_parser.level != parser.level) {
            parser.append(location, "Unbalanced block(s) in @include");
            return nullptr;
        }
        if (node) {
            node = normalize_node(node, parser);
            if (node) {
                node->location = location;
            }
            return node;
        }
        for (auto &err : include_parser.errors) {
            parser.errors.push_back(err);
        }
    } else {
        parser.append(location, "Could not open include file `{}`", fname);
    }
    return nullptr;
}

pType Include::bind(Parser &parser)
{
    return parser.bind_error(location, L"`@include` statement should have been elided");
}

std::wostream& Include::header(std::wostream &os)
{
    return os << file_name;
}

}
