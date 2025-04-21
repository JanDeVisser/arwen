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
#include <cstddef>
#include <memory>

namespace Arwen {

using namespace Util;

Import::Import(std::wstring name)
    : SyntaxNode(SyntaxNodeType::Import)
    , name(std::move(name))
{
}

pSyntaxNode Import::normalize(Parser &parser)
{
    auto fname = name;
    for (auto ix = 0; ix < fname.length(); ++ix) {
        if (fname[ix] == '.') fname[ix] = '/';
    }
    if (!fname.ends_with(L".arw")) {
        fname += L".arw";
    }
    if (auto contents_maybe = read_file_by_name<wchar_t>(as_utf8(fname)); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        auto        module = parser.parse_module(as_utf8(name), std::move(contents));
        if (module) {
            module = std::dynamic_pointer_cast<Module>(module->normalize(parser));
            if (module) {
                module->location = location;
            }
            parser.program->modules[name] = module;
            return module;
        }
    } else {
        parser.append(location, L"Could not open import file `{}`", fname);
    }
    return nullptr;
}

pType Import::bind(Parser &parser)
{
    return parser.bind_error(location, L"`@include` statement should have been elided");
}

void Import::header()
{
    std::wcout << name;
}

}
