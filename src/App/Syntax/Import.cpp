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

Import::Import(std::wstring name)
    : name(std::move(name))
{
}

ASTNode Import::normalize(ASTNode const &n)
{
    auto fname = name;
    for (auto ix = 0; ix < fname.length(); ++ix) {
        if (fname[ix] == '.')
            fname[ix] = '/';
    }
    if (!fname.ends_with(L".arw")) {
        fname += L".arw";
    }
    if (auto contents_maybe = read_file_by_name<wchar_t>(as_utf8(fname)); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        auto        module = parse<Module>(*(n.repo), as_utf8(name), std::move(contents));
        if (module) {
	    module->location = n->location;
            return module->normalize();
        }
    } else {
        n.error(L"Could not open import file `{}`", fname);
    }
    return nullptr;
}

std::wostream &Import::header(ASTNode const &, std::wostream &os)
{
    return os << name;
}

}
