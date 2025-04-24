/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <memory>
#include <string>

#include <Util/Defer.h>
#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Program::Program(std::wstring name)
    : SyntaxNode(SyntaxNodeType::Program)
    , name(std::move(name))
{
}

pSyntaxNode Program::normalize(Parser &parser)
{
    for (auto &[name, mod] : modules) {
        auto normalized = std::dynamic_pointer_cast<Module>(mod->normalize(parser));
        if (normalized != nullptr && normalized != mod) {
            modules[mod->name] = normalized;
        }
    }
    return shared_from_this();
}

pType Program::bind(Parser &parser)
{
    parser.push_scope(shared_from_this());
    Defer pop_scope { [&parser]() { parser.pop_scope(); }};
    for (auto const& t : TypeRegistry::the().types) {
        parser.register_type(t->name, t);
    }
    pType ret { nullptr };
    for (auto &[_, mod] : modules) {
        if (auto t = bind_node(mod, parser); t == TypeRegistry::undetermined) {
            ret = t;
        }
    }
    if (ret == nullptr) {
        ret = make_type(as_wstring(name), NamespaceType {});
    }
    return ret;
}

void Program::header()
{
    std::wcout << name;
}

void Program::dump_node(int indent)
{
    for (auto &[_, mod] : modules) {
        mod->dump(indent + 4);
    }
}

}
