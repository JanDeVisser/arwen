/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>

#include <Util/Defer.h>
#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Program::Program(std::wstring name)
    : SyntaxNode(SyntaxNodeType::Program, std::make_shared<Namespace>())
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
    parser.push_namespace(ns);
    Defer pop_scope { [&parser]() { parser.pop_namespace(); }};
    pType ret { nullptr };
    if (parser.pass == 0) {
        assert(ns->types.empty());
        for (auto const& t : TypeRegistry::the().types) {
            ns->register_type(t->name, t);
        }
        for (auto &[_, mod] : modules) {
            ns->register_variable(mod->name, mod);
        }
    }
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

std::wostream& Program::header(std::wostream &os)
{
    return os << name;
}

void Program::dump_node(int indent)
{
    for (auto &[_, mod] : modules) {
        mod->dump(indent + 4);
    }
}

}
