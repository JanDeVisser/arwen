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

Program::Program(std::wstring name, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::Program, ns)
    , name(std::move(name))
{
}

Program::Program(std::wstring name, std::map<std::wstring, pModule> modules, SyntaxNodes statements, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::Program, ns)
    , name(std::move(name))
    , modules(std::move(modules))
    , statements(std::move(statements))
{
}

Program::Program(std::wstring name, std::wstring source, SyntaxNodes statements, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::Program, ns)
    , name(std::move(name))
    , source(std::move(source))
    , statements(std::move(statements))
{
}

pSyntaxNode Program::normalize(Parser &parser)
{
    for (auto &[name, mod] : modules) {
        auto const normalized = std::dynamic_pointer_cast<Module>(normalize_node(mod, parser));
        if (normalized != nullptr && normalized != mod) {
            modules[mod->name] = normalized;
        }
    }
    statements = normalize_nodes(statements, parser);
    return shared_from_this();
}

pType Program::bind(Parser &parser)
{
    parser.push_namespace(ns);
    Defer pop_scope { [&parser]() { parser.pop_namespace(); } };
    pType ret { nullptr };
    if (parser.pass == 0) {
        for (auto &[_, mod] : modules) {
            ns->register_variable(mod->name, mod);
        }
    }
    for (auto &[_, mod] : modules) {
        if (auto t = bind_node(mod, parser); t == TypeRegistry::undetermined) {
            ret = t;
        }
    }
    for (auto &statement : statements) {
        if (auto t = bind_node(statement, parser); t == TypeRegistry::undetermined) {
            ret = t;
        }
    }
    if (ret == nullptr) {
        ret = make_type(as_wstring(name), NamespaceType {});
    }
    return ret;
}

std::wostream &Program::header(std::wostream &os)
{
    return os << name;
}

void Program::dump_node(int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
    for (auto &[_, mod] : modules) {
        mod->dump(indent + 4);
    }
}

}
