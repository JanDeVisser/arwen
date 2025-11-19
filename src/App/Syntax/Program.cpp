/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdlib>
#include <iostream>
#include <string>

#include <Util/Defer.h>
#include <Util/Utf8.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Program::Program(std::wstring name, std::wstring source)
    : name(std::move(name))
    , source(std::move(source))
{
}

Program::Program(std::wstring name, std::map<std::wstring, ASTNode> modules, ASTNodes statements)
    : name(std::move(name))
    , modules(std::move(modules))
    , statements(std::move(statements))
{
}

ASTNode Program::normalize(ASTNode const &n)
{
    n->init_namespace();
    for (auto const &t : TypeRegistry::the().types) {
	n->ns->register_type(t.name, t.id);
    }
    for (auto &[name, mod] : modules) {
        auto const normalized = mod->normalize();
        if (normalized != nullptr && normalized != mod) {
            modules[std::get<Module>(mod->node).name] = normalized;
        }
    }
    statements = normalize_nodes(statements);
    return n;
}

pType Program::bind(ASTNode const &n)
{
    pType ret { nullptr };
    if (n.repo->pass == 0) {
        for (auto &[name, mod] : modules) {
            n->ns->register_variable(name, mod);
        }
    }
    for (auto &[_, mod] : modules) {
        if (auto t = mod->bind(); t == TypeRegistry::undetermined) {
            ret = t;
        }
    }
    for (auto &statement : statements) {
        if (auto t = statement->bind(); t == TypeRegistry::undetermined) {
            ret = t;
        }
    }
    if (ret == nullptr) {
        ret = make_type(as_wstring(name), NamespaceType {});
    }
    return ret;
}

std::wostream &Program::header(ASTNode const &n, std::wostream &os)
{
    return os << name;
}

void Program::dump_node(ASTNode const &n, int indent)
{
    for (auto const &stmt : statements) {
        stmt->dump(indent + 4);
    }
    for (auto &[_, mod] : modules) {
        mod->dump(indent + 4);
    }
}

}
