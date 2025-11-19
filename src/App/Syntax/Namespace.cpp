/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <functional>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Namespace::Namespace(ASTNode parent)
    : parent(std::move(parent))
{
}

bool Namespace::is_registered(std::wstring const &name) const
{
    if (has_type(name)) {
        return true;
    }
    if (has_variable(name)) {
        return true;
    }
    return has_function(name);
}

pType Namespace::find_type(std::wstring const &name) const
{
    if (has_type(name)) {
        return types.at(name);
    }
    if (parent != nullptr) {
	assert(parent->ns);
        return parent->ns->find_type(name);
    }
    return nullptr;
}

void Namespace::register_type(std::wstring name, pType type)
{
    assert(!types.contains(name));
    types[name] = std::move(type);
}

bool Namespace::has_type(std::wstring const &name) const
{
    return types.contains(name);
}

ASTNode Namespace::find_variable(std::wstring const &name) const
{
    if (variables.contains(name)) {
        return variables.at(name);
    }
    if (parent != nullptr) {
	assert(parent->ns);
        return parent->ns->find_variable(name);
    }
    return nullptr;
}

bool Namespace::has_variable(std::wstring const &name) const
{
    return variables.contains(name);
}

pType Namespace::type_of(std::wstring const &name) const
{
    auto n = find_variable(name);
    if (n == nullptr) {
        return nullptr;
    }
    if (n->bound_type == nullptr) {
        n->bound_type = TypeRegistry::undetermined;
    }
    return n->bound_type;
}

void Namespace::register_variable(std::wstring name, ASTNode node)
{
    assert(!variables.contains(name));
    variables.emplace(name, std::move(node));
}

bool Namespace::has_function(std::wstring const &name) const
{
    return functions.contains(name);
}

Namespace::FunctionConstIter Namespace::find_function_here(std::wstring name, pType const &type) const
{
    assert(is<FunctionType>(type));
    for (auto it = functions.find(name); it != functions.end(); ++it) {
        if (it->second->bound_type == type) {
            return it;
        }
    }
    return functions.end();
}

ASTNode Namespace::find_function(std::wstring const &name, pType const &type) const
{
    assert(is<FunctionType>(type));
    if (auto here = find_function_here(name, type); here != functions.end()) {
        return here->second;
    }
    if (parent != nullptr) {
	assert(parent->ns);
        return parent->ns->find_function(name, type);
    }
    return nullptr;
}

ASTNode Namespace::find_function_by_arg_list(std::wstring const &name, pType const &type) const
{
    assert(is<TypeList>(type));
    auto const &type_descr = std::get<TypeList>(type->description);
    for (auto it = functions.find(name); it != functions.end(); ++it) {
        auto const &func_type = (*it).second->bound_type;
        if (!is<FunctionType>(func_type)) {
            continue;
        }
        auto const &func_type_descr = std::get<FunctionType>(func_type->description);
        if (func_type_descr.parameters == type_descr.types) {
            return (*it).second;
        }
    }
    if (parent != nullptr) {
	assert(parent->ns);
        return parent->ns->find_function_by_arg_list(name, type);
    }
    return nullptr;
}

ASTNodes Namespace::find_overloads(std::wstring const &name, ASTNodes const &type_args) const
{
    std::function<void(Namespace const &, ASTNodes &)> find_them;
    find_them = [&name, &find_them, &type_args](Namespace const &ns, ASTNodes &overloads) -> void {
        for (auto it = ns.functions.find(name); it != ns.functions.end() && it->first == name; ++it) {
            if (auto const &func_def = it->second; get<FunctionDeclaration>(get<FunctionDefinition>(func_def).declaration).generics.size() >= type_args.size()) {
                overloads.push_back(func_def);
            }
        }
        if (ns.parent != nullptr) {
	    assert(ns.parent->ns);
            find_them(ns.parent->ns.value(), overloads);
        }
    };
    ASTNodes ret;
    find_them(*this, ret);
    return ret;
}

void Namespace::register_function(std::wstring name, ASTNode fnc)
{
    auto const &def = get<FunctionDefinition>(fnc);
    assert(fnc->bound_type == nullptr || find_function_here(def.name, fnc->bound_type) == functions.end());
    functions.emplace(name, fnc);
}

void Namespace::unregister_function(std::wstring name, ASTNode fnc)
{
    assert(is<FunctionType>(fnc->bound_type));
    if (auto it = find_function_here(name, fnc->bound_type); it != functions.end()) {
        functions.erase(it);
    }
}

}
