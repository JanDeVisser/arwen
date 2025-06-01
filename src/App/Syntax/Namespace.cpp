/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <functional>
#include <memory>

#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Namespace::Namespace(pNamespace parent)
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
        return parent->find_type(name);
    }
    return nullptr;
}

void Namespace::register_type(std::wstring name, pType type)
{
    assert(!types.contains(name));
    types[name] = std::move(type);
}

bool Namespace::has_type(std::wstring const& name) const
{
    return types.contains(name);
}

pSyntaxNode Namespace::find_variable(std::wstring const &name) const
{
    if (variables.contains(name)) {
        return variables.at(name);
    }
    if (parent != nullptr) {
        return parent->find_variable(name);
    }
    return nullptr;
}

bool Namespace::has_variable(std::wstring const& name) const
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

void Namespace::register_variable(std::wstring name, pSyntaxNode node)
{
    assert(!variables.contains(name));
    variables.emplace(name, std::move(node));
}

bool Namespace::has_function(std::wstring const& name) const
{
    return functions.contains(name);
}

Namespace::FunctionConstIter Namespace::find_function_here(std::wstring name, pType const &type) const
{
    assert(type->is<FunctionType>());
    for (auto it = functions.find(name); it != functions.end(); ++it) {
        if (it->second->bound_type == type) {
            return it;
        }
    }
    return functions.end();
}

pFunctionDefinition Namespace::find_function(std::wstring const &name, pType const &type) const
{
    assert(type->is<FunctionType>());
    if (auto here = find_function_here(name, type); here != functions.end()) {
        return here->second;
    }
    if (parent != nullptr) {
        return parent->find_function(name, type);
    }
    return nullptr;
}

pFunctionDefinition Namespace::find_function_by_arg_list(std::wstring const &name, pType const &type) const
{
    assert(type->is<TypeList>());
    auto const &type_descr = std::get<TypeList>(type->description);
    for (auto it = functions.find(name); it != functions.end(); ++it) {
        auto const &func_type = (*it).second->bound_type;
        if (!func_type->is<FunctionType>()) {
            continue;
        }
        auto const &func_type_descr = std::get<FunctionType>(func_type->description);
        if (func_type_descr.parameters == type_descr.types) {
            return (*it).second;
        }
    }
    if (parent != nullptr) {
        return parent->find_function_by_arg_list(name, type);
    }
    return nullptr;
}

std::vector<pFunctionDefinition> Namespace::find_overloads(std::wstring const &name, TypeSpecifications const &type_args) const
{
    std::function<void(std::shared_ptr<const Namespace> const &, std::vector<pFunctionDefinition>&)> find_them;
    find_them = [&name, &find_them, &type_args](std::shared_ptr<Namespace const> const &ns, std::vector<pFunctionDefinition> &overloads) -> void {
        for (auto it = ns->functions.find(name); it != ns->functions.end() && it->first == name; ++it) {
            if (auto const &func_def = it->second; func_def->declaration->generics.size() >= type_args.size()) {
                overloads.push_back(func_def);
            }
        }
        if (ns->parent != nullptr) {
            find_them(ns->parent, overloads);
        }
    };
    std::vector<pFunctionDefinition> ret;
    find_them(shared_from_this(), ret);
    return ret;
}

void Namespace::register_function(std::wstring name, pFunctionDefinition fnc)
{
    assert(fnc->bound_type == nullptr || find_function_here(fnc->name, fnc->bound_type) == functions.end());
    functions.emplace(name, fnc);
}

void Namespace::unregister_function(std::wstring name, pFunctionDefinition fnc)
{
    assert(fnc->bound_type->is<FunctionType>());
    if (auto it = find_function_here(name, fnc->bound_type); it != functions.end()) {
        functions.erase(it);
    }
}

}
