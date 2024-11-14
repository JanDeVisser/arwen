/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cassert>
#include <iostream>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeindex>
#include <variant>

#include <AST/AST.h>
#include <Result.h>
#include <Binder/Binder.h>
#include <Logging.h>

namespace Arwen {

BoundNodeReference Binder::bind_node(NodeReference ast_ref)
{
    auto const &ast_node = ast[ast_ref];
    auto        ret = std::visit(
        [this, ast_node](auto &impl) -> BoundNodeReference {
            using T = std::decay_t<decltype(impl)>;
            return Arwen::bind<T>(*this, ast_node.ref);
        },
        ast_node.impl);
    if (!bound_nodes[ret].type) {
        ++unbound;
    }
    return ret;
}

BoundNodeReference Binder::rebind_node(BoundNodeReference ref)
{
    auto const &node = bound_nodes[ref];
    auto        ret = ref;
    if (!node.type) {
        ret = std::visit(
            [this, node](auto &impl) -> BoundNodeReference {
                using T = std::decay_t<decltype(impl)>;
                return Arwen::rebind<T>(*this, node.ref);
            },
            node.impl);
    }
    if (!bound_nodes[ret].type) {
        ++unbound;
    }
    return ret;
}

Result<BoundNodeReference, bool> Binder::bind(NodeReference ast_entrypoint)
{
    errors.clear();
    unbound = 0;
    pass = 0;
    entrypoint = {};
    auto bound_entrypoint = bind_node(ast_entrypoint);
    std::cout << "Pass 0 - AST Transformation\n\n";
    dump(bound_entrypoint, "Program");
    list();
    std::cout << "\n";
    if (!errors.empty()) {
        return false;
    }
    for (auto t = bound_nodes[bound_entrypoint].type; !t; t = bound_nodes[bound_entrypoint].type) {
        errors.clear();
        auto prev = unbound;
        unbound = 0;
        ++pass;
        rebind_node(bound_entrypoint);
        std::cout << "\nPass " << pass << " - rebind\n\n";
        dump(bound_entrypoint, "Program");
        std::cout << "\n";
        list();
        std::cout << "\n";
        if (prev <= unbound) {
            bound_entrypoint = add_error(bound_entrypoint, "Infinite loop in bind stage");
        }
        if (!errors.empty()) {
            return false;
        }
    }
    entrypoint = bound_entrypoint;
    return bound_entrypoint;
}

void Binder::push_namespace(BoundNodeReference ref)
{
    namespaces.emplace_back(ref);
}

void Binder::set_name(std::string_view name, NodeReference ref)
{
    assert(!namespaces.empty());
    auto &ns = bound_nodes[namespaces.back()];
    ns.names.emplace(name, ref);
}

void Binder::pop_namespace()
{
    assert(!namespaces.empty());
    namespaces.pop_back();
}

std::optional<NodeReference> Binder::resolve(std::string_view name)
{
    assert(!namespaces.empty());
    for (ssize_t depth = static_cast<ssize_t>(namespaces.size()) - 1; depth >= 0; --depth) {
        auto &ns = bound_nodes[namespaces[depth]];
        if (auto it = ns.names.find(name); it != ns.names.end()) {
            return it->second;
        }
    }
    return {};
}

BoundNodeReference Binder::add_error(BoundNodeReference ref, std::string const &message)
{
    BoundNodeReference err = add_node<BindError>(bound_nodes[ref].ast_ref, bound_nodes[ref].location);
    auto      &impl = std::get<BindError>(bound_nodes[err].impl);
    impl.node = ref;
    impl.message = message;
    errors.emplace_back(err);
    return err;
}

std::map<std::type_index, std::string> BoundNode::type_names {};

void Binder::list()
{
    for (auto const& node : bound_nodes) {
        std::cout << node.ref << ". " << node.type_name();
        if (node.type) {
            std::cout << " " << registry[*node.type].name;
        }
        std::cout << "\n";
    }
}

void Binder::dump(BoundNodeReference ref, std::string_view caption, int indent)
{
    if (indent == 0 && !errors.empty()) { // Hack
        std::cout << "Errors:\n";
        for (auto const &err : errors) {
            std::cout << std::get<BindError>(bound_nodes[err].impl).message << "\n";
        }
    }
    BoundNode &node = bound_nodes[ref];
    std::cout << std::string(indent, ' ') << caption << ": " << ref << ". " << node.type_name();
    if (node.type) {
        std::cout << " " << registry[*node.type].name;
    }
    std::cout << "\n";
    return std::visit(
        [this, indent](auto const &impl) {
            dump<std::decay_t<decltype(impl)>>(*this, impl, indent);
        },
        node.impl);
}

void Binder::dump(std::optional<BoundNodeReference> ref, std::string_view caption, int indent)
{
    if (ref) {
        dump(*ref, caption, indent);
    } else {
        std::cout << std::string(indent, ' ') << caption << ": [empty]" << '\n';
    }
}

void Binder::dump(BoundNodeReferences refs, std::string_view caption, int indent)
{
    if (refs.empty()) {
        std::cout << std::string(indent, ' ') << caption << ": [empty]" << '\n';
    } else {
        for (auto ref : refs) {
            dump(ref, caption, indent);
        }
    }
}



}
