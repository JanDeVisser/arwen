/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <cstddef>

namespace Arwen {

Identifier::Identifier(std::wstring_view const identifier)
    : identifier(identifier)
{
}

pType Identifier::bind(ASTNode const &n)
{
    auto const &type = n.repo->type_of(identifier);
    if (type == nullptr) {
        if (n.repo->pass == 0) {
            return TypeRegistry::undetermined;
        } else {
            return n.bind_error(L"Unresolved identifier `{}`", identifier);
        }
    }
    return TypeRegistry::the().referencing(type);
}

std::wostream &Identifier::header(ASTNode const &, std::wostream &os)
{
    return os << identifier;
}

StampedIdentifier::StampedIdentifier(std::wstring_view const identifier, ASTNodes arguments)
    : identifier(identifier)
    , arguments(std::move(arguments))
{
}

ASTNode StampedIdentifier::stamp(ASTNode const &n)
{
    arguments = stamp_nodes(arguments);
    return n;
}

pType StampedIdentifier::bind(ASTNode const &n)
{
    auto const &type = n.repo->type_of(identifier);
    if (type == nullptr) {
        if (n.repo->pass == 0) {
            return TypeRegistry::undetermined;
        } else {
            return n.bind_error(L"Unresolved identifier `{}`", identifier);
        }
    }
    return type;
}

std::wostream &StampedIdentifier::header(ASTNode const &, std::wostream &os)
{
    return os << identifier;
}

VariableDeclaration::VariableDeclaration(std::wstring name, ASTNode type_name, ASTNode initializer, bool is_const)
    : name(std::move(name))
    , type_name(type_name)
    , initializer(std::move(initializer))
    , is_const(is_const)
{
}

pType VariableDeclaration::bind(ASTNode const &n)
{
    auto my_type = (type_name != nullptr) ? type_name->bind() : nullptr;
    auto init_type = (initializer != nullptr) ? initializer->bind() : nullptr;

    assert(my_type != nullptr || init_type != nullptr);

    if (my_type == nullptr) {
        my_type = init_type;
    } else if (my_type == TypeRegistry::undetermined || is<BindErrors>(my_type)) {
        return my_type;
    }
    if (init_type != nullptr) {
        if (init_type == TypeRegistry::undetermined || is<BindErrors>(my_type)) {
            return init_type;
        } else if (init_type == TypeRegistry::ambiguous) {
            return n.bind_error(L"Type ambiguity");
        }
        if (my_type != init_type) {
            auto coerced = initializer->coerce(my_type);
            if (coerced == nullptr) {
                return n.bind_error(
                    L"Type mismatch between declared type `{}` of `{}` and type of initializer value `{}`",
                    my_type->name, name, init_type->name);
            }
            initializer = coerced;
            init_type = initializer->bound_type;
            assert(my_type == init_type);
        }
    }
    if (n.repo->has_variable(name)) {
        return n.bind_error(L"Duplicate variable name `{}`", name);
    }
    n.repo->register_variable(name, n);
    return my_type;
}

ASTNode VariableDeclaration::normalize(ASTNode const &n)
{
    if (type_name != nullptr) {
        type_name = type_name->normalize();
    }
    if (initializer != nullptr) {
        initializer = initializer->normalize();
    }
    return n;
}

ASTNode VariableDeclaration::stamp(ASTNode const &n)
{
    if (type_name != nullptr) {
        type_name = type_name->stamp();
    }
    if (initializer != nullptr) {
        initializer = initializer->stamp();
    }
    return n;
}

void VariableDeclaration::dump_node(ASTNode const &, int indent)
{
    if (initializer != nullptr) {
        initializer->dump(indent + 4);
    }
}

std::wostream &VariableDeclaration::header(ASTNode const &, std::wostream &os)
{
    if (is_const) {
        os << "const ";
    }
    os << name;
    if (type_name) {
        os << ": " << get<TypeSpecification>(type_name).to_string();
    }
    return os;
}

}
