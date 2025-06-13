/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Identifier::Identifier(std::wstring_view const identifier)
    : SyntaxNode(SyntaxNodeType::Identifier)
    , identifier(identifier)
{
}

pSyntaxNode Identifier::stamp(Parser &)
{
    return make_node<Identifier>(location, identifier);
}

pType Identifier::bind(Parser &parser)
{
    auto const &type = parser.type_of(identifier);
    if (type == nullptr) {
        if (parser.pass == 0) {
            return TypeRegistry::undetermined;
        } else {
            return parser.bind_error(
                location,
                std::format(L"Unresolved identifier `{}`", identifier));
        }
    }
    return type;
}

std::wostream& Identifier::header(std::wostream &os)
{
    return os << identifier;
}

StampedIdentifier::StampedIdentifier(std::wstring_view const identifier, TypeSpecifications arguments)
    : SyntaxNode(SyntaxNodeType::StampedIdentifier)
    , identifier(identifier)
    , arguments(std::move(arguments))
{
}

pSyntaxNode StampedIdentifier::stamp(Parser &)
{
    return make_node<StampedIdentifier>(location, identifier, arguments);
}

pType StampedIdentifier::bind(Parser &parser)
{
    auto const &type = parser.type_of(identifier);
    if (type == nullptr) {
        if (parser.pass == 0) {
            return TypeRegistry::undetermined;
        } else {
            return parser.bind_error(
                location,
                std::format(L"Unresolved identifier `{}`", identifier));
        }
    }
    return type;
}

std::wostream &StampedIdentifier::header(std::wostream &os)
{
    return os << identifier;
}

VariableDeclaration::VariableDeclaration(std::wstring name, pTypeSpecification type_name, pSyntaxNode initializer, bool is_const)
    : SyntaxNode(SyntaxNodeType::VariableDeclaration)
    , name(std::move(name))
    , type_name(type_name)
    , initializer(std::move(initializer))
    , is_const(is_const)
{
}

pType VariableDeclaration::bind(Parser &parser)
{
    auto my_type = (type_name != nullptr) ? bind_node(type_name, parser) : nullptr;
    auto init_type = (initializer != nullptr) ? bind_node(initializer, parser) : nullptr;

    assert(my_type != nullptr || init_type != nullptr);

    if (my_type == nullptr) {
        my_type = init_type;
    } else if (my_type == TypeRegistry::undetermined || my_type->is<BindErrors>()) {
        return my_type;
    }
    if (init_type != nullptr) {
        if (init_type == TypeRegistry::undetermined || my_type->is<BindErrors>()) {
            return init_type;
        } else if (init_type == TypeRegistry::ambiguous) {
            return parser.bind_error(initializer->location, L"Type ambiguity");
        }
        if (my_type != init_type) {
            auto coerced = initializer->coerce(my_type, parser);
            if (coerced == nullptr) {
                return parser.bind_error(
                    location,
                    std::format(L"Type mismatch between declared type `{}` of `{}` and type of initializer value `{}`",
                        my_type->name,
                        name,
                        init_type->name));
            }
            initializer = coerced;
            init_type = initializer->bound_type;
            assert(my_type == init_type);
        }
    }
    if (parser.has_variable(name)) {
        return parser.bind_error(location, std::format(L"Duplicate variable name `{}`", name));
    }
    parser.register_variable(name, shared_from_this());
    return my_type;
}

pSyntaxNode VariableDeclaration::normalize(Parser &parser)
{
    return make_node<VariableDeclaration>(
        location,
        name,
        (type_name != nullptr) ? normalize_node(type_name, parser) : nullptr,
        (initializer != nullptr) ? normalize_node(initializer, parser) : nullptr,
        is_const);
}

pSyntaxNode VariableDeclaration::stamp(Parser &parser)
{
    return make_node<VariableDeclaration>(
        location,
        name,
        type_name,
        (initializer) ? stamp_node(initializer, parser) : nullptr,
        is_const);
}

void VariableDeclaration::dump_node(int indent)
{
    if (initializer != nullptr) {
        initializer->dump(indent + 4);
    }
}

std::wostream& VariableDeclaration::header(std::wostream &os)
{
    if (is_const) {
        os << "const ";
    }
    os << name;
    if (type_name) {
        os << ": " << type_name->to_string();
    }
    return os;
}

}
