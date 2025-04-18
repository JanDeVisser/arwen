/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

Identifier::Identifier(std::wstring_view identifier)
    : SyntaxNode(SyntaxNodeType::Identifier)
    , identifier(identifier)
{
}

pType Identifier::bind(Parser &parser)
{
    auto const &type = parser.find_name(identifier);
    if (type == nullptr) {
        return TypeRegistry::undetermined;
    }
    return type;
}

void Identifier::header()
{
    std::wcout << identifier;
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
    if (parser.find_name(name) != nullptr) {
        return parser.bind_error(location, std::format(L"Duplicate variable name `{}`", name));
    }
    parser.register_name(name, my_type);
    return my_type;
}

pSyntaxNode VariableDeclaration::normalize(Parser &parser)
{
    return make_node<VariableDeclaration>(
        location,
        name,
        type_name,
        (initializer) ? initializer->normalize(parser) : nullptr,
        is_const);
}

void VariableDeclaration::dump_node(int indent)
{
    if (initializer != nullptr) {
        initializer->dump(indent + 4);
    }
}

void VariableDeclaration::header()
{
    if (is_const) {
        std::wcout << "const ";
    }
    std::wcout << name;
    if (type_name) {
        std::wcout << ": " << type_name->to_string();
    }
}

}
