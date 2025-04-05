/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <memory>
#include <string>

#include <App/SyntaxNode.h>

namespace Arwen {

FunctionDeclaration::FunctionDeclaration(std::wstring name, std::vector<pParameter> parameters, pTypeSpecification return_type)
    : SyntaxNode(SyntaxNodeType::FunctionDeclaration)
    , name(std::move(name))
    , parameters(std::move(parameters))
    , return_type(std::move(return_type))
{
}

pSyntaxNode FunctionDeclaration::normalize(Parser &parser)
{
    std::vector<pParameter> normalized;
    for (auto const &param : parameters) {
        auto p = std::dynamic_pointer_cast<Parameter>(param->normalize(parser));
        assert(p != nullptr);
        normalized.push_back(p);
    }
    return make_node<FunctionDeclaration>(
        location,
        name,
        normalized,
        return_type);
}

pBoundNode FunctionDeclaration::bind()
{
    return nullptr;
}

void FunctionDeclaration::header()
{
    std::wcout << name << ": " << return_type->to_string();
}

void FunctionDeclaration::dump_node(int indent)
{
    for (auto &param : parameters) {
        param->dump(indent + 4);
    }
}

FunctionDefinition::FunctionDefinition(pFunctionDeclaration declaration, pSyntaxNode implementation)
    : SyntaxNode(SyntaxNodeType::FunctionDefinition)
    , declaration(std::move(declaration))
    , implementation(std::move(implementation))
{
    assert(this->declaration != nullptr);
}

pSyntaxNode FunctionDefinition::normalize(Parser &parser)
{
    return make_node<FunctionDefinition>(
        location,
        std::dynamic_pointer_cast<FunctionDeclaration>(declaration->normalize(parser)),
        implementation->normalize(parser));
}

pBoundNode FunctionDefinition::bind()
{
    return nullptr;
}

void FunctionDefinition::dump_node(int indent)
{
    declaration->dump(indent + 4);
    implementation->dump(indent + 4);
}

Parameter::Parameter(std::wstring name, pTypeSpecification type_name)
    : SyntaxNode(SyntaxNodeType::Parameter)
    , name(std::move(name))
    , type_name(std::move(type_name))
{
}

pBoundNode Parameter::bind()
{
    return nullptr;
}

void Parameter::header()
{
    std::wcout << name << ": " << type_name->to_string();
}

}
