/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <memory>
#include <string>

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

ExternLink::ExternLink(std::wstring link_name)
    : SyntaxNode(SyntaxNodeType::ExternLink)
    , link_name(std::move(link_name))
{
}

pType ExternLink::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

std::wostream& ExternLink::header(std::wostream &os)
{
    return os << link_name;
}

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

pType FunctionDeclaration::bind(Parser &parser)
{
    std::vector<pType> parameter_types;
    for (auto const &param : parameters) {
        auto param_type = bind_node(param, parser);
        if (param_type == TypeRegistry::ambiguous || param_type == TypeRegistry::undetermined) {
            return param_type;
        }
        parameter_types.push_back(param_type);
    }
    auto result_type = bind_node(return_type, parser);
    if (result_type == TypeRegistry::ambiguous || result_type == TypeRegistry::undetermined) {
        return result_type;
    }
    if (parser.find_name(name) != nullptr) {
        return parser.bind_error(location, L"Duplicate variable name `{}`", name);
    }
    return TypeRegistry::the().function_of(parameter_types, result_type);
}

std::wostream& FunctionDeclaration::header(std::wostream &os)
{
    return os << name << ": " << return_type->to_string();
}

void FunctionDeclaration::dump_node(int indent)
{
    for (auto &param : parameters) {
        param->dump(indent + 4);
    }
}

FunctionDefinition::FunctionDefinition(std::wstring name, pFunctionDeclaration declaration, pSyntaxNode implementation, pNamespace ns)
    : SyntaxNode(SyntaxNodeType::FunctionDefinition, std::move(ns))
    , name(std::move(name))
    , declaration(std::move(declaration))
    , implementation(std::move(implementation))
{
    assert(this->declaration != nullptr);
}

pSyntaxNode FunctionDefinition::normalize(Parser &parser)
{
    return make_node<FunctionDefinition>(
        location,
        name,
        std::dynamic_pointer_cast<FunctionDeclaration>(declaration->normalize(parser)),
        implementation->normalize(parser),
        ns);
}

pType FunctionDefinition::bind(Parser &parser)
{
    if (auto t = bind_node(declaration, parser); t->is<BindErrors>()) {
        return t;
    }
    if (parser.pass == 0) {
        parser.register_name(name, shared_from_this());
    }
    parser.push_namespace(ns);
    Defer pop_namespace { [&parser]() { parser.pop_namespace(); } };
    if (parser.pass == 0) {
        for (auto const &param : declaration->parameters) {
            parser.register_name(param->name, param);
        }
    }
    bind_node(implementation, parser);
    if (declaration->bound_type == TypeRegistry::undetermined || implementation->bound_type == TypeRegistry::undetermined) {
        return TypeRegistry::undetermined;
    }
    return declaration->bound_type;
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

pType Parameter::bind(Parser &parser)
{
    return bind_node(type_name, parser);
}

std::wostream& Parameter::header(std::wostream &os)
{
    return os << name << ": " << type_name->to_string();
}

}
