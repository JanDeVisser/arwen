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

std::wostream &ExternLink::header(std::wostream &os)
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
    return TypeRegistry::the().function_of(parameter_types, result_type);
}

std::wostream &FunctionDeclaration::header(std::wostream &os)
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
    if (auto const &t = bind_node(declaration, parser); t->is<BindErrors>() || t->is<Undetermined>()) {
        return t;
    } else {
        assert(t->is<FunctionType>());
        if (parser.has_function(name, t)) {
            return parser.bind_error(location, L"Duplicate overload for function `{}` with type `{}`", name, t->to_string());
        }
        bound_type = t; // Cheating? Needed when function is called recursively.
        parser.register_function(name, std::dynamic_pointer_cast<FunctionDefinition>(shared_from_this()));
        {
            parser.push_namespace(ns);
            Defer pop_namespace { [&parser]() { parser.pop_namespace(); } };
            if (parser.pass == 0) {
                for (auto const &param : declaration->parameters) {
                    parser.register_variable(param->name, param);
                }
            }
            bind_node(implementation, parser);
        }
        if (implementation->bound_type->is<Undetermined>()) {
            parser.unregister_function(name, std::dynamic_pointer_cast<FunctionDefinition>(shared_from_this()));
            return TypeRegistry::undetermined;
        }
        return t;
    }
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

std::wostream &Parameter::header(std::wostream &os)
{
    return os << name << ": " << type_name->to_string();
}

Call::Call(pSyntaxNode callable, pExpressionList args)
    : SyntaxNode(SyntaxNodeType::Call)
    , callable(std::move(callable))
    , arguments(std::move(args))
{
    if (auto id = std::dynamic_pointer_cast<Identifier>(this->callable); id == nullptr) {
        NYI("Callable must be a function name");
    }
    assert(this->arguments != nullptr);
}

std::wostream &Call::header(std::wostream &os)
{
    auto id = std::dynamic_pointer_cast<Identifier>(this->callable);
    assert(id != nullptr);
    return os << id->identifier;
}

void Call::dump_node(int ident)
{
    callable->dump_node(ident + 4);
    arguments->dump_node(ident + 4);
}

pType Call::bind(Parser &parser)
{
    auto arg_types = bind_node(arguments, parser);
    if (!arg_types->is<TypeList>()) {
        return arg_types;
    }
    auto id = std::dynamic_pointer_cast<Identifier>(this->callable);
    assert(id != nullptr);
    function = parser.find_function_by_arg_list(id->identifier, arg_types);
    if (function != nullptr) {
        return std::get<FunctionType>(function->bound_type->description).result;
    }
    if (parser.pass == 0) {
        return TypeRegistry::undetermined;
    } else {
        return parser.bind_error(
            location,
            std::format(L"Unresolved function `{}{}`", id->identifier, arg_types->to_string()));
    }
}

}
