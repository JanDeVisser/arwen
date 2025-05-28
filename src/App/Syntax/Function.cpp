/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <memory>
#include <ranges>
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

pSyntaxNode ExternLink::stamp(Parser &)
{
    return make_node<ExternLink>(location, link_name);
}

pType ExternLink::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

std::wostream &ExternLink::header(std::wostream &os)
{
    return os << link_name;
}

FunctionDeclaration::FunctionDeclaration(std::wstring name, Identifiers generics, std::vector<pParameter> parameters, pTypeSpecification return_type)
    : SyntaxNode(SyntaxNodeType::FunctionDeclaration)
    , name(std::move(name))
    , generics(std::move(generics))
    , parameters(std::move(parameters))
    , return_type(std::move(return_type))
{
}

pSyntaxNode FunctionDeclaration::normalize(Parser &parser)
{
    return make_node<FunctionDeclaration>(
        location,
        name,
        normalize_nodes(generics, parser),
        normalize_nodes(parameters, parser),
        normalize_node(return_type, parser));
}

pSyntaxNode FunctionDeclaration::stamp(Parser &parser)
{
    return make_node<FunctionDeclaration>(
        location,
        name,
        stamp_nodes(generics, parser),
        stamp_nodes(parameters, parser),
        stamp_node(return_type, parser));
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

static pType bind_declaration(pFunctionDeclaration const &declaration, pNamespace const &ns, Parser &parser)
{
    for (auto const &generic_param : declaration->generics) {
        if (auto const &generic = ns->find_type(generic_param->identifier); generic != nullptr) {
            assert(generic->is<GenericParameter>());
            continue;
        }
        ns->register_type(generic_param->identifier, TypeRegistry::the().generic_parameter(generic_param->identifier));
    }
    {
        parser.push_namespace(ns);
        Defer pop_namespace { [&parser]() { parser.pop_namespace(); } };
        if (auto t = bind_node(declaration, parser); t->is<BindErrors>() || t->is<Undetermined>()) {
            return t;
        }
    }
    return declaration->bound_type;
}

std::wostream &FunctionDeclaration::header(std::wostream &os)
{
    os << name;
    if (!generics.empty()) {
        wchar_t sep { '<' };
        for (auto const &gen : generics) {
            os << sep << gen->identifier;
            sep = ',';
        }
        os << '>';
    }
    return os << ": " << return_type->to_string();
}

void FunctionDeclaration::dump_node(int indent)
{
    for (auto const &gen : generics) {
        gen->dump(indent + 4);
    }
    for (auto const &param : parameters) {
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
    auto const ret = make_node<FunctionDefinition>(
        location,
        name,
        normalize_node(declaration, parser),
        normalize_node(implementation, parser),
        ns);
    parser.register_function(name, ret);
    return ret;
}

pSyntaxNode FunctionDefinition::stamp(Parser &parser)
{
    return make_node<FunctionDefinition>(
        location,
        name,
        stamp_node(declaration, parser),
        stamp_node(implementation, parser),
        ns);
}

pType FunctionDefinition::bind(Parser &parser)
{
    pType t = bind_declaration(declaration, ns, parser);
    assert(t->is<FunctionType>());
    if (auto found = ns->parent->find_function(name, t); found != nullptr && found != shared_from_this()) {
        return parser.bind_error(location, L"Duplicate overload for function `{}` with type `{}`", name, t->to_string());
    }
    if (!declaration->generics.empty()) {
        return TypeRegistry::void_;
    }
    parser.push_namespace(ns);
    Defer pop_namespace { [&parser]() { parser.pop_namespace(); } };
    bound_type = t; // Cheating? Needed when function is called recursively.
    if (parser.pass == 0) {
        for (auto const &param : declaration->parameters) {
            parser.register_variable(param->name, param);
        }
    }
    if (auto impl_type = bind_node(implementation, parser); t->is<Undetermined>() || t->is<BindErrors>()) {
        return impl_type;
    }
    return t;
}

void FunctionDefinition::dump_node(int indent)
{
    declaration->dump(indent + 4);
    implementation->dump(indent + 4);
}

pFunctionDefinition FunctionDefinition::instantiate(Parser &parser, std::vector<pType> const &generic_args) const
{
    if (generic_args.size() != declaration->generics.size()) {
        parser.append(location, "Incompatible number of generic arguments");
        return nullptr;
    }
    std::map<std::wstring, pType> generic_args_map;
    for (auto const &[param, arg] : std::ranges::views::zip(declaration->generics, generic_args)) {
        generic_args_map[param->identifier] = arg;
    }
    return instantiate(parser, generic_args_map);
}

pFunctionDefinition FunctionDefinition::instantiate(Parser &parser, std::map<std::wstring, pType> const &generic_args) const
{
    assert(!declaration->generics.empty() && declaration->generics.size() == generic_args.size());
    pFunctionDefinition new_func;
    {
        pNamespace new_ns = parser.push_new_namespace();
        Defer      pop_ns { [&parser]() { parser.pop_namespace(); } };
        for (auto const &[name, type] : generic_args) {
            new_ns->register_type(name, TypeRegistry::the().alias_for(type));
        }

        auto new_decl = make_node<FunctionDeclaration>(
            declaration->location,
            declaration->name,
            Identifiers {},
            stamp_nodes(declaration->parameters, parser),
            stamp_node(declaration->return_type, parser));

        auto new_impl = stamp_node(implementation, parser);
        assert(new_impl != nullptr);

        new_func = make_node<FunctionDefinition>(
            location,
            new_decl->name,
            new_decl,
            new_impl,
            new_ns);
    }
    bind_node(new_func, parser);
    assert(new_func->declaration->bound_type != nullptr && !new_func->declaration->bound_type->is<Undetermined>());
    // std::wcout << "\ninstantiated " << name << ":\n";
    // new_func->dump();
    return new_func;
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

pSyntaxNode Parameter::stamp(Parser &parser)
{
    return make_node<Parameter>(location, name, stamp_node(type_name, parser));
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
    if (this->callable->type != SyntaxNodeType::Identifier && this->callable->type != SyntaxNodeType::StampedIdentifier) {
        NYI("Callable must be a function name");
    }
    assert(this->arguments != nullptr);
}

std::wostream &Call::header(std::wostream &os)
{
    if (auto const &id = std::dynamic_pointer_cast<Identifier>(this->callable); id != nullptr) {
        return os << id->identifier;
    }
    if (auto const &id = std::dynamic_pointer_cast<StampedIdentifier>(this->callable); id != nullptr) {
        return os << id->identifier;
    }
    fatal("Invalid callable type");
}

void Call::dump_node(int ident)
{
    callable->dump_node(ident + 4);
    arguments->dump_node(ident + 4);
}

pSyntaxNode Call::stamp(Parser &parser)
{
    return make_node<Call>(location, stamp_node(callable, parser), stamp_node(arguments, parser));
}

pType Call::bind(Parser &parser)
{
    if (function != nullptr) {
        bind_node(function, parser);
        if (function->bound_type->is<Undetermined>()) {
            return function->bound_type;
        }
        auto const &func_type_descr = std::get<FunctionType>(function->declaration->bound_type->description);
        return func_type_descr.result;
    }

    auto arg_types = bind_node(arguments, parser);
    if (!arg_types->is<TypeList>()) {
        return arg_types;
    }
    auto const        &type_descr = std::get<TypeList>(arg_types->description);
    std::wstring       name;
    TypeSpecifications type_args {};
    if (auto const &id = std::dynamic_pointer_cast<Identifier>(this->callable); id != nullptr) {
        name = id->identifier;
    } else if (auto const &stamped_id = std::dynamic_pointer_cast<StampedIdentifier>(this->callable); stamped_id != nullptr) {
        name = stamped_id->identifier;
        type_args = stamped_id->arguments;
    }
    // function = parser.find_function_by_arg_list(id->identifier, arg_types);
    // if (function != nullptr) {
    //     return std::get<FunctionType>(function->bound_type->description).result;
    // }

    auto match_non_generic_function = [this, &type_descr, &parser, &name, &arg_types](pFunctionDefinition const &func_def) -> pType {
        if (func_def->declaration->generics.empty()) {
            if (func_def->declaration->bound_type == nullptr || func_def->declaration->bound_type->is<Undetermined>()) {
                if (auto const &decl_type = bind_declaration(func_def->declaration, parser.namespaces.back(), parser); !decl_type->is<FunctionType>()) {
                    return TypeRegistry::void_;
                }
            }
            auto const &func_type_descr = std::get<FunctionType>(func_def->declaration->bound_type->description);
            if (func_type_descr.parameters == type_descr.types) {
                if (function == nullptr) {
                    function = func_def;
                    return TypeRegistry::void_;
                }
                return parser.bind_error(
                    location,
                    std::format(L"Ambiguous function `{}{}`", name, arg_types->to_string()));
            }
        }
        return TypeRegistry::void_;
    };

    auto match_generic_function = [this, &parser, &name, &type_args, &arg_types](pFunctionDefinition const &func_def) -> pType {
        if (!func_def->declaration->generics.empty()) {
            if (func_def->declaration->bound_type == nullptr || func_def->declaration->bound_type->is<Undetermined>()) {
                if (auto const &decl_type = bind_declaration(func_def->declaration, parser.namespaces.back(), parser); !decl_type->is<FunctionType>()) {
                    return TypeRegistry::void_;
                }
            }
            std::map<std::wstring, pType> generic_args;
            for (auto const &[param_name, arg_type] : std::ranges::views::zip(func_def->declaration->generics, type_args)) {
                generic_args[param_name->identifier] = arg_type->resolve(parser);
            }
            for (auto const &[param, arg] : std::ranges::views::zip(func_def->declaration->parameters, arguments->expressions)) {
                auto const &param_type = param->bound_type;
                auto const &arg_type = arg->bound_type;
                for (auto const inferred = arg_type->infer_generic_arguments(param_type); auto const &[name, arg_type] : inferred) {
                    if (generic_args.contains(name) && generic_args[name] != arg_type) {
                        return parser.bind_error(
                            location,
                            std::format(L"Ambiguous values inferred for generic parameter  `{}`: `{}` and `{}`", name, arg_types->to_string(), generic_args[name]->to_string()));
                    }
                    generic_args[name] = arg_type;
                }
            }
            auto all_inferred { true };
            for (auto const &generic_param : func_def->declaration->generics) {
                if (!generic_args.contains(generic_param->identifier)) {
                    all_inferred = false;
                    break;
                }
            }
            if (!all_inferred) {
                return TypeRegistry::void_;
            }
            if (function == nullptr) {
                function = func_def->instantiate(parser, generic_args);
                return TypeRegistry::void_;
            } else {
                return parser.bind_error(
                    location,
                    std::format(L"Ambiguous function `{}{}`", name, arg_types->to_string()));
            }
        }
        return TypeRegistry::void_;
    };

    auto const overloads = parser.find_overloads(name, type_args);
    if (type_args.empty()) {
        for (auto const &func_def : overloads) {
            if (func_def->declaration->parameters.size() != arguments->expressions.size()) {
                continue;
            }
            if (auto const ret = match_non_generic_function(func_def); ret->is<BindErrors>()) {
                return ret;
            }
        }
        if (function == nullptr) {
            for (auto const &func_def : overloads) {
                if (func_def->declaration->parameters.size() != arguments->expressions.size()) {
                    continue;
                }
                if (auto const ret = match_generic_function(func_def); ret->is<BindErrors>()) {
                    return ret;
                }
            }
        }
    } else {
        for (auto const &func_def : overloads) {
            if (func_def->declaration->parameters.size() != arguments->expressions.size()) {
                continue;
            }
            if (auto const ret = match_generic_function(func_def); ret->is<BindErrors>()) {
                return ret;
            }
        }
    }
    if (function != nullptr) {
        if (function->bound_type == nullptr || function->bound_type->is<Undetermined>()) {
            bind_node(function, parser);
        }
        if (function->bound_type->is<Undetermined>()) {
            return function->bound_type;
        }
        auto const &func_type_descr = std::get<FunctionType>(function->declaration->bound_type->description);
        return func_type_descr.result;
    }
    if (parser.pass == 0) {
        return TypeRegistry::undetermined;
    }
    return parser.bind_error(
        location,
        std::format(L"Unresolved function `{}{}`", name, arg_types->to_string()));
}

}
