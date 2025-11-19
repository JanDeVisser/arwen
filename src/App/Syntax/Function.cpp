/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ostream>
#include <ranges>
#include <string>

#include <Util/Defer.h>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

ExternLink::ExternLink(std::wstring link_name)
    : link_name(std::move(link_name))
{
}

pType ExternLink::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

std::wostream &ExternLink::header(ASTNode const &, std::wostream &os)
{
    return os << link_name;
}

FunctionDeclaration::FunctionDeclaration(std::wstring name, ASTNodes generics, ASTNodes parameters, ASTNode return_type)
    : name(std::move(name))
    , generics(std::move(generics))
    , parameters(std::move(parameters))
    , return_type(std::move(return_type))
{
}

ASTNode FunctionDeclaration::normalize(ASTNode const &n)
{
    generics = normalize_nodes(generics);
    parameters = normalize_nodes(parameters);
    return_type = return_type->normalize();
    return n;
}

ASTNode FunctionDeclaration::stamp(ASTNode const &n)
{
    generics = stamp_nodes(generics);
    parameters = stamp_nodes(parameters);
    return_type = return_type->stamp();
    return n;
}

pType FunctionDeclaration::bind(ASTNode const &n)
{
    std::vector<pType> parameter_types;
    for (auto const &param : parameters) {
        auto param_type = param->bind();
        if (param->status != ASTNodeImpl::Status::Bound) {
            return param_type;
        }
        parameter_types.push_back(param_type);
    }
    auto result_type = return_type->bind();
    if (return_type->status != ASTNodeImpl::Status::Bound) {
        return result_type;
    }
    return TypeRegistry::the().function_of(parameter_types, result_type);
}

static pType bind_declaration(ASTNode const &declaration, ASTNode const &definition)
{
    assert(definition->ns.has_value());
    for (auto const &generic_param : get<FunctionDeclaration>(declaration).generics) {
        auto generic_id = get<Identifier>(generic_param);
        if (auto const &generic = definition->ns->find_type(generic_id.identifier); generic != nullptr) {
            assert(is<GenericParameter>(generic));
            continue;
        }
        definition->ns->register_type(generic_id.identifier, TypeRegistry::the().generic_parameter(generic_id.identifier));
    }
    if (auto t = declaration->bind(); declaration->status != ASTNodeImpl::Status::Bound) {
        return t;
    }
    return declaration->bound_type;
}

std::wostream &FunctionDeclaration::header(ASTNode const &, std::wostream &os)
{
    os << name;
    if (!generics.empty()) {
        wchar_t sep { '<' };
        for (auto const &gen : generics) {
            os << sep << get<Identifier>(gen).identifier;
            sep = ',';
        }
        os << '>';
    }
    return os << ": " << get<TypeSpecification>(return_type).to_string();
}

void FunctionDeclaration::dump_node(ASTNode const &, int const indent)
{
    for (auto const &gen : generics) {
        gen->dump(indent + 4);
    }
    for (auto const &param : parameters) {
        param->dump(indent + 4);
    }
}

FunctionDefinition::FunctionDefinition(std::wstring name, ASTNode declaration, ASTNode implementation)
    : name(std::move(name))
    , declaration(std::move(declaration))
    , implementation(std::move(implementation))
{
    assert(this->declaration != nullptr);
}

FunctionDefinition::FunctionDefinition(std::wstring name)
    : name(std::move(name))
{
}

ASTNode FunctionDefinition::normalize(ASTNode const &n)
{
    n->init_namespace();
    n->ns->parent->ns->register_function(name, n);
    declaration = declaration->normalize();
    implementation = implementation->normalize();
    return n;
}

ASTNode FunctionDefinition::stamp(ASTNode const &n)
{
    declaration = declaration->stamp();
    implementation = implementation->stamp();
    return n;
}

pType FunctionDefinition::bind(ASTNode const &n)
{
    pType t = bind_declaration(declaration, n);
    if (declaration->status != ASTNodeImpl::Status::Bound) {
        return t;
    }
    assert(is<FunctionType>(t));
    if (auto const &found = n->ns->parent->ns->find_function(name, t); found != nullptr && found != n) {
        return n.bind_error(L"Duplicate overload for function `{}` with type `{}`", name, t->to_string());
    }
    if (!get<FunctionDeclaration>(declaration).generics.empty()) {
        return TypeRegistry::void_;
    }
    for (auto const &param : get<FunctionDeclaration>(declaration).parameters) {
        auto parameter = get<Parameter>(param);
        if (!n.repo->has_variable(parameter.name)) {
            n.repo->register_variable(parameter.name, param);
        }
    }
    if (auto impl_type = implementation->bind(); implementation->status != ASTNodeImpl::Status::Bound) {
        return impl_type;
    }
    return TypeRegistry::void_;
}

void FunctionDefinition::dump_node(ASTNode const &, int indent)
{
    declaration->dump(indent + 4);
    implementation->dump(indent + 4);
}

ASTNode FunctionDefinition::instantiate(ASTNode const &n, std::vector<pType> const &generic_args) const
{
    auto decl = get<FunctionDeclaration>(declaration);
    if (generic_args.size() != decl.generics.size()) {
        n.error("Incompatible number of generic arguments");
        return nullptr;
    }
    std::map<std::wstring, pType> generic_args_map;
    for (auto const &[param, arg] : std::ranges::views::zip(decl.generics, generic_args)) {
        auto param_id = get<Identifier>(param);
        generic_args_map[param_id.identifier] = arg;
    }
    return instantiate(n, generic_args_map);
}

ASTNode FunctionDefinition::instantiate(ASTNode const &n, std::map<std::wstring, pType> const &generic_args) const
{
    auto const &decl = get<FunctionDeclaration>(declaration);
    assert(!decl.generics.empty() && decl.generics.size() == generic_args.size());
    ASTNode new_func = make_node<FunctionDefinition>(n, decl.name);
    new_func->init_namespace();
    {
        Defer _ { [&n]() { n.repo->pop_namespace(); } };
        for (auto const &[name, type] : generic_args) {
            new_func->ns->register_type(name, TypeRegistry::the().alias_for(type));
        }

        auto &def = get<FunctionDefinition>(new_func);
        def.declaration = make_node<FunctionDeclaration>(
            declaration,
            decl.name,
            ASTNodes {},
            stamp_nodes(decl.parameters),
            decl.return_type->stamp());
        def.implementation = implementation->stamp();
    }
    new_func->bind();
    // assert(new_func->declaration->bound_type != nullptr && !new_func->declaration->bound_type->is<Undetermined>());
    // std::wcout << "\ninstantiated " << name << ":\n";
    // new_func->dump();
    return new_func;
}

Parameter::Parameter(std::wstring name, ASTNode type_name)
    : name(std::move(name))
    , type_name(std::move(type_name))
{
}

ASTNode Parameter::normalize(ASTNode const &n)
{
    type_name = type_name->normalize();
    return n;
}

pType Parameter::bind(ASTNode const &n)
{
    return type_name->bind();
}

ASTNode Parameter::stamp(ASTNode const &n)
{
    return make_node<Parameter>(n, name, type_name->stamp());
}

std::wostream &Parameter::header(ASTNode const &, std::wostream &os)
{
    return os << name << ": " << get<TypeSpecification>(type_name).to_string();
}

Call::Call(ASTNode callable, ASTNode args)
    : callable(std::move(callable))
    , arguments(std::move(args))
{
    if (!is<Identifier>(this->callable) && !is<StampedIdentifier>(this->callable)) {
        NYI("Callable must be a function name");
    }
    assert(this->arguments != nullptr);
}

std::wostream &Call::header(ASTNode const &, std::wostream &os)
{
    return std::visit(overloads {
                          [&os](is_identifier auto const &id) -> std::wostream & {
                              return os << id.identifier;
                          },

                          [&os](auto const &) -> std::wostream & {
                              fatal("Invalid callable type");
                          },
                      },
        callable->node);
}

void Call::dump_node(ASTNode const &, int ident)
{
    callable->dump(ident + 4);
    arguments->dump(ident + 4);
}

ASTNode Call::stamp(ASTNode const &n)
{
    callable = callable->stamp();
    arguments = arguments->stamp();
    return n;
}

pType Call::bind(ASTNode const &n)
{
    if (function != nullptr) {
        if (function->status == ASTNodeImpl::Status::Initialized) {
            function = function->normalize();
        }
        function->bind();
        if (function->status != ASTNodeImpl::Status::Bound) {
            return function->bound_type;
        }
        auto const &f = get<FunctionDefinition>(function);
        auto const &[_, result] = get<FunctionType>(f.declaration->bound_type);
        return result;
    }

    auto arg_types = arguments->bind();
    if (!is<TypeList>(arg_types)) {
        return arg_types;
    }
    auto const  &type_descr = std::get<TypeList>(arg_types->description);
    std::wstring name;
    ASTNodes     type_args {};
    if (auto const *id = get_if<Identifier>(this->callable); id != nullptr) {
        name = id->identifier;
    } else if (auto const &stamped_id = get_if<StampedIdentifier>(this->callable); stamped_id != nullptr) {
        name = stamped_id->identifier;
        type_args = stamped_id->arguments;
    }
    // function = parser.find_function_by_arg_list(id->identifier, arg_types);
    // if (function != nullptr) {
    //     return std::get<FunctionType>(function->bound_type->description).result;
    // }
    //

    auto const &args = get<ExpressionList>(arguments);
    auto        match_non_generic_function = [&](ASTNode const &func_def) -> pType {
        auto const &def = get<FunctionDefinition>(func_def);
        auto const &decl = get<FunctionDeclaration>(def.declaration);
        if (decl.generics.empty()) {
            if (def.declaration->bound_type == nullptr || is<Undetermined>(def.declaration->bound_type)) {
                if (auto const &decl_type = bind_declaration(def.declaration, func_def); !is<FunctionType>(decl_type)) {
                    return TypeRegistry::void_;
                }
            }
            auto const &func_type_descr = get<FunctionType>(def.declaration->bound_type);
            for (auto [arg, param] : std::views::zip(type_descr.types, func_type_descr.parameters)) {
                if (arg == param) {
                    continue;
                }
                if (is<ReferenceType>(arg)) {
                    auto arg_descr { std::get<ReferenceType>(arg->description) };
                    if (arg_descr.referencing == param) {
                        continue;
                    }
                }
                return TypeRegistry::void_;
            }
            if (function == nullptr) {
                function = func_def;
                return TypeRegistry::void_;
            }
            return func_def.bind_error(L"Ambiguous function `{}{}`", name, arg_types->to_string());
        }
        return TypeRegistry::void_;
    };

    auto match_generic_function = [&](ASTNode const &func_def) -> pType {
        auto const &def = get<FunctionDefinition>(func_def);
        auto const &decl = get<FunctionDeclaration>(def.declaration);
        if (!decl.generics.empty()) {
            if (def.declaration->bound_type == nullptr || is<Undetermined>(def.declaration->bound_type)) {
                if (auto const &decl_type = bind_declaration(def.declaration, func_def); !is<FunctionType>(decl_type)) {
                    return TypeRegistry::void_;
                }
            }
            std::map<std::wstring, pType> generic_args;
            for (auto const &[param_name, arg_type] : std::ranges::views::zip(decl.generics, type_args)) {
                generic_args[std::wstring(identifier(param_name))] = get<TypeSpecification>(arg_type).resolve(func_def);
            }
            for (auto const &[param, arg] : std::ranges::views::zip(decl.parameters, args.expressions)) {
                auto const &param_type = param->bound_type;
                auto const &arg_type = arg->bound_type;
                for (auto const inferred = arg_type->infer_generic_arguments(param_type); auto const &[name, arg_type] : inferred) {
                    if (generic_args.contains(name) && generic_args[name] != arg_type) {
                        return n.bind_error(
                            L"Ambiguous values inferred for generic parameter  `{}`: `{}` and `{}`",
                            name, arg_types->to_string(), generic_args[name]->to_string());
                    }
                    generic_args[name] = arg_type;
                }
            }
            auto all_inferred { true };
            for (auto const &generic_param : decl.generics) {
                if (!generic_args.contains(std::wstring(identifier(generic_param)))) {
                    all_inferred = false;
                    break;
                }
            }
            if (!all_inferred) {
                return TypeRegistry::void_;
            }
            if (function == nullptr) {
                function = get<FunctionDefinition>(func_def).instantiate(func_def, generic_args);
                return TypeRegistry::void_;
            } else {
                return n.bind_error(L"Ambiguous function `{}{}`", name, arg_types->to_string());
            }
        }
        return TypeRegistry::void_;
    };

    ASTNodes bound_overloads;
    {
        auto const overloads = n.repo->find_overloads(name, type_args);
        for (auto func_def : overloads) {
            if (func_def->status == ASTNodeImpl::Status::Initialized) {
                func_def = func_def->normalize();
            }
            func_def->bind();
            bound_overloads.push_back(func_def);
        }
    }
    if (type_args.empty()) {
        for (auto const &func_def : bound_overloads) {
            auto const &def = get<FunctionDefinition>(func_def);
            auto const &decl = get<FunctionDeclaration>(def.declaration);
            if (decl.parameters.size() != args.expressions.size()) {
                continue;
            }
            if (auto const ret = match_non_generic_function(func_def); is<BindErrors>(ret)) {
                return ret;
            }
        }
        if (function == nullptr) {
            for (auto const &func_def : bound_overloads) {
                auto const &def = get<FunctionDefinition>(func_def);
                auto const &decl = get<FunctionDeclaration>(def.declaration);
                if (decl.parameters.size() != args.expressions.size()) {
                    continue;
                }
                if (auto const ret = match_generic_function(func_def); is<BindErrors>(ret)) {
                    return ret;
                }
            }
        }
    } else {
        for (auto const &func_def : bound_overloads) {
            auto const &def = get<FunctionDefinition>(func_def);
            auto const &decl = get<FunctionDeclaration>(def.declaration);
            if (decl.parameters.size() != args.expressions.size()) {
                continue;
            }
            if (auto const ret = match_generic_function(func_def); is<BindErrors>(ret)) {
                return ret;
            }
        }
    }
    if (function != nullptr) {
	auto const &func = get<FunctionDefinition>(function);
        if (function->bound_type == nullptr || is<Undetermined>(function->bound_type)) {
            function->bind();
        }
        if (is<Undetermined>(function->bound_type)) {
            return function->bound_type;
        }
        if (auto const impl_type = func.implementation->bind(); impl_type == nullptr || is<Undetermined>(impl_type)) {
            return TypeRegistry::undetermined;
        } else if (is<BindErrors>(impl_type)) {
            return impl_type;
        }
        auto const &func_type_descr = get<FunctionType>(func.declaration->bound_type);
        return func_type_descr.result;
    }
    if (n.repo->pass == 0) {
        return TypeRegistry::undetermined;
    }
    return n.bind_error(L"Unresolved function `{}{}`", name, arg_types->to_string());
}

}
