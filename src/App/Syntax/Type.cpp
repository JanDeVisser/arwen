/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <memory>
#include <string>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

template<typename T>
pType resolve(T const &, Parser &)
{
    fatal("No resolve method for `{}`", typeid(T).name());
}

template<>
pType resolve(TypeNameNode const &d, Parser &parser)
{
    auto t = parser.find_type(d.name);
    if (t == nullptr) {
        return nullptr;
    }
    while (t != nullptr && t->is<TypeAlias>()) {
        t = std::get<TypeAlias>(t->description).alias_of;
    }
    return t;
}

template<>
pType resolve(ReferenceDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().referencing(d.referencing->resolve(parser));
}

pType resolve(SliceDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().slice_of(d.slice_of->resolve(parser));
}

pType resolve(ZeroTerminatedArrayDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().zero_terminated_array_of(d.array_of->resolve(parser));
}

pType resolve(ArrayDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().array_of(d.array_of->resolve(parser), d.size);
}

pType resolve(DynArrayDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().dyn_array_of(d.array_of->resolve(parser));
}

pType resolve(OptionalDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().optional_of(d.optional_of->resolve(parser));
}

pType resolve(ErrorDescriptionNode const &d, Parser &parser)
{
    return TypeRegistry::the().error_of(d.success->resolve(parser), d.error->resolve(parser));
}

TypeSpecification::TypeSpecification(TypeSpecificationDescription description)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(description)
{
}

TypeSpecification::TypeSpecification(TypeNameNode type)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(type)
{
}

TypeSpecification::TypeSpecification(ReferenceDescriptionNode ref)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(ref)
{
}

TypeSpecification::TypeSpecification(SliceDescriptionNode slice)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(slice)
{
}

TypeSpecification::TypeSpecification(ZeroTerminatedArrayDescriptionNode array)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(array)
{
}

TypeSpecification::TypeSpecification(ArrayDescriptionNode array)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(array)
{
}

TypeSpecification::TypeSpecification(DynArrayDescriptionNode array)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(array)
{
}

TypeSpecification::TypeSpecification(OptionalDescriptionNode optional)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(optional)
{
}

TypeSpecification::TypeSpecification(ErrorDescriptionNode error)
    : SyntaxNode(SyntaxNodeType::TypeSpecification)
    , description(error)
{
}

pSyntaxNode TypeSpecification::normalize(Parser &parser)
{
    auto descr = std::visit(overloads {
                                [this, &parser](TypeNameNode const &d) -> TypeSpecificationDescription {
                                    TypeSpecifications args;
                                    for (auto const &arg : d.arguments) {
                                        args.push_back(std::dynamic_pointer_cast<TypeSpecification>(arg->normalize(parser)));
                                    }
                                    return TypeNameNode { d.name, args };
                                },
                                [this, &parser](ReferenceDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ReferenceDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.referencing->normalize(parser)) };
                                },
                                [this, &parser](SliceDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return SliceDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.slice_of->normalize(parser)) };
                                },
                                [this, &parser](ZeroTerminatedArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ZeroTerminatedArrayDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.array_of->normalize(parser)) };
                                },
                                [this, &parser](ArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ArrayDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.array_of->normalize(parser)), d.size };
                                },
                                [this, &parser](DynArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return DynArrayDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.array_of->normalize(parser)) };
                                },
                                [this, &parser](OptionalDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return OptionalDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.optional_of->normalize(parser)) };
                                },
                                [this, &parser](ErrorDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ErrorDescriptionNode {
                                        std::dynamic_pointer_cast<TypeSpecification>(d.success->normalize(parser)),
                                        std::dynamic_pointer_cast<TypeSpecification>(d.error->normalize(parser)),
                                    };
                                },
                            },
        this->description);
    auto ret = make_node<TypeSpecification>(location, description);
    ret->bound_type = resolve(parser);
    return ret;
}

pSyntaxNode TypeSpecification::stamp(Parser &)
{
    return make_node<TypeSpecification>(location, description);
}

pType TypeSpecification::bind(Parser &parser)
{
    auto ret = resolve(parser);
    if (ret == nullptr) {
        ret = TypeRegistry::undetermined;
    }
    return ret;
}

std::wostream &TypeSpecification::header(std::wostream &os)
{
    return os << to_string();
}

std::wstring TypeSpecification::to_string()
{
    return std::visit(overloads {
                          [](TypeNameNode const &d) -> std::wstring {
                              auto ret { d.name };
                              if (!d.arguments.empty()) {
                                  wchar_t sep = '<';
                                  for (auto const &arg : d.arguments) {
                                      ret += sep;
                                      sep = ',';
                                      ret += arg->to_string();
                                  }
                                  ret += '>';
                              }
                              return ret;
                          },
                          [](ReferenceDescriptionNode const &d) -> std::wstring {
                              return std::format(L"&{}", d.referencing->to_string());
                          },
                          [](SliceDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[]{}", d.slice_of->to_string());
                          },
                          [](ZeroTerminatedArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[0]{}", d.array_of->to_string());
                          },
                          [](ArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[{}]{}", d.size, d.array_of->to_string());
                          },
                          [](DynArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[*]{}", d.array_of->to_string());
                          },
                          [](OptionalDescriptionNode const &d) -> std::wstring {
                              return std::format(L"{}?", d.optional_of->to_string());
                          },
                          [](ErrorDescriptionNode const &d) -> std::wstring {
                              return std::format(L"{}/{}", d.success->to_string(), d.error->to_string());
                          },
                      },
        this->description);
}

pType TypeSpecification::resolve(Parser &parser)
{
    if (bound_type == nullptr) {
        bound_type = std::visit(
            [this, &parser](auto const &d) -> pType {
                return Arwen::resolve(d, parser);
            },
            this->description);
    }
    return bound_type;
}

Void::Void()
    : SyntaxNode(SyntaxNodeType::Void)
{
}

}
