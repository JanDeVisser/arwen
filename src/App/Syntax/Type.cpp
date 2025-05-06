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
                                [this, &parser](SliceDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return SliceDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.slice_of->normalize(parser)) };
                                },
                                [this, &parser](ZeroTerminatedArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ZeroTerminatedArrayDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.array_of->normalize(parser)) };
                                },
                                [this, &parser](ArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                                    return ArrayDescriptionNode { std::dynamic_pointer_cast<TypeSpecification>(d.array_of->normalize(parser)) };
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

std::wostream& TypeSpecification::header(std::wostream &os)
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
                          [](SliceDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[]{}", d.slice_of->to_string());
                          },
                          [](ZeroTerminatedArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[0]{}", d.array_of->to_string());
                          },
                          [](ArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[{}]{}", d.size, d.array_of->to_string());
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
    return std::visit(overloads {
                          [this, &parser](TypeNameNode d) -> pType {
                              auto t = parser.find_type(d.name);
                              if (t == nullptr) {
                                  return nullptr;
                              }
                              while (t != nullptr && t->is<TypeAlias>()) {
                                  t = std::get<TypeAlias>(t->description).alias_of;
                              }
                              return t;
                          },
                          [this, &parser](SliceDescriptionNode d) -> pType {
                              return TypeRegistry::the().slice_of(d.slice_of->resolve(parser));
                          },
                          [this, &parser](ZeroTerminatedArrayDescriptionNode d) -> pType {
                              return TypeRegistry::the().zero_terminated_array_of(d.array_of->resolve(parser));
                          },
                          [this, &parser](ArrayDescriptionNode d) -> pType {
                              return TypeRegistry::the().array_of(d.array_of->resolve(parser), d.size);
                          },
                          [this, &parser](OptionalDescriptionNode d) -> pType {
                              return TypeRegistry::the().optional_of(d.optional_of->resolve(parser));
                          },
                          [this, &parser](ErrorDescriptionNode d) -> pType {
                              return TypeRegistry::the().error_of(d.success->resolve(parser), d.error->resolve(parser));
                          },
                      },
        this->description);
}

Void::Void()
    : ConstantExpression(SyntaxNodeType::Void)
{
}

pType Void::bind(Parser &parser)
{
    return TypeRegistry::void_;
}

}
