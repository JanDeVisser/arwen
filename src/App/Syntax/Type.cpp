/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>
#include <string>

#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Arwen {

template<typename T>
pType resolve(T const &, ASTNode const &)
{
    fatal("No resolve method for `{}`", typeid(T).name());
}

template<>
pType resolve(TypeNameNode const &d, ASTNode const &n)
{
    auto t = n.repo->find_type(d.name);
    if (t == nullptr) {
        return nullptr;
    }
    while (t != nullptr && is<TypeAlias>(t)) {
        t = std::get<TypeAlias>(t->description).alias_of;
    }
    return t;
}

TypeSpecification &specification(ASTNode const &type)
{
    return get<TypeSpecification>(type);
}

template<>
pType resolve(ReferenceDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().referencing(specification(d.referencing).resolve(n));
}

pType resolve(SliceDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().slice_of(specification(d.slice_of).resolve(n));
}

pType resolve(ZeroTerminatedArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().zero_terminated_array_of(specification(d.array_of).resolve(n));
}

pType resolve(ArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().array_of(specification(d.array_of).resolve(n), d.size);
}

pType resolve(DynArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().dyn_array_of(specification(d.array_of).resolve(n));
}

pType resolve(OptionalDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().optional_of(specification(d.optional_of).resolve(n));
}

pType resolve(ErrorDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().error_of(specification(d.success).resolve(n), specification(d.error).resolve(n));
}

TypeSpecification::TypeSpecification(TypeSpecificationDescription description)
    : description(description)
{
}

ASTNode TypeSpecification::normalize(ASTNode const &n)
{
    description = std::visit(
        overloads {
            [this, &n](TypeNameNode const &d) -> TypeSpecificationDescription {
                ASTNodes args;
                for (auto const &arg : d.arguments) {
                    args.push_back(arg->normalize());
                }
                return TypeNameNode { d.name, args };
            },
            [this, &n](ReferenceDescriptionNode const &d) -> TypeSpecificationDescription {
                return ReferenceDescriptionNode { d.referencing->normalize() };
            },
            [this, &n](SliceDescriptionNode const &d) -> TypeSpecificationDescription {
                return SliceDescriptionNode { d.slice_of->normalize() };
            },
            [this, &n](ZeroTerminatedArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return ZeroTerminatedArrayDescriptionNode { d.array_of->normalize() };
            },
            [this, &n](ArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return ArrayDescriptionNode { d.array_of->normalize(), d.size };
            },
            [this, &n](DynArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return DynArrayDescriptionNode { d.array_of->normalize() };
            },
            [this, &n](OptionalDescriptionNode const &d) -> TypeSpecificationDescription {
                return OptionalDescriptionNode { d.optional_of->normalize() };
            },
            [this, &n](ErrorDescriptionNode const &d) -> TypeSpecificationDescription {
                return ErrorDescriptionNode {
                    d.success->normalize(),
                    d.error->normalize(),
                };
            },
        },
        this->description);
    // n->bind(); // Why?
    return n;
}

pType TypeSpecification::bind(ASTNode const &n)
{
    auto ret = resolve(n);
    if (ret == nullptr) {
        ret = TypeRegistry::undetermined;
    }
    return ret;
}

std::wostream &TypeSpecification::header(ASTNode const &, std::wostream &os)
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
                                      ret += get<TypeSpecification>(arg).to_string();
                                  }
                                  ret += '>';
                              }
                              return ret;
                          },
                          [](ReferenceDescriptionNode const &d) -> std::wstring {
                              return std::format(L"&{}", specification(d.referencing).to_string());
                          },
                          [](SliceDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[]{}", specification(d.slice_of).to_string());
                          },
                          [](ZeroTerminatedArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[0]{}", specification(d.array_of).to_string());
                          },
                          [](ArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[{}]{}", d.size, specification(d.array_of).to_string());
                          },
                          [](DynArrayDescriptionNode const &d) -> std::wstring {
                              return std::format(L"[*]{}", specification(d.array_of).to_string());
                          },
                          [](OptionalDescriptionNode const &d) -> std::wstring {
                              return std::format(L"{}?", specification(d.optional_of).to_string());
                          },
                          [](ErrorDescriptionNode const &d) -> std::wstring {
                              return std::format(L"{}/{}", specification(d.success).to_string(), specification(d.error).to_string());
                          },
                      },
        this->description);
}

pType TypeSpecification::resolve(ASTNode const &n)
{
    if (n->bound_type == nullptr || n->bound_type == TypeRegistry::undetermined) {
        n->bound_type = std::visit(
            [this, &n](auto const &d) -> pType {
                return Arwen::resolve(d, n);
            },
            this->description);
    }
    return n->bound_type;
}

Void::Void()
{
}

pType Void::bind(ASTNode const &n)
{
    return TypeRegistry::void_;
}

}
