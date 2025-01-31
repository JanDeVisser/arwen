/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <format>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <typeindex>
#include <utility>
#include <variant>
#include <vector>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Lexer/Lexer.h>
#include <Type/Type.h>
#include <Type/Value.h>

#include <Lib.h>
#include <Logging.h>
#include <Result.h>
#include <ScopeGuard.h>
#include <SimpleFormat.h>
#include <TaggedUnion.h>
#include <Unescape.h>

namespace Arwen {

using BoundNodeReference = NodeReference;
using BoundNodeReferences = std::vector<BoundNodeReference>;

struct TypeAlternatives {
    TypeReference              preferred;
    std::vector<TypeReference> alternatives;
};

struct BindError {
    BoundNodeReference node;
    int                pass;
    std::string        message;
};

struct BoundArrayType {
    TypeReference      element_type;
    BoundNodeReference size;
};

struct BoundAssignmentExpression {
    BoundNodeReference left;
    BinaryOperator     op { BinaryOperator::None };
    BoundNodeReference right;
};

struct BoundBinaryExpression {
    BoundNodeReference left;
    BinaryOperator     op { BinaryOperator::None };
    BoundNodeReference right;
};

struct BoundBlock {
    std::optional<std::string_view> label;
    BoundNodeReferences             statements;
};

struct BoundBreak {
    BoundNodeReference                block;
    bool                              block_is_loop { false };
    std::optional<BoundNodeReference> expression {};
};

struct BoundCoercion {
    BoundNodeReference expression;
};

struct BoundConstant {
    Value value;
};

struct BoundConstantDeclaration {
    std::string_view                  name;
    std::optional<BoundNodeReference> type;
    BoundNodeReference                initializer;
};

struct BoundContinue {
    BoundNodeReference block;
};

struct BoundFor {
    BoundNodeReference variable_decl;
    BoundNodeReference range;
    BoundNodeReference body;
};

struct BoundForeignFunction {
    std::string_view foreign_name;
};

struct BoundFunction {
    std::string_view                  name;
    BoundNodeReferences               parameters;
    std::optional<BoundNodeReference> return_type;
    BoundNodeReference                implementation;
};

struct BoundFunctionCall {
    std::string_view                  name;
    bool                              discard_result { false };
    std::optional<BoundNodeReference> function = {};
    BoundNodeReferences               arguments;
};

struct BoundFunctionImplementation {
    std::string_view   name;
    BoundNodeReference implementation;
};

struct BoundIdentifier {
    std::string_view                  name;
    std::optional<BoundNodeReference> declaration = {};
};

struct BoundIf {
    BoundNodeReference                condition;
    BoundNodeReference                true_branch;
    std::optional<BoundNodeReference> false_branch;
};

struct BoundIntrinsic {
    std::string_view name;
};

struct BoundLoop {
    BoundNodeReference body;
};

struct BoundMember {
    std::string_view name;
};

struct BoundModule {
    std::string_view    name;
    BoundNodeReferences names;
};

struct BoundParameter {
    std::string_view   name;
    BoundNodeReference type;
};

struct BoundPointerType {
    BoundNodeReference element_type;
};

struct BoundProgram {
    BoundNodeReferences modules;
};

struct BoundRange {
    BoundNodeReference begin;
    BoundNodeReference end;
};

struct BoundReturn {
    std::optional<BoundNodeReference> expression;
};

struct BoundSliceType {
    TypeReference element_type;
};

struct BoundSubscript {
    BoundNodeReferences subscripts;
};

struct BoundUnaryExpression {
    UnaryOperator      op { UnaryOperator::None };
    BoundNodeReference operand;
};

struct BoundVariableDeclaration {
    std::string_view                  name;
    std::optional<BoundNodeReference> type;
    std::optional<BoundNodeReference> initializer;
};

struct BoundWhile {
    BoundNodeReference condition;
    BoundNodeReference body;
};

#define BoundNodeImpls(S)          \
    S(BoundArrayType)              \
    S(BoundAssignmentExpression)   \
    S(BasicTypeNode)               \
    S(BoundBinaryExpression)       \
    S(BoundBlock)                  \
    S(BoundBreak)                  \
    S(BoundCoercion)               \
    S(BoolConstant)                \
    S(BoundConstant)               \
    S(BoundConstantDeclaration)    \
    S(BoundContinue)               \
    S(FloatConstant)               \
    S(BoundFor)                    \
    S(BoundForeignFunction)        \
    S(BoundFunction)               \
    S(BoundFunctionCall)           \
    S(BoundFunctionImplementation) \
    S(BoundIdentifier)             \
    S(BoundIf)                     \
    S(BoundIntrinsic)              \
    S(IntConstant)                 \
    S(BoundLoop)                   \
    S(BoundMember)                 \
    S(BoundModule)                 \
    S(Nullptr)                     \
    S(BoundParameter)              \
    S(BoundPointerType)            \
    S(BoundProgram)                \
    S(BoundRange)                  \
    S(BoundReturn)                 \
    S(BoundSliceType)              \
    S(StringConstant)              \
    S(BoundSubscript)              \
    S(BoundUnaryExpression)        \
    S(BoundVariableDeclaration)    \
    S(BoundWhile)

#undef S
#define S(T) T,
enum class BoundNodeType {
    BoundNodeImpls(S)
};

using BoundNodeImpl = std::variant<
    BoundNodeImpls(S)
#undef S
        BindError>;

struct BoundNode {
    std::optional<TypeReference>                  type {};
    BoundNodeReference                            ref { 0 };
    BoundNodeReference                            parent { 0 };
    NodeReference                                 ast_ref;
    Location                                      location;
    std::map<std::string_view, NodeReference>     names;
    BoundNodeImpl                                 impl;
    static std::map<std::type_index, std::string> type_names;

    [[nodiscard]] std::string_view type_name() const;

    template<typename Impl>
    static BoundNode make(NodeReference parent, NodeReference ast_ref, Location location)
    {
        auto ret = BoundNode { parent, ast_ref, location };
        ret.impl = { Impl {} };
        return ret;
    };

    template<typename Impl>
    Impl &implementation()
    {
        return std::get<Impl>(impl);
    };

private:
    BoundNode(BoundNodeReference parent, NodeReference ast_ref, Location location)
        : parent(parent)
        , ast_ref(ast_ref)
        , location(location)
    {
    }
};

struct Binder {
    TypeRegistry                &registry;
    std::vector<ASTNode> const  &ast;
    std::vector<BoundNode>       bound_nodes;
    BoundNodeReferences          namespaces;
    int                          pass { 0 };
    BoundNodeReferences          errors;
    BoundNodeReference           entrypoint;
    bool                         log { false };
    std::set<BoundNodeReference> visited;
    std::set<BoundNodeReference> still_unbound;

    explicit Binder(std::vector<ASTNode> const &ast)
        : registry(TypeRegistry::the())
        , ast(ast)
    {
    }

    BoundNode &operator[](BoundNodeReference ref)
    {
        return bound_nodes[ref];
    }

    template<typename Impl>
    Impl &impl(BoundNodeReference ref)
    {
        return bound_nodes[ref].implementation<Impl>();
    };

    TypeAlternatives                 alternatives(NodeReference ref, TypeReference hint);
    BoundNodeReference               accept(NodeReference ref, TypeReference type);
    BoundNodeReference               bind_node(NodeReference ast_ref, BoundNodeReference parent = 0);
    BoundNodeReference               rebind_node(BoundNodeReference ref);
    Result<BoundNodeReference, bool> bind(NodeReference ast_entrypoint);
    BoundNodeType                    type_of(NodeReference ref) const;
    void                             push_namespace(BoundNodeReference ref);
    void                             set_name(std::string_view name, BoundNodeReference ref);
    void                             pop_namespace();
    std::optional<NodeReference>     resolve(std::string_view name);
    void                             to_string(std::ostream &out, BoundNodeReference ref);
    void                             dump(std::ostream &out, BoundNodeReference ref, std::string_view caption, int indent = 0);
    void                             dump(std::ostream &out, std::optional<BoundNodeReference> ref, std::string_view caption, int indent);
    void                             dump(std::ostream &out, BoundNodeReferences refs, std::string_view caption, int indent);
    void                             list(std::ostream &out = std::cout);
};

#define I(Cls, Ref) (binder.impl<Cls>(Ref))
#define IMPL I(STRUCT, ref) //(binder.impl<STRUCT>(ref))

template<class Impl>
inline BoundNodeReference add_node(Binder &binder, NodeReference ref, Location location, BoundNodeReference parent)
{
    binder.bound_nodes.push_back(BoundNode::make<Impl>(parent, ref, location));
    binder.bound_nodes.back().ref = binder.bound_nodes.size() - 1;
    return binder.bound_nodes.back().ref;
}

template<typename... Args>
inline BoundNodeReference add_error(Binder &binder, BoundNodeReference ref, std::format_string<Args...> message, Args &&...args)
{
    BoundNodeReference err = add_node<BindError>(binder, binder.bound_nodes[ref].ast_ref, binder.bound_nodes[ref].location, ref);
    auto              &impl = std::get<BindError>(binder.bound_nodes[err].impl);
    impl.node = ref;
    impl.pass = binder.pass;
    impl.message = std::format(message, std::forward<Args>(args)...);
    binder.errors.emplace_back(err);
    return err;
}

template<typename AstImpl>
inline BoundNodeReference bind([[maybe_unused]] Binder &, [[maybe_unused]] NodeReference, BoundNodeReference)
{
    std::cerr << "No binder for " << typeid(AstImpl).name() << "\n";
    UNREACHABLE();
}

template<typename T>
inline BoundNodeReference rebind([[maybe_unused]] Binder &, BoundNodeReference ref)
{
    return ref;
}

template<typename BoundImpl>
inline TypeAlternatives alternatives(Binder &binder, BoundNodeReference ref, TypeReference hint)
{
    assert(binder[ref].type.has_value());
    TypeAlternatives ret = { *binder[ref].type, {} };
    auto const      &type = TypeRegistry::the()[*binder[ref].type].decay();
    auto const      &hinted = TypeRegistry::the()[hint].decay();
    if (type.is_numeric()) {
        ret.alternatives.push_back(DoubleType);
        ret.alternatives.push_back(FloatType);
        if (type.is_integer()) {
            if (type.size() < 8) {
                if (type.is_unsigned()) {
                    for (TypeReference t = type.ref + 1; binder.registry[t].is_integer(); ++t) {
                        ret.alternatives.push_back(t);
                    }
                } else {
                    for (TypeReference t = type.ref + 2; binder.registry[t].is_integer(); t = t + 2) {
                        ret.alternatives.push_back(t);
                    }
                }
            } else if (type.ref == I64Type) {
                ret.alternatives.push_back(U64Type);
            } else if (type.ref == U64Type) {
                ret.alternatives.push_back(I64Type);
            }
        }
    }
    if (type.is_raw_pointer()) {
        ret.alternatives.push_back(PtrType);
        if (hinted.typespec.tag() == TypeKind::Pointer) {
            ret.alternatives.push_back(hint);
        }
    }
    if (type.typespec.tag() == TypeKind::Pointer) {
        ret.alternatives.push_back(PtrType);
    }
    if (type.typespec.tag() == TypeKind::Array) {
        ret.alternatives.push_back(PtrType);
        auto slice_type = binder.registry.resolve_slice(type.typespec.get<TypeKind::Array>().element_type).or_else([]() {
            fatal("Could not resolve slice type");
            return std::optional { VoidType };
        });
        ret.alternatives.push_back(slice_type.value());
        auto ptr_type = binder.registry.resolve_pointer(type.typespec.get<TypeKind::Array>().element_type).or_else([]() {
            fatal("Could not resolve pointer type");
            return std::optional { VoidType };
        });
        ret.alternatives.push_back(ptr_type.value());
    }
    if (type.typespec.tag() == TypeKind::Slice) {
        ret.alternatives.push_back(PtrType);
        auto ptr_type = binder.registry.resolve_pointer(type.typespec.get<TypeKind::Slice>().element_type).or_else([]() {
            fatal("Could not resolve pointer type");
            return std::optional { VoidType };
        });
        ret.alternatives.push_back(ptr_type.value());
    }
    return ret;
}

template<typename Impl>
inline BoundNodeReference accept(Binder &binder, BoundNodeReference ref, TypeReference type)
{
    auto &node = binder[ref];
    assert(node.type.has_value());
    if (node.type == type) {
        return ref;
    }
    auto        current = *node.type;
    auto const &current_type = binder.registry[current];
    auto const &desired = binder.registry[type];
    if (current_type.is_raw_pointer() && desired.is_raw_pointer()) {
        node.type = type;
        return ref;
    }
    if (desired.typespec.tag() == TypeKind::Pointer) {
        if (current == PtrType) {
            node.type = type;
        } else {
            switch (current_type.typespec.tag()) {
                case TypeKind::Array:
                case TypeKind::Slice: {
                    auto ptr_call = add_node<BoundFunctionCall>(binder, node.ast_ref, node.location, node.parent);
                    I(BoundFunctionCall, ptr_call).name = "ptr";
                    I(BoundFunctionCall, ptr_call).arguments.push_back(ref);
                    binder[ptr_call].type = type;
                    binder[ref].parent = ptr_call;
                    return ptr_call;
                } break;
                default:
                    UNREACHABLE();
            }
        }
        return ref;
    }
    if (desired.typespec.tag() == TypeKind::Slice && current_type.typespec.tag() == TypeKind::Array) {
        node.type = type;
        return ref;
    }
    ref = add_node<BoundCoercion>(binder, node.ast_ref, node.location, node.parent);
    binder[ref].type = type;
    binder.impl<BoundCoercion>(ref).expression = node.ref;
    node.parent = ref;
    return ref;
}

template<typename T>
void dump(std::ostream &out, Binder &, T const &, int)
{
}

template<typename T>
void to_string(std::ostream &out, Binder &, T const &)
{
}

}

template<>
struct std::formatter<Arwen::TypeAlternatives, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::TypeAlternatives const &v, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << "preferred: " << Arwen::TypeRegistry::the()[v.preferred].name << " alternatives: ";
        for (auto alternative : v.alternatives) {
            out << ", " << Arwen::TypeRegistry::the()[alternative].name;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
