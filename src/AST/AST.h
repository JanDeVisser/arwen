/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <algorithm>
#include <cstddef>
#include <format>
#include <ios>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <AST/Operator.h>
#include <Grammar/Parser.h>
#include <Lexer/Lexer.h>
#include <Lib.h>
#include <Logging.h>
#include <Type/Type.h>
#include <Unescape.h>

namespace Arwen {

#define ASTNodeKinds(S)       \
    S(ArrayType)              \
    S(AssignmentExpression)   \
    S(BasicTypeNode)          \
    S(BinaryExpression)       \
    S(Block)                  \
    S(BoolConstant)           \
    S(ConstantDeclaration)    \
    S(FloatConstant)          \
    S(ForeignFunction)        \
    S(Function)               \
    S(FunctionCall)           \
    S(FunctionImplementation) \
    S(Identifier)             \
    S(If)                     \
    S(IntConstant)            \
    S(Label)                  \
    S(Loop)                   \
    S(Member)                 \
    S(Module)                 \
    S(Parameter)              \
    S(PointerType)            \
    S(Program)                \
    S(Return)                 \
    S(StartBlock)             \
    S(StringConstant)         \
    S(Subscript)              \
    S(UnaryExpression)        \
    S(VariableDeclaration)

enum class ASTNodeKind {
#undef S
#define S(K) K,
    ASTNodeKinds(S)
#undef S
};

template<>
inline std::optional<ASTNodeKind> decode(std::string_view s, ...)
{
#undef S
#define S(S)            \
    if (iequals(s, #S)) \
        return ASTNodeKind::S;
    ASTNodeKinds(S)
#undef S
        return {};
}

template<>
inline constexpr std::string_view to_string(ASTNodeKind const &v)
{
    switch (v) {
#undef S
#define S(S)             \
    case ASTNodeKind::S: \
        return #S;
        ASTNodeKinds(S)
#undef S
    }
}

using NodeReference = size_t;

struct ArrayType {
    NodeReference                element_type;
    std::optional<NodeReference> size;
};

struct AssignmentExpression {
    NodeReference  left;
    BinaryOperator op;
    NodeReference  right;
};

struct BasicTypeNode {
    std::string_view name;
};

struct BinaryExpression {
    NodeReference  left;
    BinaryOperator op;
    NodeReference  right;
};

struct Block {
    std::optional<std::string_view> label;
    std::vector<NodeReference>      statements;
};

struct BoolConstant {
    bool value;
};

struct ConstantDeclaration {
    std::string_view             name;
    std::optional<NodeReference> type {};
    NodeReference                initializer;
};

struct FloatConstant {
    double value;
};

struct ForeignFunction {
    std::string_view foreign_name;
};

struct Function {
    std::string_view             name;
    std::vector<NodeReference>   parameters;
    std::optional<NodeReference> return_type;
    NodeReference                implementation;
};

struct FunctionCall {
    std::string_view           name;
    std::vector<NodeReference> arguments;
};

struct FunctionImplementation {
    NodeReference implementation;
};

struct Identifier {
    std::string_view text;
};

struct IntConstant {
    ssize_t value;
};

struct If {
    NodeReference                condition;
    NodeReference                true_branch;
    std::optional<NodeReference> false_branch;
};

struct Label {
    std::string_view label;
};

struct Loop {
    NodeReference body;
};

struct Member {
    std::string_view name;
};

struct Module {
    std::string_view           name;
    std::vector<NodeReference> names;
};

struct Parameter {
    std::string_view name;
    NodeReference    type;
};

struct PointerType {
    NodeReference element_type;
};

struct Program {
    std::vector<NodeReference> modules;
};

struct Return {
    std::optional<NodeReference> expression;
};

struct StartBlock {
};

class StringConstant {
public:
    StringConstant() = default;
    StringConstant(StringConstant const &) = default;
    StringConstant(std::string_view sv)
    {
        assert(sv.length() > 1 && sv[0] == sv[sv.length() - 1]);
        quote = sv[0];
        value = sv.substr(1, sv.size() - 2);
    }

    char             quote = 0;
    std::string_view value = {};
};

struct Subscript {
    std::vector<NodeReference> subscripts;
};

struct UnaryExpression {
    UnaryOperator op;
    NodeReference operand;
};

struct VariableDeclaration {
    std::string_view             name;
    std::optional<NodeReference> type {};
    std::optional<NodeReference> initializer {};
};

using ASTNodeImpl = std::variant<
    ArrayType,
    AssignmentExpression,
    BasicTypeNode,
    BinaryExpression,
    Block,
    BoolConstant,
    ConstantDeclaration,
    FloatConstant,
    ForeignFunction,
    Function,
    FunctionCall,
    FunctionImplementation,
    Identifier,
    If,
    IntConstant,
    Label,
    Loop,
    Member,
    Module,
    Parameter,
    PointerType,
    Program,
    Return,
    StartBlock,
    StringConstant,
    Subscript,
    UnaryExpression,
    VariableDeclaration>;

struct ASTNode {
    struct ArwenParser *parser = nullptr;
    size_t              ref { 0 };
    Location            location {};
    ASTNodeKind         kind;
    ASTNodeImpl         impl;

    ASTNode(ASTNodeKind kind, ASTNodeImpl impl);
    [[nodiscard]] ASTNode const &get_node(NodeReference node_ref) const;
};

struct ArwenParser {
    bool                         log { false };
    std::vector<Token>           token_stack {};
    std::vector<NodeReference>   node_stack {};
    std::vector<ASTNode>         node_cache {};
    NodeReference                program { 0 };
    std::optional<NodeReference> module { 0 };

    ArwenParser();
    static ArwenParser                  &get(Parser<ArwenParser> &parser);
    void                                 startup(std::string_view buffer);
    void                                 cleanup() const;
    Token                                pop_token();
    void                                 push_token(Token t);
    [[nodiscard]] ASTNode const         &peek_node() const;
    [[nodiscard]] ASTNode const         &peek_and_assert(ASTNodeKind kind) const;
    [[nodiscard]] ASTNodeKind            peek_kind() const;
    ASTNode                             &pop_node();
    ASTNode                             &pop_typed_node(ASTNodeKind kind);
    std::optional<ASTNode>               try_pop_typed_node(ASTNodeKind kind);
    ASTNode const                       &make_node(Location location, ASTNodeKind kind, ASTNodeImpl const &impl);
    ASTNode const                       &push_node(Location location, ASTNodeKind kind, ASTNodeImpl const &impl);
    NodeReference                        cache_node(ASTNode const &n);
    [[nodiscard]] ASTNode const         &get_node(NodeReference ref) const;
    [[nodiscard]] ASTNode               &get_node(NodeReference ref);
    [[nodiscard]] ASTNode const         &get_typed_node(NodeReference ref, ASTNodeKind kind) const;
    [[nodiscard]] ASTNode               &get_typed_node(NodeReference ref, ASTNodeKind kind);
    [[nodiscard]] std::optional<ASTNode> try_get_typed_node(NodeReference ref, ASTNodeKind kind) const;
    void                                 dump_node_stack(std::string_view caption) const;
    void                                 dump();

    template<typename... Kinds>
    [[nodiscard]] ASTNode const &get_one_of(NodeReference ref, Kinds &&...kinds) const
    {
        auto const &ret = get_node(ref);
        if (check_node_type(ref, kinds...)) {
            return ret;
        }
    }

    template<typename... Kinds>
    ASTNode const &pop_one_of(Kinds &&...kinds)
    {
        if (auto ref_maybe = try_pop_one_of(kinds...); ref_maybe) {
            return get_node(*ref_maybe);
        }
        fatal("Got '{}'", get_node(node_stack.back()).kind);
    }

    template<typename... Kinds>
    std::optional<NodeReference> try_pop_one_of(Kinds &&...kinds)
    {
        dump_node_stack("try_pop_one_of");
        if (node_stack.empty()) {
            fatal("node_stack underflow");
        }
        auto ret = node_stack.back();
        if (check_node_type(ret, kinds...)) {
            node_stack.pop_back();
            return ret;
        }
        return {};
    }

    template<typename... Kinds>
    [[nodiscard]] bool check_node_type(NodeReference ref, ASTNodeKind kind, Kinds &&...kinds) const
    {
        auto const &node = get_node(ref);
        bool        ret = node.kind == kind;
        if constexpr (sizeof...(Kinds) > 0) {
            ret |= check_node_type(ref, kinds...);
        }
        return ret;
    }

    template<typename... Kinds>
    [[nodiscard]] bool check_node_type(NodeReference ref, auto const &kinds_container, Kinds &&...kinds) const
    {
        auto const &node = get_node(ref);
        bool        ret = std::any_of(kinds_container.begin(), kinds_container.end(), [&node](ASTNodeKind k) { return node.kind == k; });
        if constexpr (sizeof...(Kinds) > 0) {
            ret |= check_node_type(ref, kinds...);
        }
        return ret;
    }

private:
};

}

template<>
struct std::formatter<Arwen::ASTNodeKind, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::ASTNodeKind const &k, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(k);
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};

template<>
struct std::formatter<Arwen::ASTNode, char> : public Arwen::SimpleFormatParser {
    template<class FmtContext>
    FmtContext::iterator format(Arwen::ASTNode const &node, FmtContext &ctx) const
    {
        std::ostringstream out;
        switch (node.kind) {
        case Arwen::ASTNodeKind::ArrayType: {
            auto &impl = std::get<Arwen::ArrayType>(node.impl);
            if (impl.size) {
                out << std::format("[{}]{}", node.parser->get_node(*impl.size), node.get_node(impl.element_type));
            } else {
                out << std::format("[]{}", node.parser->get_node(impl.element_type));
            }
        } break;
        case Arwen::ASTNodeKind::AssignmentExpression: {
            auto &impl = std::get<Arwen::AssignmentExpression>(node.impl);
            out << std::format("{} {} {}", node.get_node(impl.left), impl.op, node.get_node(impl.right));
        } break;
        case Arwen::ASTNodeKind::BasicTypeNode: {
            auto &impl = std::get<Arwen::BasicTypeNode>(node.impl);
            out << impl.name;
        } break;
        case Arwen::ASTNodeKind::BinaryExpression: {
            auto &impl = std::get<Arwen::BinaryExpression>(node.impl);
            out << std::format("{} {} {}", node.get_node(impl.left), impl.op, node.get_node(impl.right));
        } break;
        case Arwen::ASTNodeKind::Block: {
            auto &impl = std::get<Arwen::Block>(node.impl);
            if (impl.label) {
                out << std::format("{}: {{\n", *impl.label);
            }
            for (auto ref : impl.statements) {
                out << std::format("{}\n", node.get_node(ref));
            }
            out << "}\n";
        } break;
        case Arwen::ASTNodeKind::BoolConstant: {
            auto &impl = std::get<Arwen::BoolConstant>(node.impl);
            out << ios::boolalpha << impl.value;
        } break;
        case Arwen::ASTNodeKind::ConstantDeclaration: {
            auto &impl = std::get<Arwen::ConstantDeclaration>(node.impl);
            out << std::format("const {}", impl.name);
            if (impl.type) {
                out << std::format(": {}", node.get_node(*impl.type));
            }
            out << std::format(" = {}", node.get_node(impl.initializer));
        } break;
        case Arwen::ASTNodeKind::FloatConstant: {
            auto &impl = std::get<Arwen::FloatConstant>(node.impl);
            out << impl.value;
        } break;
        case Arwen::ASTNodeKind::ForeignFunction: {
            auto &impl = std::get<Arwen::ForeignFunction>(node.impl);
            out << std::format(" -> \"{}\"\n", impl.foreign_name);
        } break;
        case Arwen::ASTNodeKind::Function: {
            auto &impl = std::get<Arwen::Function>(node.impl);
            out << std::format("func {}(", impl.name);
            bool first { true };
            for (auto &param : impl.parameters) {
                if (!first) {
                    out << ", ";
                }
                out << std::format("{}", node.get_node(param));
                first = false;
            }
            out << ")";
            if (impl.return_type) {
                out << std::format(" {}", node.get_node(*impl.return_type));
            }
            out << std::format("\n{}", node.get_node(impl.implementation));
        } break;
        case Arwen::ASTNodeKind::FunctionCall: {
            auto &impl = std::get<Arwen::FunctionCall>(node.impl);
            out << std::format("{}(", impl.name);
            bool first { true };
            for (auto &arg : impl.arguments) {
                if (!first) {
                    out << ", ";
                }
                out << std::format("{}", node.get_node(arg));
                first = false;
            }
            out << ")";
        } break;
        case Arwen::ASTNodeKind::FunctionImplementation: {
            auto &impl = std::get<Arwen::FunctionImplementation>(node.impl);
            out << std::format("{}", node.get_node(impl.implementation));
        } break;
        case Arwen::ASTNodeKind::Identifier: {
            auto &impl = std::get<Arwen::Identifier>(node.impl);
            out << impl.text;
        } break;
        case Arwen::ASTNodeKind::If: {
            auto &impl = std::get<Arwen::If>(node.impl);
            out << std::format("if {} {{\n{}", node.get_node(impl.condition), node.get_node(impl.true_branch));
            if (impl.false_branch) {
                out << "}} else {{\n";
                out << std::format("{}", node.get_node(*impl.false_branch));
            }
            out << "}";
        } break;
        case Arwen::ASTNodeKind::IntConstant: {
            auto &impl = std::get<Arwen::IntConstant>(node.impl);
            out << impl.value;
        } break;
        // case Arwen::ASTNodeKind::Intrinsic: {
        //     auto &impl = std::get<Arwen::ForeignFunction>(node.impl);
        //     out << std::format(" -> {}\n", impl.foreign_name);
        // } break;
        case Arwen::ASTNodeKind::Label: {
            auto &impl = std::get<Arwen::Label>(node.impl);
            out << std::format("#{}", impl.label);
        } break;
        case Arwen::ASTNodeKind::Loop: {
            auto &impl = std::get<Arwen::Loop>(node.impl);
            out << std::format("loop {{\n{} }}", node.get_node(impl.body));
        } break;
        case Arwen::ASTNodeKind::Member: {
            auto &impl = std::get<Arwen::Member>(node.impl);
            out << std::format("{}", impl.name);
        } break;
        case Arwen::ASTNodeKind::Module: {
            auto &impl = std::get<Arwen::Module>(node.impl);
            for (auto name : impl.names) {
                out << std::format("{}\n", node.get_node(name));
            }
        } break;
        case Arwen::ASTNodeKind::Parameter: {
            auto &impl = std::get<Arwen::Parameter>(node.impl);
            out << std::format("{}: {} ", impl.name, node.get_node(impl.type));
        } break;
        case Arwen::ASTNodeKind::PointerType: {
            auto &impl = std::get<Arwen::PointerType>(node.impl);
            out << std::format("*{}", node.get_node(impl.element_type));
        } break;
        case Arwen::ASTNodeKind::Program: {
            auto &impl = std::get<Arwen::Program>(node.impl);
            for (auto decl : impl.modules) {
                out << std::format("{}\n", node.get_node(decl));
            }
        } break;
        case Arwen::ASTNodeKind::Return: {
            auto &impl = std::get<Arwen::Return>(node.impl);
            out << "return";
            if (impl.expression) {
                out << std::format(" {}", node.get_node(*impl.expression));
            }
        } break;
        case Arwen::ASTNodeKind::StringConstant: {
            auto &impl = std::get<Arwen::StringConstant>(node.impl);
            out << impl.quote << impl.value << impl.quote;
        } break;
        case Arwen::ASTNodeKind::Subscript: {
            auto &impl = std::get<Arwen::Subscript>(node.impl);
            out << "[";
            bool first { true };
            for (auto &sub : impl.subscripts) {
                if (!first) {
                    out << ", ";
                }
                out << std::format("{}", node.get_node(sub));
                first = false;
            }
            out << "]";
        } break;
        case Arwen::ASTNodeKind::UnaryExpression: {
            auto &impl = std::get<Arwen::UnaryExpression>(node.impl);
            out << std::format("{}{}", impl.op, node.get_node(impl.operand));
        } break;
        case Arwen::ASTNodeKind::VariableDeclaration: {
            auto &impl = std::get<Arwen::VariableDeclaration>(node.impl);
            out << std::format("var {}", impl.name);
            if (impl.type) {
                out << std::format(": {}", node.get_node(*impl.type));
            }
            if (impl.initializer) {
                out << std::format(" = {}", node.get_node(*impl.initializer));
            }
        } break;
        default:
            out << Arwen::to_string(node.kind);
            break;
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};
