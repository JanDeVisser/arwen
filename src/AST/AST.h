/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <AST/Operator.h>
#include <Grammar/Parser.h>
#include <Lexer/Lexer.h>
#include <Lib.h>

namespace Arwen {

#define ASTNodeKinds(S)     \
    S(ArrayType)            \
    S(AssignmentExpression) \
    S(BasicType)            \
    S(BinaryExpression)     \
    S(Block)                \
    S(BoolConstant)         \
    S(ConstantDeclaration)  \
    S(FloatConstant)        \
    S(Function)             \
    S(FunctionCall)         \
    S(FunctionDecl)         \
    S(Identifier)           \
    S(If)                   \
    S(IntConstant)          \
    S(Label)                \
    S(Loop)                 \
    S(Parameter)            \
    S(PointerType)          \
    S(Program)              \
    S(QString)              \
    S(Return)               \
    S(StartBlock)           \
    S(Subscript)            \
    S(UnaryExpression)      \
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
    ~ArrayType() = default;
};

struct AssignmentExpression {
    NodeReference  left;
    BinaryOperator op;
    NodeReference  right;
    ~AssignmentExpression() = default;
};

struct BinaryExpression {
    NodeReference  left;
    BinaryOperator op;
    NodeReference  right;
    ~BinaryExpression() = default;
};

struct Block {
    std::vector<NodeReference> statements;
    ~Block() = default;
};

struct ConstantDeclaration {
    std::string_view             name;
    std::optional<NodeReference> type {};
    NodeReference                initializer;
    ~ConstantDeclaration() = default;
};

struct Function {
    NodeReference declaration;
    NodeReference implementation;
    ~Function() = default;
};

struct FunctionCall {
    std::string_view           name;
    std::vector<NodeReference> arguments;
    ~FunctionCall() = default;
};

struct FunctionDecl {
    std::string_view             name;
    std::vector<NodeReference>   parameters;
    std::optional<NodeReference> return_type;
    ~FunctionDecl() = default;
};

struct If {
    NodeReference                condition;
    NodeReference                true_branch;
    std::optional<NodeReference> false_branch;
    ~If() = default;
};

struct Label {
    std::string_view label;
    ~Label() = default;
};

struct Loop {
    std::optional<NodeReference> label;
    NodeReference                body;
    ~Loop() = default;
};

struct Parameter {
    std::string_view name;
    NodeReference    type;
    ~Parameter() = default;
};

struct PointerType {
    NodeReference element_type;
    ~PointerType() = default;
};

struct Program {
    std::vector<NodeReference>                declarations;
    std::map<std::string_view, NodeReference> declarations_index;
    ~Program() = default;
};

struct Return {
    std::optional<NodeReference> expr;
    ~Return() = default;
};

struct Subscript {
    std::vector<NodeReference> subscripts;
    ~Subscript() = default;
};

struct UnaryExpression {
    UnaryOperator op;
    NodeReference operand;
    ~UnaryExpression() = default;
};

struct VariableDeclaration {
    std::string_view             name;
    std::optional<NodeReference> type {};
    std::optional<NodeReference> initializer {};
    ~VariableDeclaration() = default;
};

struct ASTNode {
    struct ArwenParser *parser = nullptr;
    size_t              ref { 0 };
    Location            location {};
    ASTNodeKind         kind;
    std::variant<
        bool,
        ssize_t,
        double,
        std::string_view,
        ArrayType,
        AssignmentExpression,
        BinaryExpression,
        Block,
        ConstantDeclaration,
        Function,
        FunctionCall,
        FunctionDecl,
        If,
        Label,
        Loop,
        Parameter,
        PointerType,
        Program,
        Return,
        Subscript,
        UnaryExpression,
        VariableDeclaration>
        impl;
    ~ASTNode() = default;

    ASTNode const &get_node(NodeReference node_ref) const;
};

struct ArwenParser {
    bool                       log { false };
    std::vector<Token>         token_stack {};
    std::vector<NodeReference> node_stack {};
    std::vector<ASTNode>       node_cache {};
    NodeReference              program { 0 };

    ArwenParser() = default;
    static ArwenParser          &get(Parser<ArwenParser> &parser);
    void                         startup();
    void                         cleanup() const;
    Token                        pop_token();
    void                         push_token(Token t);
    [[nodiscard]] ASTNode const &peek_node() const;
    [[nodiscard]] ASTNode const &peek_and_assert(ASTNodeKind kind) const;
    [[nodiscard]] ASTNodeKind    peek_kind() const;
    ASTNode                     &pop_node();

    ASTNode                             &pop_typed_node(ASTNodeKind kind);
    std::optional<ASTNode>               try_pop_typed_node(ASTNodeKind kind);
    NodeReference                        push_node(ASTNode const &n);
    NodeReference                        cache_node(ASTNode const &n);
    [[nodiscard]] ASTNode const         &get_node(NodeReference ref) const;
    [[nodiscard]] ASTNode               &get_node(NodeReference ref);
    [[nodiscard]] ASTNode const         &get_typed_node(ASTNodeKind kind, NodeReference ref) const;
    [[nodiscard]] std::optional<ASTNode> try_get_typed_node(ASTNodeKind kind, NodeReference ref) const;
    void                                 dump_node_stack(std::string_view caption) const;

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
    [[nodiscard]] bool check_node_type(NodeReference ref, auto const& kinds_container, Kinds &&...kinds) const
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
    FmtContext::iterator format(Arwen::ASTNodeKind const &node, FmtContext &ctx) const
    {
        std::ostringstream out;
        out << Arwen::to_string(node);
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
        case Arwen::ASTNodeKind::BasicType:
        case Arwen::ASTNodeKind::Identifier:
        case Arwen::ASTNodeKind::QString:
            out << std::get<std::string_view>(node.impl);
            break;
        case Arwen::ASTNodeKind::BinaryExpression: {
            auto &impl = std::get<Arwen::BinaryExpression>(node.impl);
            out << std::format("{} {} {}", node.get_node(impl.left), impl.op, node.get_node(impl.right));
        } break;
        case Arwen::ASTNodeKind::Block: {
            auto &impl = std::get<Arwen::Block>(node.impl);
            for (auto ref : impl.statements) {
                out << std::format("{}", node.get_node(ref));
            }
        } break;
        case Arwen::ASTNodeKind::BoolConstant:
            out << ios::boolalpha << std::get<bool>(node.impl);
            break;
        case Arwen::ASTNodeKind::ConstantDeclaration: {
            auto &impl = std::get<Arwen::ConstantDeclaration>(node.impl);
            out << std::format("const {}", impl.name);
            if (impl.type) {
                out << std::format(": {}", node.get_node(*impl.type));
            }
            out << std::format(" = {}", node.get_node(impl.initializer));
        } break;
        case Arwen::ASTNodeKind::FloatConstant:
            out << std::get<double>(node.impl);
            break;
        case Arwen::ASTNodeKind::Function: {
            auto &impl = std::get<Arwen::Function>(node.impl);
            out << std::format("{} {{\n{}}}\n", node.get_node(impl.declaration), node.get_node(impl.implementation));
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
        case Arwen::ASTNodeKind::FunctionDecl: {
            auto &impl = std::get<Arwen::FunctionDecl>(node.impl);
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
        case Arwen::ASTNodeKind::IntConstant:
            out << std::get<ssize_t>(node.impl);
            break;
        case Arwen::ASTNodeKind::Label: {
            auto &impl = std::get<Arwen::Label>(node.impl);
            out << std::format("#{}", impl.label);
        } break;
        case Arwen::ASTNodeKind::Loop: {
            auto &impl = std::get<Arwen::Loop>(node.impl);
            if (impl.label) {
                out << std::format("{} ", node.get_node(*impl.label));
            }
            out << std::format("loop {{\n{} }}", node.get_node(impl.body));
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
            for (auto decl : impl.declarations) {
                out << std::format("{}\n", node.get_node(decl));
            }
        } break;
        case Arwen::ASTNodeKind::Return: {
            auto &impl = std::get<Arwen::Return>(node.impl);
            out << "return";
            if (impl.expr) {
                out << std::format(" {}", node.get_node(impl.expr.value()));
            }
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
            //
        }
        return std::ranges::copy(std::move(out).str(), ctx.out()).out;
    }
};