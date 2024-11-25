/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <algorithm>
#include <array>
#include <cstdio>
#include <optional>
#include <print>
#include <string_view>
#include <vector>

#include <AST/AST.h>
#include <AST/Operator.h>
#include <Grammar/Parser.h>
#include <Lexer/Lexer.h>
#include <Type/Value.h>

#include <Lib.h>
#include <Logging.h>

namespace Arwen {

std::vector<ASTNodeKind> expressionKinds = {
    ASTNodeKind::AssignmentExpression,
    ASTNodeKind::BinaryExpression,
    ASTNodeKind::BoolConstant,
    ASTNodeKind::FloatConstant,
    ASTNodeKind::FunctionCall,
    ASTNodeKind::Identifier,
    ASTNodeKind::IntConstant,
    ASTNodeKind::Member,
    ASTNodeKind::StringConstant,
    ASTNodeKind::UnaryExpression,
};

std::array<ASTNodeKind, 3> typeKinds = {
    ASTNodeKind::BasicTypeNode,
    ASTNodeKind::ArrayType,
    ASTNodeKind::PointerType,
};

void list_cache(ArwenParser const &parser)
{
    for (auto const &node : parser.node_cache) {
        std::println("{:4}. {:20}{:20}", node.ref, to_string(node.kind), to_string(static_cast<ASTNodeKind>(node.impl.index())));
    }
}

ASTNode::ASTNode(ASTNodeKind kind, ASTNodeImpl impl)
    : kind(kind)
    , impl(std::move(impl))
{
}

ASTNode const &ASTNode::get_node(NodeReference node_ref) const
{
    return parser->get_node(node_ref);
}

ArwenParser &ArwenParser::get(Parser<ArwenParser> &parser)
{
    return parser.impl;
}

ArwenParser::ArwenParser()
{
    push_node({}, ASTNodeKind::Program, Program {});
}

void ArwenParser::startup(std::string_view buffer)
{
    auto mod = buffer;
    if (mod.ends_with(".arw") || mod.ends_with(".ARW")) {
        mod = mod.substr(0, mod.length() - 4);
    }
    auto &module_node = push_node({ buffer }, ASTNodeKind::Module, Module { mod });
    module = module_node.ref;
    auto &program_node = get_typed_node(program, ASTNodeKind::Program);
    auto &program_impl = std::get<Program>(program_node.impl);
    program_impl.modules.push_back(*module);
}

void ArwenParser::cleanup() const
{
}

Token ArwenParser::pop_token()
{
    if (token_stack.empty()) {
        fatal("token_stack underflow");
    }
    Token ret = token_stack.back();
    token_stack.pop_back();
    return ret;
}

void ArwenParser::push_token(Token t)
{
    token_stack.push_back(t);
}

ASTNode const &ArwenParser::peek_node() const
{
    dump_node_stack("peek_node");
    if (node_stack.empty()) {
        fatal("node_stack underflow");
    }
    return get_node(node_stack.back());
}

ASTNode &ArwenParser::pop_node()
{
    if (node_stack.empty()) {
        fatal("node_stack underflow");
    }
    auto ret = node_stack.back();
    node_stack.pop_back();
    dump_node_stack("pop_node");
    // list_cache(*this);
    return get_node(ret);
}

ASTNode &ArwenParser::pop_typed_node(ASTNodeKind kind)
{
    auto &ret = pop_node();
    if (ret.kind == kind) {
        return ret;
    }
    fatal("pop_typed_node({}): got '{}'", kind, ret.kind);
}

std::optional<ASTNode> ArwenParser::try_pop_typed_node(ASTNodeKind kind)
{
    auto &top = peek_node();
    if (top.kind == kind) {
        return pop_node();
    }
    return {};
}

ASTNode const &ArwenParser::make_node(Location location, ASTNodeKind kind, ASTNodeImpl const &impl)
{
    ASTNode n { kind, impl };
    n.location = location;
    auto ref = cache_node(n);
    return node_cache.at(ref);
}

ASTNode const &ArwenParser::push_node(Location location, ASTNodeKind kind, ASTNodeImpl const &impl)
{
    auto const &ret = make_node(location, kind, impl);
    node_stack.push_back(ret.ref);
    dump_node_stack("push_node");
    // list_cache(*this);
    return ret;
}

ASTNode const &ArwenParser::peek_and_assert(ASTNodeKind kind) const
{
    dump_node_stack("peek_and_assert");
    auto &ret = peek_node();
    if (ret.kind != kind) {
        fatal("Top of stack has invalid node type '{}', was expecting '{}'", ret.kind, kind);
    }
    return ret;
}

ASTNodeKind ArwenParser::peek_kind() const
{
    dump_node_stack("peek_kind");
    auto &ret = peek_node();
    return ret.kind;
}

NodeReference ArwenParser::cache_node(ASTNode const &n)
{
    ASTNode n_ { n };
    n_.ref = node_cache.size();
    n_.parser = this;
    node_cache.push_back(n_);
    assert(node_cache.size() == n_.ref + 1);
    return n_.ref;
}

ASTNode const &ArwenParser::get_node(NodeReference ref) const
{
    if (ref >= node_cache.size()) {
        fatal("Node reference {} out of range", ref);
    }
    return node_cache[ref];
}

ASTNode &ArwenParser::get_node(NodeReference ref)
{
    if (ref >= node_cache.size()) {
        fatal("Node reference {} out of range", ref);
    }
    return node_cache[ref];
}

ASTNode const &ArwenParser::get_typed_node(NodeReference ref, ASTNodeKind kind) const
{
    dump_node_stack("get_typed_node (const)");
    auto &ret = get_node(ref);
    if (ret.kind == kind) {
        return ret;
    }
    fatal("pop_typed_node({}): got '{}'", kind, ret.kind);
}

ASTNode &ArwenParser::get_typed_node(NodeReference ref, ASTNodeKind kind)
{
    dump_node_stack("get_typed_node");
    auto &ret = get_node(ref);
    if (ret.kind == kind) {
        return ret;
    }
    fatal("pop_typed_node({}): got '{}'", kind, ret.kind);
}

std::optional<ASTNode> ArwenParser::try_get_typed_node(NodeReference ref, ASTNodeKind kind) const
{
    dump_node_stack("try_get_typed_node");
    auto &ret = get_node(ref);
    if (ret.kind == kind) {
        return ret;
    }
    return {};
}

void ArwenParser::dump_node_stack(std::string_view caption) const
{
    if (!log) {
        return;
    }
    std::print(stdout, "Stack {}: ", caption);
    auto first { true };
    for (auto ref : node_stack) {
        auto &n = node_cache[ref];
        if (!first) {
            std::print(stdout, " | ");
        }
        first = false;
        std::print(stdout, "{}", to_string(n.kind));
    }
    std::println(stdout, "");
}

void ArwenParser::dump()
{
    auto const &program_node = get_node(program);
    std::println("{}", program_node);
}

}

using namespace Arwen;

using P = Parser<ArwenParser>;

extern "C" {

[[maybe_unused]] void push_current_token(P *parser)
{
    parser->impl.push_token(parser->last_token);
}

[[maybe_unused]] void arwen_make_identifier(P *parser)
{
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::Identifier, Identifier { parser->last_token.text });
}

[[maybe_unused]] void arwen_make_member_access(P *parser)
{
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::Member, Member { parser->last_token.text });
}

[[maybe_unused]] void arwen_make_qstring(P *parser)
{
    if (parser->last_token.tag() != KindTag::String) {
        fatal("Expected quoted string, got '{s}'", parser->last_token.kind);
    }
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::StringConstant, StringConstant { parser->last_token.text });
}

[[maybe_unused]] void arwen_make_int(P *parser)
{
    if (parser->last_token.tag() != KindTag::Number) {
        fatal("Expected integer number, got '{s}'", parser->last_token.kind);
    }
    if (parser->last_token.kind.number_type() == NumberType::Float) {
        fatal("Expected integer number, got float");
    }
    auto res = parser->last_token.as<ssize_t>();
    if (res.is_error()) {
        fatal("Could not convert {} to number", parser->last_token.text);
    }
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::IntConstant, IntConstant { res.value() });
}

[[maybe_unused]] void arwen_make_float(P *parser)
{
    if (parser->last_token.tag() != KindTag::Number) {
        fatal("Expected floating point number, got '{s}'", parser->last_token.kind);
    }
    if (parser->last_token.kind.number_type() != NumberType::Float) {
        fatal("Expected floating point number, got '{}'", parser->last_token.kind.number_type());
    }
    auto res = parser->last_token.as<double>();
    if (res.is_error()) {
        fatal("Could not convert {} to floating point number", parser->last_token.text);
    }
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::FloatConstant, FloatConstant { res.value() });
}

[[maybe_unused]] void arwen_make_bool(P *parser, Value *data)
{
    if (!data) {
        fatal("Expected boolean Value as argument");
    }
    if (data->type() != Value::BoolType) {
        fatal("Expected boolean Value as argument, got '{}'", data->type());
    }
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::BoolConstant, BoolConstant { data->as_bool() });
}

[[maybe_unused]] void arwen_make_binary_expression(P *parser)
{
    auto  op_token { parser->impl.pop_token() };
    auto  op = BinaryOperatorMapping(op_token.kind);
    auto  right { parser->impl.pop_node().ref };
    auto &left { parser->impl.pop_node() };
    parser->impl.push_node(
        left.location,
        ASTNodeKind::BinaryExpression,
        BinaryExpression {
            .left = left.ref,
            .op = op.op,
            .right = right,
        });
}

[[maybe_unused]] [[maybe_unused]] void arwen_make_assignment_expression(P *parser)
{
    auto op_token { parser->impl.pop_token() };
    auto op = BinaryOperatorMapping(op_token.kind);
    if (!op.is_assignment_op && op.op != BinaryOperator::Assign) {
        fatal("Operator '{}' is not an assignment operator", op.op);
    }
    auto  right = parser->impl.pop_node().ref;
    auto &left = parser->impl.pop_node();
    switch (left.kind) {
    case ASTNodeKind::Identifier:
        break;
    case ASTNodeKind::UnaryExpression: {
        auto &impl = std::get<UnaryExpression>(left.impl);
        if (impl.op == UnaryOperator::Deref) {
            break;
        }
    }
    /* fallthrough */
    default:
        fatal("Expected lvalue, got '{}'", left);
    }
    parser->impl.push_node(
        left.location,
        ASTNodeKind::AssignmentExpression,
        AssignmentExpression {
            .left = left.ref,
            .op = op.op,
            .right = right,
        });
}

[[maybe_unused]] void arwen_make_const_decl(P *parser)
{
    ConstantDeclaration decl {};
    Location            loc;
    auto                done { false };
    while (!done) {
        auto &n = parser->impl.pop_node();
        switch (n.kind) {
        case ASTNodeKind::Identifier:
            decl.name = std::get<Identifier>(n.impl).text;
            loc = n.location;
            break;
        case ASTNodeKind::BasicTypeNode:
        case ASTNodeKind::PointerType:
        case ASTNodeKind::ArrayType:
            decl.type = n.ref;
            break;
        case ASTNodeKind::StartBlock:
            done = true;
            break;
        default:
            decl.initializer = n.ref;
            break;
        }
    }
    parser->impl.push_node(loc, ASTNodeKind::ConstantDeclaration, decl);
}

[[maybe_unused]] void arwen_make_var_decl(P *parser)
{
    VariableDeclaration decl {};
    Location            loc;
    auto                done { false };
    while (!done) {
        auto &n = parser->impl.pop_node();
        switch (n.kind) {
        case ASTNodeKind::Identifier:
            decl.name = std::get<Identifier>(n.impl).text;
            loc = n.location;
            break;
        case ASTNodeKind::BasicTypeNode:
        case ASTNodeKind::PointerType:
        case ASTNodeKind::ArrayType:
            decl.type = n.ref;
            break;
        case ASTNodeKind::StartBlock:
            done = true;
            break;
        default:
            decl.initializer = n.ref;
            break;
        }
    }
    parser->impl.push_node(loc, ASTNodeKind::VariableDeclaration, decl);
}

[[maybe_unused]] void arwen_make_unary_expression(P *parser)
{
    auto op_token { parser->impl.pop_token() };
    auto op = UnaryOperatorMapping(op_token.kind);
    auto operand { parser->impl.pop_node() };
    parser->impl.push_node(
        operand.location,
        ASTNodeKind::UnaryExpression,
        UnaryExpression {
            .op = op.op,
            .operand = operand.ref,
        });
}

[[maybe_unused]] void arwen_make_type(P *parser)
{
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::BasicTypeNode, BasicTypeNode { .name = parser->last_token.text });
}

[[maybe_unused]] void arwen_make_array_type(P *parser)
{
#if 0
    auto &element_type = parser->impl.pop_one_of(typeKinds).ref;
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::ArrayType,
        ArrayType {
            .element_type = element_type,
        });
#endif
    fatal("Array types unsupported right now");
}

[[maybe_unused]] void arwen_make_pointer_type(P *parser)
{
    auto &element_type = parser->impl.pop_one_of(typeKinds).ref;
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::PointerType,
        PointerType {
            .element_type = element_type,
        });
}

[[maybe_unused]] void arwen_make_parameter(P *parser)
{
    auto &param_type = parser->impl.pop_one_of(typeKinds).ref;
    auto &ident = parser->impl.pop_typed_node(ASTNodeKind::Identifier);
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::Parameter,
        Parameter {
            .name = std::get<Identifier>(ident.impl).text,
            .type = param_type,
        });
}

void make_function_decl_(P *parser, std::optional<NodeReference> return_type)
{
    std::vector<NodeReference> params;
    for (auto param = parser->impl.try_pop_typed_node(ASTNodeKind::Parameter); param; param = parser->impl.try_pop_typed_node(ASTNodeKind::Parameter)) {
        params.push_back(param->ref);
    }
    std::reverse(params.begin(), params.end());
    auto &name = parser->impl.pop_typed_node(ASTNodeKind::Identifier);
    auto &ret = parser->impl.push_node(
        name.location,
        ASTNodeKind::Function,
        Function {
            .name = std::get<Identifier>(name.impl).text,
            .parameters = params,
            .return_type = return_type,
        });
}

[[maybe_unused]] void arwen_make_function_decl(P *parser)
{
    make_function_decl_(parser, parser->impl.pop_one_of(typeKinds).ref);
}

[[maybe_unused]] void arwen_make_void_function_decl(P *parser)
{
    make_function_decl_(parser, {});
}

[[maybe_unused]] void arwen_make_function(P *parser)
{
    auto &block = parser->impl.pop_typed_node(ASTNodeKind::Block);
    auto &decl = parser->impl.pop_typed_node(ASTNodeKind::Function);
    auto &function_impl = parser->impl.make_node(
        block.location,
        ASTNodeKind::FunctionImplementation,
        FunctionImplementation {
            .implementation = block.ref,
        });
    std::get<Function>(decl.impl).implementation = function_impl.ref;
    auto &module_node = parser->impl.get_typed_node(*parser->impl.module, ASTNodeKind::Module);
    auto &mod = std::get<Module>(module_node.impl);
    mod.names.push_back(decl.ref);
}

[[maybe_unused]] void arwen_make_foreign_function(P *parser)
{
    auto &foreign_func = parser->impl.pop_typed_node(ASTNodeKind::StringConstant);
    auto  decl_ref = parser->impl.pop_typed_node(ASTNodeKind::Function).ref;
    auto &foreign_func_node = parser->impl.make_node(
        foreign_func.location,
        ASTNodeKind::ForeignFunction,
        ForeignFunction { std::get<StringConstant>(foreign_func.impl).value });
    std::get<Function>(parser->impl.get_node(decl_ref).impl).implementation = foreign_func_node.ref;
    auto &module_node = parser->impl.get_typed_node(*parser->impl.module, ASTNodeKind::Module);
    auto &mod = std::get<Module>(module_node.impl);
    mod.names.push_back(decl_ref);
}

[[maybe_unused]] void arwen_start_block(P *parser)
{
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::StartBlock, StartBlock {});
}

[[maybe_unused]] void arwen_finish_block(P *parser)
{
    std::vector<NodeReference> stmts;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &stmt = parser->impl.pop_node();
        stmts.push_back(stmt.ref);
    }
    std::reverse(stmts.begin(), stmts.end());
    auto                           &start = parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    std::optional<std::string_view> label;
    if (parser->impl.peek_kind() == ASTNodeKind::Label) {
        auto &label_node = parser->impl.pop_typed_node(ASTNodeKind::Label);
        label = std::get<Label>(label_node.impl).label;
    }
    parser->impl.push_node(
        start.location,
        ASTNodeKind::Block,
        Block {
            .label = label,
            .statements = stmts,
        });
}

[[maybe_unused]] void arwen_finish_if(P *parser)
{
    std::vector<NodeReference> nodes;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &n = parser->impl.pop_node();
        nodes.push_back(n.ref);
    }
    std::reverse(nodes.begin(), nodes.end());
    auto &start = parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    if (nodes.size() < 2 || nodes.size() > 3) {
        fatal("Invalid number of nodes in if statement: got {}, need 2 or 3", nodes.size());
    }
    std::optional<NodeReference> false_branch = {};
    if (nodes.size() == 3) {
        false_branch = nodes[2];
    }
    parser->impl.push_node(
        start.location,
        ASTNodeKind::If,
        If {
            .condition = nodes[0],
            .true_branch = nodes[1],
            .false_branch = false_branch,
        });
}

[[maybe_unused]] void arwen_make_label(P *parser)
{
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::Label,
        Label {
            .label = parser->last_token.text,
        });
}

[[maybe_unused]] void arwen_make_loop(P *parser)
{
    auto &body = parser->impl.pop_node();

    // #foo loop x = 42 does not make any sense. So the label should have
    // been consumed by the block following 'loop'.
    parser->impl.try_pop_typed_node(ASTNodeKind::Label);
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::Loop,
        Loop {
            .body = body.ref,
        });
}

[[maybe_unused]] void arwen_make_return_with_value(P *parser)
{
    auto &value = parser->impl.pop_one_of(expressionKinds);
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::Return,
        Return {
            .expression = value.ref,
        });
}

[[maybe_unused]] void arwen_make_void_return(P *parser)
{
    parser->impl.push_node(
        parser->last_token.location,
        ASTNodeKind::Return,
        Return {
            .expression = {},
        });
}

[[maybe_unused]] void arwen_make_function_call(P *parser)
{
    std::vector<NodeReference> args;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &expr = parser->impl.pop_one_of(expressionKinds);
        args.push_back(expr.ref);
    }
    std::reverse(args.begin(), args.end());
    parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    auto &name = parser->impl.pop_typed_node(ASTNodeKind::Identifier);
    parser->impl.push_node(
        name.location,
        ASTNodeKind::FunctionCall,
        FunctionCall {
            .name = std::get<Identifier>(name.impl).text,
            .arguments = args,
        });
}

[[maybe_unused]] void arwen_make_subscript(P *parser)
{
    std::vector<NodeReference> subscripts;
    Location                   l;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &expr = parser->impl.pop_one_of(expressionKinds);
        subscripts.push_back(expr.ref);
        l = expr.location;
    }
    std::reverse(subscripts.begin(), subscripts.end());
    parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    parser->impl.push_node(
        l,
        ASTNodeKind::Subscript,
        Subscript {
            .subscripts = subscripts,
        });
}
}
