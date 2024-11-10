/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <AST/AST.h>
#include <Value.h>

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
    ASTNodeKind::QString,
    ASTNodeKind::UnaryExpression,
};

std::array<ASTNodeKind, 3> typeKinds = {
    ASTNodeKind::BasicType,
    ASTNodeKind::ArrayType,
    ASTNodeKind::PointerType,
};

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

void ArwenParser::startup()
{
    node_stack.clear();
    node_cache.clear();
}

void ArwenParser::cleanup() const
{
    if (!node_stack.empty()) {
        std::println(stderr, "Stack not empty on cleanup");
    }
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
    dump_node_stack("pop_node");
    if (node_stack.empty()) {
        fatal("node_stack underflow");
    }
    auto ret = node_stack.back();
    node_stack.pop_back();
    return get_node(ret);
}

ASTNode &ArwenParser::pop_typed_node(ASTNodeKind kind)
{
    dump_node_stack("pop_typed_node");
    auto &ret = pop_node();
    if (ret.kind == kind) {
        return ret;
    }
    fatal("pop_typed_node({}): got '{}'", kind, ret.kind);
}

std::optional<ASTNode> ArwenParser::try_pop_typed_node(ASTNodeKind kind)
{
    dump_node_stack("pop_typed_node_optional");
    auto &top = peek_node();
    if (top.kind == kind) {
        return pop_node();
    }
    return {};
}

ASTNode const &ArwenParser::push_node(Location location, ASTNodeKind kind, ASTNodeImpl const &impl)
{
    ASTNode n { kind, impl };
    n.location = location;
    auto ref = cache_node(n);
    node_stack.push_back(ref);
    return node_cache.at(ref);
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
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::QString, StringConstant { std::string { parser->last_token.text } });
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
    if (data->type != ValueType::Bool) {
        fatal("Expected boolean Value as argument, got '{}'", data->type);
    }
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::BoolConstant, BoolConstant { data->payload.bool_value });
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
        case ASTNodeKind::BasicType:
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
        case ASTNodeKind::BasicType:
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
    parser->impl.push_node(parser->last_token.location, ASTNodeKind::BasicType, BasicType { .name = parser->last_token.text });
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
    parser->impl.push_node(
        name.location,
        ASTNodeKind::FunctionDecl,
        FunctionDecl {
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
    auto &decl = parser->impl.pop_typed_node(ASTNodeKind::FunctionDecl);
    auto  func = parser->impl.push_node(
        decl.location,
        ASTNodeKind::Function,
        Function {
             .declaration = decl.ref,
             .implementation = block.ref,
        });
    parser->impl.pop_node();
    auto &program_node = parser->impl.get_typed_node(0, ASTNodeKind::Program);
    auto &program = std::get<Program>(program_node.impl);

    program.declarations.push_back(func.ref);
}

[[maybe_unused]] void arwen_make_foreign_function(P *parser)
{
    auto &foreign_func = parser->impl.pop_typed_node(ASTNodeKind::QString);
    auto &decl = parser->impl.pop_typed_node(ASTNodeKind::FunctionDecl);
    auto  func = parser->impl.push_node(
        decl.location,
        ASTNodeKind::ForeignFunction,
        ForeignFunction {
             .declaration = decl.ref,
             .foreign_function = foreign_func.ref,
        });
    parser->impl.pop_node();
    auto &program_node = parser->impl.get_typed_node(0, ASTNodeKind::Program);
    auto &program = std::get<Program>(program_node.impl);

    program.declarations.push_back(func.ref);
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

[[maybe_unused]] void arwen_program(P *parser)
{
    parser->impl.push_node({}, ASTNodeKind::Program, Program {});
}

[[maybe_unused]] void arwen_pop_program(P *parser)
{
    auto &p = parser->impl.pop_typed_node(ASTNodeKind::Program);
    parser->impl.program = p.ref;
}

}
