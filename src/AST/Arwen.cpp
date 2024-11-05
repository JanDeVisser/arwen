/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <AST/AST.h>
#include <Value.h>

namespace Arwen {

std::array<ASTNodeKind, 9> expressionKinds = {
    ASTNodeKind::AssignmentExpression,
    ASTNodeKind::BinaryExpression,
    ASTNodeKind::BoolConstant,
    ASTNodeKind::FloatConstant,
    ASTNodeKind::FunctionCall,
    ASTNodeKind::Identifier,
    ASTNodeKind::IntConstant,
    ASTNodeKind::QString,
    ASTNodeKind::UnaryExpression,
};

std::array<ASTNodeKind, 3> typeKinds = {
    ASTNodeKind::BasicType,
    ASTNodeKind::ArrayType,
    ASTNodeKind::PointerType,
};

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

NodeReference ArwenParser::push_node(ASTNode const &n)
{
    auto ref = cache_node(n);
    node_stack.push_back(ref);
    dump_node_stack("push_node");
    return ref;
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

ASTNode const &ArwenParser::get_typed_node(ASTNodeKind kind, NodeReference ref) const
{
    dump_node_stack("get_typed_node");
    auto &ret = get_node(ref);
    if (ret.kind == kind) {
        return ret;
    }
    fatal("pop_typed_node({}): got '{}'", kind, ret.kind);
}

std::optional<ASTNode> ArwenParser::try_get_typed_node(ASTNodeKind kind, NodeReference ref) const
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
//    if (!log) {
//        return;
//    }
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
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Identifier,
        .impl = parser->last_token.text,
    });
}

[[maybe_unused]] void arwen_make_qstring(P *parser)
{
    if (parser->last_token.tag() != KindTag::String) {
        fatal("Expected quoted string, got '{s}'", parser->last_token.kind);
    }
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::QString,
        .impl = parser->last_token.text,
    });
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
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::IntConstant,
        .impl = res.value(),
    });
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
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::FloatConstant,
        .impl = res.value(),
    });
}

[[maybe_unused]] void arwen_make_bool(P *parser, Value *data)
{
    if (!data) {
        fatal("Expected boolean Value as argument");
    }
    if (data->type != ValueType::Bool) {
        fatal("Expected boolean Value as argument, got '{}'", data->type);
    }
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::BoolConstant,
        .impl = data->payload.bool_value,
    });
}

[[maybe_unused]] void arwen_make_binary_expression(P *parser)
{
    auto  op_token { parser->impl.pop_token() };
    auto  op = BinaryOperatorMapping(op_token.kind);
    auto  right { parser->impl.pop_node().ref };
    auto &left { parser->impl.pop_node() };
    parser->impl.push_node(ASTNode {
        .location = left.location,
        .kind = ASTNodeKind::BinaryExpression,
        .impl = BinaryExpression {
            .left = left.ref,
            .op = op.op,
            .right = right,
        },
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
    parser->impl.push_node(ASTNode {
        .location = left.location,
        .kind = ASTNodeKind::AssignmentExpression,
        .impl = AssignmentExpression {
            .left = left.ref,
            .op = op.op,
            .right = right,
        },
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
            decl.name = std::get<std::string_view>(n.impl);
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
    parser->impl.push_node(ASTNode {
        .location = loc,
        .kind = ASTNodeKind::ConstantDeclaration,
        .impl = decl,
    });
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
            decl.name = std::get<std::string_view>(n.impl);
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
    parser->impl.push_node(ASTNode {
        .location = loc,
        .kind = ASTNodeKind::VariableDeclaration,
        .impl = decl,
    });
}

[[maybe_unused]] void arwen_make_unary_expression(P *parser)
{
    auto op_token { parser->impl.pop_token() };
    auto op = UnaryOperatorMapping(op_token.kind);
    auto operand { parser->impl.pop_node() };
    parser->impl.push_node(ASTNode {
        .location = operand.location,
        .kind = ASTNodeKind::UnaryExpression,
        .impl = UnaryExpression {
            .op = op.op,
            .operand = operand.ref,
        },
    });
}

[[maybe_unused]] void arwen_make_type(P *parser)
{
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::BasicType,
        .impl = parser->last_token.text,
    });
}

[[maybe_unused]] void arwen_make_array_type(P *parser)
{
    auto &element_type = parser->impl.pop_one_of(typeKinds).ref;
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::ArrayType,
        .impl = ArrayType {
            .element_type = element_type,
        },
    });
}

[[maybe_unused]] void arwen_make_pointer_type(P *parser)
{
    auto &element_type = parser->impl.pop_one_of(typeKinds).ref;
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::PointerType,
        .impl = PointerType {
            .element_type = element_type,
        },
    });
}

[[maybe_unused]] void arwen_make_parameter(P *parser)
{
    auto &param_type = parser->impl.pop_one_of(typeKinds).ref;
    auto &ident = parser->impl.pop_typed_node(ASTNodeKind::Identifier);
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Parameter,
        .impl = Parameter {
            .name = std::get<std::string_view>(ident.impl),
            .type = param_type,
        },
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
    parser->impl.push_node(ASTNode {
        .location = name.location,
        .kind = ASTNodeKind::FunctionDecl,
        .impl = FunctionDecl {
            .name = std::get<std::string_view>(name.impl),
            .parameters = params,
            .return_type = return_type,
        },
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
    auto  func_ref = parser->impl.cache_node(ASTNode {
         .location = decl.location,
         .kind = ASTNodeKind::Function,
         .impl = Function {
             .declaration = decl.ref,
             .implementation = block.ref,
        },
    });
    auto &program_node = parser->impl.pop_typed_node(ASTNodeKind::Program);
    auto &program = std::get<Program>(program_node.impl);

    program.declarations.push_back(func_ref);
    program.declarations_index.emplace(std::get<FunctionDecl>(decl.impl).name, func_ref);
    parser->impl.push_node(program_node);
}

[[maybe_unused]] void arwen_start_block(P *parser)
{
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::StartBlock,
    });
}

[[maybe_unused]] void arwen_finish_block(P *parser)
{
    std::vector<NodeReference> stmts;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &stmt = parser->impl.pop_node();
        stmts.push_back(stmt.ref);
    }
    std::reverse(stmts.begin(), stmts.end());
    auto &start = parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    parser->impl.push_node(ASTNode {
        .location = start.location,
        .kind = ASTNodeKind::Block,
        .impl = Block {
            .statements = stmts,
        },
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
    parser->impl.push_node(ASTNode {
        .location = start.location,
        .kind = ASTNodeKind::If,
        .impl = If {
            .condition = nodes[0],
            .true_branch = nodes[1],
            .false_branch = false_branch,
        },
    });
}

[[maybe_unused]] void arwen_make_label(P *parser)
{
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Label,
        .impl = Label {
            .label = parser->last_token.text,
        },
    });
}

[[maybe_unused]] void arwen_make_loop(P *parser)
{
    auto                        &body = parser->impl.pop_node();
    auto                         label = parser->impl.try_pop_typed_node(ASTNodeKind::Label);
    std::optional<NodeReference> label_ref {};
    if (label) {
        label_ref = label->ref;
    }

    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Loop,
        .impl = Loop {
            .label = label_ref,
            .body = body.ref,
        },
    });
}

[[maybe_unused]] void arwen_make_return_with_value(P *parser)
{
    auto &value = parser->impl.pop_one_of(expressionKinds);
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Return,
        .impl = Return {
            .expr = value.ref,
        },
    });
}

[[maybe_unused]] void arwen_make_void_return(P *parser)
{
    parser->impl.push_node(ASTNode {
        .location = parser->last_token.location,
        .kind = ASTNodeKind::Return,
        .impl = Return {
            .expr = {},
        },
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
    parser->impl.push_node(ASTNode {
        .location = name.location,
        .kind = ASTNodeKind::FunctionCall,
        .impl = FunctionCall {
            .name = std::get<std::string_view>(name.impl),
            .arguments = args,
        },
    });
}

[[maybe_unused]] void arwen_make_subscript(P *parser)
{
    std::vector<NodeReference> subscripts;
    while (parser->impl.peek_kind() != ASTNodeKind::StartBlock) {
        auto &expr = parser->impl.pop_one_of(expressionKinds);
        subscripts.push_back(expr.ref);
    }
    std::reverse(subscripts.begin(), subscripts.end());
    parser->impl.pop_typed_node(ASTNodeKind::StartBlock);
    auto &name = parser->impl.pop_typed_node(ASTNodeKind::Identifier);
    parser->impl.push_node(ASTNode {
        .location = name.location,
        .kind = ASTNodeKind::Subscript,
        .impl = Subscript {
            .subscripts = subscripts,
        },
    });
}

[[maybe_unused]] void arwen_program(P *parser)
{
    parser->impl.push_node(ASTNode {
        .location = {},
        .kind = ASTNodeKind::Program,
        .impl = Program {},
    });
}

[[maybe_unused]] void arwen_pop_program(P *parser)
{
    auto &p = parser->impl.pop_typed_node(ASTNodeKind::Program);
    parser->impl.program = p.ref;
}

}
