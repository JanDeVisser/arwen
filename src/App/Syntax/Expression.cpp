/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <functional>
#include <memory>
#include <ostream>
#include <print>
#include <ranges>
#include <variant>

#include <Util/Logging.h>
#include <Util/TokenLocation.h>
#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

namespace Arwen {

enum class PseudoType {
    Self,
    Lhs,
    Rhs,
};

struct Operand final {
    std::variant<pType, TypeKind, PseudoType> type;

    Operand(pType t) // NOLINT(*-explicit-constructor)
        : type(std::move(t))
    {
    }

    Operand(TypeKind k) // NOLINT(*-explicit-constructor)
        : type(k)
    {
    }

    Operand(PseudoType pseudo_type) // NOLINT(*-explicit-constructor)
        : type(pseudo_type)
    {
    }

    [[nodiscard]] bool matches(pType const &concrete, pType const &hint = nullptr) const
    {
        return std::visit(overloads {
                              [&concrete](pType const &t) -> bool {
                                  return concrete == t;
                              },
                              [&concrete](TypeKind k) -> bool {
                                  return concrete->is(k);
                              },
                              [&hint, &concrete](PseudoType pseudo_type) -> bool {
                                  assert(hint != nullptr);
                                  return hint == concrete;
                              },
                          },
            type);
    }
};

using OpResult = std::variant<pType, PseudoType>;

struct BinaryOperator {
    Operand  lhs;
    Operator op;
    Operand  rhs;
    OpResult result;

    [[nodiscard]] bool matches(pType const &concrete_lhs, pType const &concrete_rhs) const
    {
        return lhs.matches(concrete_lhs, concrete_rhs) && rhs.matches(concrete_rhs, concrete_lhs);
    }
};

static std::vector<BinaryOperator> binary_ops;

struct AssignOperator {
    Operator assign_op;
    Operator bin_op;
};

static std::map<Operator, Operator> assign_ops = {
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignDecrement, Operator::Subtract },
    { Operator::AssignDivide, Operator::Divide },
    { Operator::AssignIncrement, Operator::Add },
    { Operator::AssignModulo, Operator::Modulo },
    { Operator::AssignMultiply, Operator::Multiply },
    { Operator::AssignOr, Operator::BinaryOr },
    { Operator::AssignShiftLeft, Operator::ShiftLeft },
    { Operator::AssignShiftRight, Operator::ShiftRight },
    { Operator::AssignXor, Operator::BinaryXor },
};

struct UnaryOperator {
    Operator op;
    Operand  operand;
    OpResult result;
};

static std::vector<UnaryOperator> unary_ops;

BinaryExpression::BinaryExpression(pSyntaxNode lhs, Operator const op, pSyntaxNode rhs)
    : SyntaxNode(SyntaxNodeType::BinaryExpression)
    , lhs(std::move(lhs))
    , op(op)
    , rhs(std::move(rhs))
{
}

pSyntaxNode BinaryExpression::normalize(Parser &parser)
{
    auto make_expression_list = [this, &parser]() -> pSyntaxNode {
        SyntaxNodes                      nodes;
        std::function<void(pSyntaxNode)> flatten;
        flatten = [&nodes, &flatten](pSyntaxNode const &n) {
            if (auto const binary_expr = std::dynamic_pointer_cast<BinaryExpression>(n); binary_expr != nullptr) {
                if (binary_expr->op == Operator::Sequence) {
                    flatten(binary_expr->lhs);
                    nodes.push_back(binary_expr->rhs);
                } else {
                    nodes.push_back(binary_expr);
                }
            } else {
                nodes.push_back(n);
            }
        };
        flatten(shared_from_this());
        return normalize_node(make_node<ExpressionList>(nodes, nodes), parser);
    };

    auto make_member_path = [this, &parser]() -> pSyntaxNode {
        Identifiers path;
        auto        add_identifier = [&parser, &path](pSyntaxNode const &n) -> bool {
            if (auto const ident = std::dynamic_pointer_cast<Identifier>(n); ident == nullptr) {
                parser.append(n->location, "Member path segment must be identifier");
                return false;
            } else {
                path.push_back(ident);
                return true;
            }
        };

        std::function<void(pSyntaxNode)> flatten;
        flatten = [&parser, &path, &flatten, &add_identifier](pSyntaxNode const &n) {
            if (auto const binex = std::dynamic_pointer_cast<BinaryExpression>(n); binex) {
                if (binex->op == Operator::MemberAccess) {
                    if (!add_identifier(binex->lhs)) {
                        path.clear();
                        return;
                    }
                    flatten(binex->rhs);
                } else {
                    parser.append(n->location, "Member path segment must be identifier");
                    path.clear();
                }
            } else {
                if (!add_identifier(n)) {
                    path.clear();
                }
            }
        };
        flatten(shared_from_this());
        if (path.empty()) {
            return nullptr;
        }
        return normalize_node(make_node<MemberPath>(path[0]->location + path.back()->location, path), parser);
    };

    auto const_evaluate = [this, &parser](pSyntaxNode const &lhs, Operator op, pSyntaxNode const &rhs) -> pSyntaxNode {
        auto const &lhs_const = std::dynamic_pointer_cast<Constant>(lhs);
        if (lhs_const == nullptr || !lhs_const->bound_value) {
            return make_node<BinaryExpression>(location, lhs, op, rhs);
        }
        if (op == Operator::Cast) {
            if (auto const rhs_type = std::dynamic_pointer_cast<TypeSpecification>(rhs); rhs_type != nullptr) {
                if (auto const cast_type = rhs_type->resolve(parser); cast_type != nullptr) {
                    if (auto const coerced_maybe = lhs_const->bound_value.value().coerce(cast_type); coerced_maybe) {
                        return make_node<Constant>(lhs->location + rhs->location, *coerced_maybe);
                    }
                    parser.append(location, L"Cannot cast value to `{}`", rhs_type->to_string());
                    return nullptr;
                }
            }
        }
        if (auto const &rhs_const = std::dynamic_pointer_cast<Constant>(rhs); rhs_const != nullptr && rhs_const->bound_value) {
            auto ret = evaluate(lhs_const->bound_value.value(), op, rhs_const->bound_value.value());
            return make_node<Constant>(lhs->location + rhs->location, ret);
        }
        return make_node<BinaryExpression>(location, lhs, op, rhs);
    };

    switch (op) {
    case Operator::Assign: {
        auto new_lhs = normalize_node(lhs, parser);
        auto new_rhs = normalize_node(rhs, parser);
        if (auto const ident = std::dynamic_pointer_cast<Identifier>(new_lhs); ident != nullptr) {
            Identifiers path { ident };
            return normalize_node(
                make_node<BinaryExpression>(
                    location,
                    make_node<MemberPath>(new_lhs->location, path),
                    op,
                    new_rhs),
                parser);
        }
        if (std::dynamic_pointer_cast<MemberPath>(new_lhs) == nullptr) {
            parser.append(location, "Cannot assign to non-lvalues");
            return nullptr;
        }
        return make_node<BinaryExpression>(
            location,
            new_lhs,
            op,
            new_rhs);
    }
    case Operator::Call: {
        auto arg_list = normalize_node(rhs, parser);
        if (std::dynamic_pointer_cast<Void>(arg_list) != nullptr) {
            arg_list = normalize_node(make_node<ExpressionList>(arg_list->location, SyntaxNodes {}), parser);
        }
        if (std::dynamic_pointer_cast<ExpressionList>(arg_list) == nullptr) {
            arg_list = normalize_node(make_node<ExpressionList>(arg_list->location, SyntaxNodes { arg_list }), parser);
        }
        return make_node<Call>(location, normalize_node(lhs, parser), std::dynamic_pointer_cast<ExpressionList>(arg_list));
    }
    case Operator::MemberAccess:
        return make_member_path();
    case Operator::Sequence:
        return make_expression_list();
    default:
        if (assign_ops.contains(op)) {
            auto const bin_expr = make_node<BinaryExpression>(location, normalize_node(lhs, parser), assign_ops[op], rhs->normalize(parser));
            return make_node<BinaryExpression>(location, normalize_node(lhs, parser), Operator::Assign, normalize_node(bin_expr, parser));
        }
        return const_evaluate(normalize_node(lhs, parser), op, normalize_node(rhs, parser));
    }
}

pSyntaxNode BinaryExpression::stamp(Parser &parser)
{
    return make_node<BinaryExpression>(location, lhs->stamp(parser), op, rhs->stamp(parser));
}

pType BinaryExpression::bind(Parser &parser)
{
    if (binary_ops.empty()) {
        binary_ops = {
            { TypeKind::IntType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
            { TypeRegistry::string, Operator::Multiply, TypeKind::IntType, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::FloatType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::FloatType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
            { TypeKind::IntType, Operator::BinaryAnd, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::BinaryOr, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::BinaryXor, PseudoType::Lhs, PseudoType::Lhs },
            { TypeKind::IntType, Operator::ShiftLeft, TypeRegistry::u8, PseudoType::Lhs },
            { TypeKind::IntType, Operator::ShiftRight, TypeRegistry::u8, PseudoType::Lhs },
            { TypeRegistry::boolean, Operator::LogicalAnd, TypeRegistry::boolean, TypeRegistry::boolean },
            { TypeRegistry::boolean, Operator::LogicalOr, TypeRegistry::boolean, TypeRegistry::boolean },
        };
    }

    auto lhs_type = bind_node(lhs, parser);
    if (lhs->status == Status::BindErrors || lhs->status == Status::Ambiguous) {
        return lhs_type;
    }
    if (lhs->status == Status::Ambiguous) {
        return parser.bind_error(lhs->location, L"Type ambiguity");
    }
    auto rhs_type = bind_node(rhs, parser);
    if (rhs->status == Status::BindErrors || rhs->status == Status::Ambiguous) {
        return lhs_type;
    }
    if (rhs->status == Status::Ambiguous) {
        return parser.bind_error(lhs->location, L"Type ambiguity");
    }

    if (op == Operator::Assign) {
        if (auto const member_path = std::dynamic_pointer_cast<MemberPath>(lhs); member_path == nullptr) {
            return parser.bind_error(location, L"Cannot assign to non-lvalues");
        }
        if (lhs_type != rhs_type) {
            return parser.bind_error(
                location,
                L"Cannot assign a value of type `{}` to a variable of type `{}`",
                rhs_type->name,
                lhs_type->name);
        }
        return lhs_type;
    }

    if (op == Operator::Call && lhs_type->is<FunctionType>() && rhs_type->is<TypeList>()) {
        auto const [parameters, result] = std::get<FunctionType>(lhs_type->description);
        auto const [types] = std::get<TypeList>(rhs_type->description);
        if (parameters.size() != types.size()) {
            if (auto const ident_node = std::dynamic_pointer_cast<Identifier>(lhs); ident_node != nullptr) {
                return parser.bind_error(
                    location,
                    L"Invalid number of arguments calling function `{}`. Expected {}, got {}.",
                    ident_node->identifier,
                    parameters.size(),
                    types.size());
            }
            return parser.bind_error(
                location,
                L"Invalid number of arguments calling function. Expected {}, got {}.",
                parameters.size(),
                types.size());
        }
        for (auto const &[param, arg] : std::views::zip(parameters, types)) {
            if (param != arg) {
                return parser.bind_error(
                    location,
                    L"Invalid argument type. Expected {}, got {}.",
                    param->name,
                    arg->name);
            }
        }
        return result;
    }

    if (op == Operator::Cast) {
        if (auto const &lhs_const = std::dynamic_pointer_cast<Constant>(lhs); lhs_const != nullptr) {
            if (auto const &rhs_type_node = std::dynamic_pointer_cast<TypeSpecification>(rhs); rhs_type_node != nullptr) {
                if (auto type = rhs_type_node->resolve(parser); type != nullptr) {
                    if (auto const &casted = lhs_const->coerce(type, parser); casted != nullptr) {
                        return type;
                    }
                }
            }
        }
        return std::visit(overloads {
                              [&rhs_type, this, &parser](IntType const &lhs_int_type, IntType const &rhs_int_type) {
                                  if (lhs_int_type.width_bits > rhs_int_type.width_bits) {
                                      return parser.bind_error(
                                          location,
                                          L"Invalid argument type. Cannot narrow integers");
                                  }
                                  return rhs_type;
                              },
                              [this, &parser](auto const &, auto const &) {
                                  return parser.bind_error(
                                      location,
                                      L"Invalid argument type. Can only cast integers");
                              } },
            lhs_type->description, rhs_type->description);
    }

    auto check_operators = [](pType const &op_lhs_type, pType const &op_rhs_type) -> pType {
        for (auto const &o : binary_ops) {
            if (o.matches(op_lhs_type, op_rhs_type)) {
                return std::visit(overloads {
                                      [](pType const &result_type) -> pType {
                                          return result_type;
                                      },
                                      [&op_lhs_type, &op_rhs_type](PseudoType const &pseudo_type) -> pType {
                                          switch (pseudo_type) {
                                          case PseudoType::Lhs:
                                              return op_lhs_type;
                                          case PseudoType::Rhs:
                                              return op_rhs_type;
                                          default:
                                              UNREACHABLE();
                                          }
                                      } },
                    o.result);
            }
        }
        return nullptr;
    };

    if (auto result = check_operators(lhs_type, rhs_type); result != nullptr) {
        return result;
    }
    if (auto const rhs_coerced_to_lhs = rhs->coerce(lhs_type, parser); rhs_coerced_to_lhs != nullptr) {
        if (auto result = check_operators(lhs_type, rhs_coerced_to_lhs->bound_type); result != nullptr) {
            rhs = rhs_coerced_to_lhs;
            return result;
        }
    }
    if (auto const lhs_coerced_to_rhs = lhs->coerce(rhs_type, parser); lhs_coerced_to_rhs != nullptr) {
        if (auto result = check_operators(lhs_coerced_to_rhs->bound_type, rhs_type); result != nullptr) {
            lhs = lhs_coerced_to_rhs;
            return result;
        }
    }

    return parser.bind_error(
        location,
        L"Operator `{}` cannot be applied to left hand type `{}` and right hand type `{}`",
        as_wstring(Operator_name(op)),
        lhs_type->name,
        rhs_type->name);
}

std::wostream &BinaryExpression::header(std::wostream &os)
{
    return os << Operator_name(op);
}

void BinaryExpression::dump_node(int const indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
}

UnaryExpression::UnaryExpression(Operator const op, pSyntaxNode operand)
    : SyntaxNode(SyntaxNodeType::UnaryExpression)
    , op(op)
    , operand(std::move(operand))
{
}

pSyntaxNode UnaryExpression::normalize(Parser &parser)
{
    auto normalized_operand = normalize_node(operand, parser);
    if (auto operand_const = std::dynamic_pointer_cast<Constant>(operand); operand_const != nullptr) {
        auto res = evaluate(op, operand_const->bound_value.value());
        return make_node<Constant>(location, res);
    }
    if (op == Operator::Sizeof) {
        if (auto const &operand_as_type = std::dynamic_pointer_cast<TypeSpecification>(normalized_operand); operand_as_type != nullptr) {
            if (auto const type_maybe = operand_as_type->resolve(parser); type_maybe != nullptr) {
                return make_node<Constant>(location, static_cast<int64_t>(type_maybe->size_of()));
            }
        }
    }
    return make_node<UnaryExpression>(location, op, normalized_operand);
}

pSyntaxNode UnaryExpression::stamp(Parser &parser)
{
    return make_node<UnaryExpression>(location, op, operand->stamp(parser));
}

pType UnaryExpression::bind(Parser &parser)
{
    if (unary_ops.empty()) {
        unary_ops = {
            { Operator::BinaryInvert, TypeKind::IntType, PseudoType::Self },
            { Operator::Idempotent, TypeKind::IntType, PseudoType::Self },
            { Operator::Idempotent, TypeKind::FloatType, PseudoType::Self },
            { Operator::Negate, TypeKind::IntType, PseudoType::Self },
            { Operator::Negate, TypeKind::FloatType, PseudoType::Self },
            { Operator::LogicalInvert, TypeRegistry::boolean, TypeRegistry::boolean },
            { Operator::Length, TypeKind::SliceType, TypeRegistry::i32 },
            { Operator::Length, TypeKind::Array, TypeRegistry::i32 },
            { Operator::Length, TypeKind::DynArray, TypeRegistry::i32 },
            { Operator::Length, TypeKind::ZeroTerminatedArray, TypeRegistry::i32 },
        };
    }
    auto operand_type = bind_node(operand, parser);
    assert(operand_type != nullptr);
    if (operand_type == TypeRegistry::undetermined || operand_type->is<BindErrors>()) {
        return operand_type;
    }
    if (operand_type == TypeRegistry::ambiguous) {
        return parser.bind_error(operand->location, std::wstring { L"Type ambiguity" });
    }
    if (op == Operator::Sizeof) {
        return TypeRegistry::i64;
    }
    for (auto const &[oper, operand, result] : unary_ops) {
        if (op == oper && operand.matches(operand_type)) {
            return std::visit(overloads {
                                  [](pType const &result_type) -> pType {
                                      return result_type;
                                  },
                                  [&operand_type](PseudoType const &pseudo_type) -> pType {
                                      switch (pseudo_type) {
                                      case PseudoType::Self:
                                          return operand_type;
                                      default:
                                          UNREACHABLE();
                                      }
                                  } },
                result);
        }
    }
    return parser.bind_error(
        location,
        L"Unary operator `{}` cannot be applied to type `{}`",
        as_wstring(Operator_name(op)),
        operand_type->name);
}

std::wostream &UnaryExpression::header(std::wostream &os)
{
    return os << Operator_name(op);
}

void UnaryExpression::dump_node(int const indent)
{
    operand->dump(indent + 4);
}

ExpressionList::ExpressionList(SyntaxNodes expressions)
    : SyntaxNode(SyntaxNodeType::ExpressionList)
    , expressions(std::move(expressions))
{
}

pSyntaxNode ExpressionList::normalize(Parser &parser)
{
    return make_node<ExpressionList>(location, normalize_nodes(expressions, parser));
}

pSyntaxNode ExpressionList::stamp(Parser &parser)
{
    return make_node<ExpressionList>(location, stamp_nodes(expressions, parser));
}

pType ExpressionList::bind(Parser &parser)
{
    std::vector<pType> types;
    for (auto const &expr : expressions) {
        auto expr_type = bind_node(expr, parser);
        if (expr->status != Status::Bound) {
            return expr_type;
        }
        types.push_back(expr_type);
    }
    return TypeRegistry::the().typelist_of(types);
}

std::wostream &ExpressionList::header(std::wostream &os)
{
    return os << L"ExpressionList";
}

void ExpressionList::dump_node(int const indent)
{
    for (auto const &stmt : expressions) {
        stmt->dump(indent + 4);
    }
}

MemberPath::MemberPath(Identifiers path)
    : SyntaxNode(SyntaxNodeType::MemberPath)
    , path(std::move(path))
{
}

pSyntaxNode MemberPath::normalize(Parser &parser)
{
    return make_node<MemberPath>(location, normalize_nodes(path, parser));
}

pSyntaxNode MemberPath::stamp(Parser &parser)
{
    return make_node<MemberPath>(location, stamp_nodes(path, parser));
}

pType MemberPath::bind(Parser &parser)
{
    pType type = nullptr;
    for (auto &segment : path) {
        if (type == nullptr) {
            type = bind_node(segment, parser);
        } else {
            if (!std::holds_alternative<StructType>(type->description)) {
                return parser.bind_error(
                    location,
                    L"Value `{}` of type `{}` is not a struct",
                    segment->identifier,
                    type->name);
            }
            auto strukt = std::get<StructType>(type->description);
            auto field_iter = strukt.field(segment->identifier);
            if (field_iter == strukt.fields.end()) {
                return parser.bind_error(
                    location,
                    L"Type `{}` has no field `{}`",
                    type->name,
                    segment->identifier);
            }
            type = field_iter->type;
        }
        segment->bound_type = type;
    }
    return type;
}

std::wostream &MemberPath::header(std::wostream &os)
{
    bool first = true;
    for (auto const &segment : path) {
        if (!first) {
            os << '.';
        }
        first = false;
        os << segment->identifier;
    }
    return os;
}

}
