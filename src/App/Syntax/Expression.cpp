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

struct Operand {
    std::variant<pType, TypeKind, PseudoType> type;

    Operand(pType t)
        : type(std::move(t))
    {
    }

    Operand(TypeKind k)
        : type(k)
    {
    }

    Operand(PseudoType pseudo_type)
        : type(pseudo_type)
    {
    }

    bool matches(pType const &concrete, pType const &hint = nullptr) const
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

    bool matches(pType const &concrete_lhs, pType const &concrete_rhs) const
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

BinaryExpression::BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs)
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
        flatten = [&nodes, &flatten](pSyntaxNode n) {
            if (auto binex = std::dynamic_pointer_cast<BinaryExpression>(n); binex) {
                if (binex->op == Operator::Sequence) {
                    nodes.push_back(binex->lhs);
                    flatten(binex->rhs);
                } else {
                    nodes.push_back(binex);
                }
            } else {
                nodes.push_back(n);
            }
        };
        flatten(shared_from_this());
        return make_node<ExpressionList>(nodes, nodes)->normalize(parser)->normalize(parser);
    };

    auto make_member_path = [this, &parser]() -> pSyntaxNode {
        Identifiers path;
        auto        add_identifier = [&parser, &path](pSyntaxNode n) -> bool {
            if (auto ident = std::dynamic_pointer_cast<Identifier>(n); ident == nullptr) {
                parser.append(n->location, "Member path segment must be identifier");
                return false;
            } else {
                path.push_back(ident);
                return true;
            }
        };

        std::function<void(pSyntaxNode)> flatten;
        flatten = [&parser, &path, &flatten, &add_identifier](pSyntaxNode n) {
            if (auto binex = std::dynamic_pointer_cast<BinaryExpression>(n); binex) {
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
        return make_node<MemberPath>(path[0]->location + path.back()->location, path)->normalize(parser);
    };

    auto const_evaluate = [this, &parser](pSyntaxNode const &lhs, Operator op, pSyntaxNode const &rhs) -> pSyntaxNode {
        auto lhs_const = std::dynamic_pointer_cast<Constant>(lhs);
        auto rhs_const = std::dynamic_pointer_cast<Constant>(rhs);
        if (lhs_const != nullptr && rhs_const != nullptr && lhs_const->bound_value && rhs_const->bound_value) {
            auto ret = evaluate(lhs_const->bound_value.value(), op, rhs_const->bound_value.value());
            return make_node<Constant>(lhs->location + rhs->location, ret);
        }
        return make_node<BinaryExpression>(location, lhs, op, rhs);
    };

    switch (op) {
    case Operator::Assign: {
        auto new_lhs = lhs->normalize(parser);
        auto new_rhs = rhs->normalize(parser);
        if (auto ident = std::dynamic_pointer_cast<Identifier>(new_lhs); ident != nullptr) {
            Identifiers path { ident };
            return make_node<BinaryExpression>(
                location,
                make_node<MemberPath>(new_lhs->location, path),
                op,
                new_rhs)
                ->normalize(parser);
        }
        if (std::dynamic_pointer_cast<MemberPath>(new_lhs) == nullptr) {
            parser.append(location, "Cannot assign to non-lvalues");
            return nullptr;
        }
        return make_node<BinaryExpression>(
            location,
            new_lhs,
            op,
            new_rhs)
            ->normalize(parser);
    }
    case Operator::Call: {
        auto arg_list = rhs->normalize(parser);
        if (std::dynamic_pointer_cast<Void>(arg_list) != nullptr) {
            arg_list = make_node<ExpressionList>(arg_list->location, SyntaxNodes {});
        }
        if (std::dynamic_pointer_cast<ExpressionList>(arg_list) == nullptr) {
            arg_list = make_node<ExpressionList>(arg_list->location, SyntaxNodes { arg_list });
        }
        return make_node<Call>(location, lhs->normalize(parser), std::dynamic_pointer_cast<ExpressionList>(arg_list));
    }
    case Operator::MemberAccess:
        return make_member_path();
    case Operator::Sequence:
        return make_expression_list();
    default:
        if (assign_ops.contains(op)) {
            auto bin_expr = make_node<BinaryExpression>(location, lhs->normalize(parser), assign_ops[op], rhs->normalize(parser));
            return make_node<BinaryExpression>(location, lhs->normalize(parser), Operator::Assign, bin_expr->normalize(parser))->normalize(parser);
        }
        return const_evaluate(lhs->normalize(parser), op, rhs->normalize(parser));
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
    assert(lhs_type != nullptr);
    if (lhs_type == TypeRegistry::undetermined || lhs_type->is<BindErrors>()) {
        return lhs_type;
    }
    if (lhs_type == TypeRegistry::ambiguous) {
        return parser.bind_error(lhs->location, L"Type ambiguity");
    }
    auto rhs_type = bind_node(rhs, parser);
    assert(rhs_type != nullptr);
    if (rhs_type == TypeRegistry::undetermined || rhs_type->is<BindErrors>()) {
        return rhs_type;
    }
    if (rhs_type == TypeRegistry::ambiguous) {
        return parser.bind_error(rhs->location, L"Type ambiguity");
    }

    if (op == Operator::Assign) {
        if (auto member_path = std::dynamic_pointer_cast<MemberPath>(lhs); member_path == nullptr) {
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
        auto func = std::get<FunctionType>(lhs_type->description);
        auto list = std::get<TypeList>(rhs_type->description);
        if (func.parameters.size() != list.types.size()) {
            if (auto ident_node = std::dynamic_pointer_cast<Identifier>(lhs); ident_node != nullptr) {
                return parser.bind_error(
                    location,
                    L"Invalid number of arguments calling function `{}`. Expected {}, got {}.",
                    ident_node->identifier,
                    func.parameters.size(),
                    list.types.size());
            }
            return parser.bind_error(
                location,
                L"Invalid number of arguments calling function. Expected {}, got {}.",
                func.parameters.size(),
                list.types.size());
        }
        for (auto const &[param, arg] : std::views::zip(func.parameters, list.types)) {
            if (param != arg) {
                return parser.bind_error(
                    location,
                    L"Invalid argument type. Expected {}, got {}.",
                    param->name,
                    arg->name);
            }
        }
        return func.result;
    }

    if (op == Operator::Cast) {
        auto lhs_const = std::dynamic_pointer_cast<Constant>(lhs);
        if (lhs_const != nullptr && op == Operator::Cast) {
            if (auto rhs_type = std::dynamic_pointer_cast<TypeSpecification>(rhs); rhs_type != nullptr) {
                if (auto type = rhs_type->resolve(parser); type != nullptr) {
                    if (auto casted = lhs_const->coerce(type, parser); casted != nullptr) {
                        lhs_type = lhs->bound_type = type;
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

    auto check_operators = [](pType const &lhs_type, pType const &rhs_type) -> pType {
        for (auto const &o : binary_ops) {
            if (o.matches(lhs_type, rhs_type)) {
                return std::visit(overloads {
                                      [](pType const &result_type) -> pType {
                                          return result_type;
                                      },
                                      [&lhs_type, &rhs_type](PseudoType pseudo_type) -> pType {
                                          switch (pseudo_type) {
                                          case PseudoType::Lhs:
                                              return lhs_type;
                                          case PseudoType::Rhs:
                                              return rhs_type;
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
    if (auto rhs_coerced_to_lhs = rhs->coerce(lhs_type, parser); rhs_coerced_to_lhs != nullptr) {
        if (auto result = check_operators(lhs_type, rhs_coerced_to_lhs->bound_type); result != nullptr) {
            rhs = rhs_coerced_to_lhs;
            return result;
        }
    }
    if (auto lhs_coerced_to_rhs = lhs->coerce(rhs_type, parser); lhs_coerced_to_rhs != nullptr) {
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

void BinaryExpression::dump_node(int indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
}

UnaryExpression::UnaryExpression(Operator op, pSyntaxNode operand)
    : SyntaxNode(SyntaxNodeType::UnaryExpression)
    , op(op)
    , operand(operand)
{
}

pSyntaxNode UnaryExpression::normalize(Parser &parser)
{
    auto operand_const = std::dynamic_pointer_cast<Constant>(operand);
    if (operand_const != nullptr) {
        auto res = evaluate(op, operand_const->bound_value.value());
        return make_node<Constant>(location, res);
    }
    return make_node<UnaryExpression>(location, op, operand);
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
    for (auto const &o : unary_ops) {
        if (o.op == op && o.operand.matches(operand_type)) {
            return std::visit(overloads {
                                  [](pType const &result_type) -> pType {
                                      return result_type;
                                  },
                                  [&operand_type](PseudoType pseudo_type) -> pType {
                                      switch (pseudo_type) {
                                      case PseudoType::Self:
                                          return operand_type;
                                      default:
                                          UNREACHABLE();
                                      }
                                  } },
                o.result);
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

void UnaryExpression::dump_node(int indent)
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
        if (expr_type == TypeRegistry::ambiguous || expr_type == TypeRegistry::undetermined) {
            return expr_type;
        }
        types.push_back(expr_type);
    }
    return TypeRegistry::the().typelist_of(types);
}

void ExpressionList::dump_node(int indent)
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
            type = (*field_iter).type;
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
