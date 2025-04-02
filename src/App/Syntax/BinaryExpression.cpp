/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <functional>

#include <App/SyntaxNode.h>
#include <memory>

namespace Arwen {

BinaryExpression::BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs)
    : SyntaxNode(SyntaxNodeType::BinaryExpression)
    , lhs(lhs)
    , op(op)
    , rhs(rhs)
{
}

pSyntaxNode BinaryExpression::normalize()
{
    auto make_expression_list = [this]() -> pSyntaxNode {
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
        return make_node<ExpressionList>(nodes, nodes)->normalize();
    };

    auto evaluate = [this](pSyntaxNode const &lhs, Operator op, pSyntaxNode const &rhs) -> pSyntaxNode {
        auto lhs_const = std::dynamic_pointer_cast<ConstantExpression>(lhs);
        auto rhs_const = std::dynamic_pointer_cast<ConstantExpression>(rhs);
        if (lhs_const != nullptr && rhs_const != nullptr) {
            if (auto ret = lhs_const->evaluate_binop(op, rhs_const); ret != nullptr) {
                return ret;
            }
        }
        return make_node<BinaryExpression>(lhs->location + rhs->location, lhs, op, rhs);
    };

    if (op == Operator::Sequence) {
        return make_expression_list();
    }
    return evaluate(lhs->normalize(), op, rhs->normalize());
}

pBoundNode BinaryExpression::bind()
{
    return nullptr;
}

void BinaryExpression::header()
{
    std::cout << Operator_name(op);
}

void BinaryExpression::dump_node(int indent)
{
    lhs->dump(indent + 4);
    rhs->dump(indent + 4);
}

}
