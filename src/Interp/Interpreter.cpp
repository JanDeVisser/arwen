/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <memory>

#include <Util/Logging.h>
#include <Util/TokenLocation.h>

#include <App/Operator.h>
#include <App/Syntax/Number.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <Interp/Interpreter.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

template<typename T>
    requires std::derived_from<T, SyntaxNode>
void execute_node(Scope &scope, std::shared_ptr<T> const &node)
{
    // trace("execute_node({})", typeid(T).name());
    scope.interpreter->stack.push_back(Value { TypeRegistry::void_ });
}

template<>
void execute_node(Scope &scope, std::shared_ptr<BinaryExpression> const &node)
{
    // trace("execute_node(BinaryExpression)");
    if (node->op == Operator::Assign) {
        auto path = std::dynamic_pointer_cast<MemberPath>(node->lhs);
        assert(path != nullptr);
        if (path->path.size() > 1) {
            fatal("Can't do structs yet");
        }
        auto ident = path->path[0];
        scope.execute(node->rhs);
        Value rhs = scope.interpreter->stack.back();
        // Don't pop the value so the return of the assignment is the value.
        scope.reassign(ident->identifier, rhs);
        return;
    }
    if (node->op == Operator::Call) {
        auto args = std::dynamic_pointer_cast<ExpressionList>(node->rhs);
        assert(args != nullptr);
        for (auto it = args->expressions.crbegin(); it != args->expressions.crend(); ++it) {
            scope.execute(*it);
        }
        auto ident = std::dynamic_pointer_cast<Identifier>(node->lhs);
        assert(ident != nullptr);
        auto func = std::dynamic_pointer_cast<FunctionDefinition>(scope.name(ident->identifier));
        assert(func != nullptr);
        auto param_scope = scope.interpreter->new_scope(func);
        for (auto const &param_def : func->declaration->parameters) {
            param_scope.values.emplace(param_def->name, scope.interpreter->stack.back());
            scope.interpreter->stack.pop_back();
        }
        scope.execute(func->implementation);
        return;
    }
    scope.execute(node->lhs);
    scope.execute(node->rhs);
    Value rhs = scope.interpreter->stack.back();
    scope.interpreter->stack.pop_back();
    Value lhs = scope.interpreter->stack.back();
    scope.interpreter->stack.pop_back();
    scope.interpreter->stack.push_back(lhs.evaluate(node->op, rhs));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Block> const &node)
{
    // trace("execute_node(Block)");
    bool first { true };
    for (auto const &stmt : node->statements) {
        if (!first) {
            scope.interpreter->stack.pop_back();
        }
        first = false;
        scope.execute(stmt);
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Decimal> const &node)
{
    // trace("execute_node(Decimal)");
    scope.interpreter->stack.emplace_back(node->value);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<DoubleQuotedString> const &node)
{
    // trace("execute_node(DoubleQuotedString)");
    scope.interpreter->stack.emplace_back(node->string);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Identifier> const &node)
{
    // trace("execute_node(Identifier)");
    scope.interpreter->stack.push_back(scope.value(node->identifier));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<IfStatement> const &node)
{
    // trace("execute_node(IfStatement)");
    scope.execute(node->condition);
    Value res = scope.interpreter->stack.back();
    scope.interpreter->stack.pop_back();
    assert(res.type == TypeRegistry::boolean);
    if (std::get<bool>(std::get<Value::PayloadValue>(res.payload))) {
        scope.execute(node->if_branch);
        return;
    }
    if (node->else_branch != nullptr) {
        scope.execute(node->else_branch);
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Integer> const &node)
{
    // trace("execute_node(Integer)");
    scope.interpreter->stack.emplace_back(node->value);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<LoopStatement> const &node)
{
    // trace("execute_node(LoopStatement)");
    while (true) {
        scope.execute(node->statement);
        if (scope.interpreter->break_) {
            if (*scope.interpreter->break_ == L"" || scope.interpreter->break_ == node->label) {
                scope.interpreter->break_.reset();
            }
            break;
        }
        if (scope.interpreter->continue_) {
            if (*scope.interpreter->continue_ == L"" || scope.interpreter->continue_ == node->label) {
                scope.interpreter->break_.reset();
            }
            continue;
        }
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Module> const &node)
{
    // trace("execute_node(Module)");
    scope.execute(node->statements);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Program> const &node)
{
    // trace("execute_node(Program)");
    for (auto &[_, mod] : node->modules) {
        scope.execute(mod);
    }
    for (auto &[_, mod] : node->modules) {
        if (auto main = mod->ns->find_name(L"main"); main != nullptr) {
            auto call_expr = make_node<BinaryExpression>(
                TokenLocation {},
                make_node<Identifier>(TokenLocation {}, L"main"),
                Operator::Call,
                make_node<ExpressionList>(TokenLocation {}, SyntaxNodes {}));
            Scope &mod_scope { scope.interpreter->new_scope(call_expr) };
            mod_scope.execute(call_expr);
            break;
        }
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<UnaryExpression> const &node)
{
    // trace("execute_node(UnaryExpression)");
    scope.execute(node->operand);
    Value operand = scope.interpreter->stack.back();
    scope.interpreter->stack.pop_back();
    scope.interpreter->stack.push_back(operand.evaluate(node->op, Value { 0 }));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<VariableDeclaration> const &node)
{
    // trace("execute_node(VariableDeclaration)");
    if (node->initializer != nullptr) {
        scope.execute(node->initializer);
        scope.values.emplace(node->name, scope.interpreter->stack.back());
        scope.interpreter->stack.pop_back();
    } else {
        scope.values.emplace(node->name, node->bound_type);
    }
    scope.interpreter->stack.push_back(scope.values.at(node->name));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<WhileStatement> const &node)
{
    // trace("execute_node(WhileStatement)");
    while (true) {
        scope.execute(node->condition);
        Value res = scope.interpreter->stack.back();
        scope.interpreter->stack.pop_back();
        assert(res.type == TypeRegistry::boolean);
        if (!std::get<bool>(std::get<Value::PayloadValue>(res.payload))) {
            break;
        }

        scope.execute(node->statement);
        if (scope.interpreter->break_) {
            if (*scope.interpreter->break_ == L"" || scope.interpreter->break_ == node->label) {
                scope.interpreter->break_.reset();
            }
            break;
        }
        if (scope.interpreter->continue_) {
            if (*scope.interpreter->continue_ == L"" || scope.interpreter->continue_ == node->label) {
                scope.interpreter->break_.reset();
            }
            continue;
        }
    }
}

void Scope::execute(pSyntaxNode const &node)
{
    // std::cerr << "Stack: (" << interpreter->stack.size() << "): ";
    // for (auto const &v : interpreter->stack) {
    //     std::wcerr << L"{ " << v.to_string() << " }";
    // }
    // std::cerr << "\n";
    switch (node->type) {
#undef S
#define S(T)                                                     \
    case SyntaxNodeType::T:                                      \
        execute_node(*this, std::dynamic_pointer_cast<T>(node)); \
        break;
        SyntaxNodeTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

Value const &Scope::value(std::wstring const &name) const
{
    if (values.contains(name)) {
        return values.at(name);
    }
    if (parent != nullptr) {
        return parent->value(name);
    }
    fatal("Variable `{}` not found");
}

void Scope::reassign(std::wstring const &name, Value v)
{
    if (values.contains(name)) {
        values[name] = v;
        return;
    }
    if (parent != nullptr) {
        parent->reassign(name, std::move(v));
        return;
    }
}

pSyntaxNode Scope::name(std::wstring const &name) const
{
    assert(owner->ns != nullptr);
    return owner->ns->find_name(name);
}

Interpreter::Interpreter()
{
    scopes.reserve(1024);
}

Scope &Interpreter::execute(pSyntaxNode const &node)
{
    Scope &root = (scopes.empty()) ? new_scope(node) : scopes.back();
    assert(root.owner == node);
    root.execute(node);
    return root;
}

Scope &Interpreter::new_scope(pSyntaxNode const &owner)
{
    if (scopes.size() >= scopes.capacity()) {
        fatal("Stack overflow");
    }
    if (scopes.empty()) {
        scopes.emplace_back(this, nullptr, owner);
    } else {
        scopes.emplace_back(this, &scopes.back(), owner);
    }
    return scopes.back();
}

}
