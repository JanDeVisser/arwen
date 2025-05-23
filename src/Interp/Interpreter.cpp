/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <memory>
#include <ranges>

#include <Util/Logging.h>
#include <Util/TokenLocation.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <Interp/Interpreter.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

template<typename T>
    requires std::derived_from<T, SyntaxNode>
Value execute_node(Scope &scope, std::shared_ptr<T> const &node)
{
    // trace("execute_node({})", typeid(T).name());
    return make_void();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<BinaryExpression> const &node)
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
        Value rhs = scope.interpreter->stack.pop_value();
        scope.reassign(ident->identifier, rhs);
        return rhs;
    }
    scope.execute(node->lhs);
    scope.execute(node->rhs);
    auto rhs = scope.interpreter->stack.pop_value();
    auto lhs = scope.interpreter->stack.pop_value();
    return evaluate(lhs, node->op, rhs);
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Block> const &node)
{
    // trace("execute_node(Block)");
    Value ret = make_void();
    for (auto const &stmt : node->statements) {
        scope.execute(stmt);
        ret = scope.interpreter->stack.pop_value();
    }
    for (auto const &defer : std::views::reverse(node->deferred_statements)) {
        scope.execute(defer);
        scope.interpreter->stack.pop_value();
    }
    return ret;
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Call> const &node)
{
    for (auto const &expression : std::ranges::reverse_view(node->arguments->expressions)) {
        scope.execute(expression);
    }
    auto param_scope = scope.interpreter->new_scope(node->function);
    for (auto const &param_def : node->function->declaration->parameters) {
        param_scope.values.emplace(param_def->name, scope.interpreter->stack.pop_value());
    }
    param_scope.execute(node->function->implementation);
    return scope.interpreter->stack.pop_value();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Constant> const &node)
{
    // trace("execute_node(Decimal)");
    assert(node->bound_value.has_value());
    return node->bound_value.value();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Identifier> const &node)
{
    // trace("execute_node(Identifier)");
    return scope.value(node->identifier);
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<IfStatement> const &node)
{
    // trace("execute_node(IfStatement)");
    scope.execute(node->condition);
    Value res = scope.interpreter->stack.pop_value();
    assert(res.type == TypeRegistry::boolean);
    if (as<bool>(res)) {
        scope.execute(node->if_branch);
    } else if (node->else_branch != nullptr) {
        scope.execute(node->else_branch);
    } else {
        scope.interpreter->stack.push(make_void());
    }
    return scope.interpreter->stack.pop_value();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<LoopStatement> const &node)
{
    // trace("execute_node(LoopStatement)");
    Value ret = make_void();
    while (true) {
        scope.execute(node->statement);
        ret = scope.interpreter->stack.pop_value();
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
        }
    }
    return ret;
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Module> const &node)
{
    // trace("execute_node(Module)");
    scope.execute(node->statements);
    return scope.interpreter->stack.pop_value();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<Program> const &node)
{
    // trace("execute_node(Program)");
    for (auto &[_, mod] : node->modules) {
        scope.execute(mod);
    }
    for (auto &[_, mod] : node->modules) {
        if (auto main = mod->ns->find_function(L"main", TypeRegistry::the().function_of(std::vector<pType> {}, TypeRegistry::i32)); main != nullptr) {
            auto call_expr = make_node<BinaryExpression>(
                TokenLocation {},
                make_node<Identifier>(TokenLocation {}, L"main"),
                Operator::Call,
                make_node<ExpressionList>(TokenLocation {}, SyntaxNodes {}));
            Scope &mod_scope { scope.interpreter->new_scope(call_expr) };
            mod_scope.execute(call_expr);
            return scope.interpreter->stack.pop_value();
        }
    }
    return make_void();
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<UnaryExpression> const &node)
{
    // trace("execute_node(UnaryExpression)");
    scope.execute(node->operand);
    Value const operand = scope.interpreter->stack.pop_value();
    return evaluate(node->op, operand);
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<VariableDeclaration> const &node)
{
    // trace("execute_node(VariableDeclaration)");
    if (node->initializer != nullptr) {
        scope.execute(node->initializer);
        scope.values.emplace(node->name, scope.interpreter->stack.pop_value());
    } else {
        scope.values.emplace(node->name, Value { node->bound_type });
    }
    return scope.values.at(node->name);
}

template<>
Value execute_node(Scope &scope, std::shared_ptr<WhileStatement> const &node)
{
    // trace("execute_node(WhileStatement)");
    Value ret = make_void();
    while (true) {
        scope.execute(node->condition);
        Value res = scope.interpreter->stack.pop_value();
        assert(res.type == TypeRegistry::boolean);
        if (!as<bool>(res)) {
            break;
        }

        scope.execute(node->statement);
        ret = scope.interpreter->stack.pop_value();
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
        }
    }
    return ret;
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
#define S(T)                                                                \
    case SyntaxNodeType::T: {                                               \
        auto res = execute_node(*this, std::dynamic_pointer_cast<T>(node)); \
        interpreter->stack.push(res);                                       \
    }
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
    return owner->ns->find_variable(name);
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
