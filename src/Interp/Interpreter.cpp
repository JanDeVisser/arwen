/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <concepts>
#include <memory>
#include <ranges>

#include <Util/Defer.h>
#include <Util/Logging.h>
#include <Util/TokenLocation.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <Interp/Interpreter.h>
#include <Interp/Native.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;

template<typename T>
    requires std::derived_from<T, SyntaxNode>
void execute_node(Scope &scope, std::shared_ptr<T> const &)
{
    trace("execute_node({})", typeid(T).name());
    scope.push_back(make_void());
}

template<>
void execute_node(Scope &scope, std::shared_ptr<BinaryExpression> const &node)
{
    trace("Operator `{}`", Operator_name(node->op));
    if (node->op == Operator::Assign) {
        auto path = std::dynamic_pointer_cast<MemberPath>(node->lhs);
        assert(path != nullptr);
        if (path->path.size() > 1) {
            fatal("Can't do structs yet");
        }
        auto ident = path->path[0];
        scope.execute(node->rhs);
        Value const &rhs = scope.back();
        scope.reassign(ident->identifier, rhs);
        scope.pop_back();
        return;
    }
    if (node->op == Operator::Cast) {
        scope.execute(node->lhs);
        auto const lhs = scope.back();
        auto const rhs_type = std::dynamic_pointer_cast<TypeSpecification>(node->rhs);
        assert(rhs_type != nullptr && rhs_type->bound_type != nullptr);
        if (auto const coerced_maybe = lhs.coerce(rhs_type->bound_type); coerced_maybe) {
            scope.pop_back(1);
            scope.push_back(coerced_maybe.value());
            return;
        }
        fatal(L"Could not convert value of type `{}` to `{}`", lhs.type->to_string(), rhs_type->bound_type->to_string());
    }
    scope.execute(node->lhs);
    scope.execute(node->rhs);
    auto const &rhs = scope.get(-1);
    auto const &lhs = scope.get(-2);
    auto        res = evaluate(lhs, node->op, rhs);
    scope.pop_back(2);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Block> const &node)
{
    Scope block_scope { scope.interpreter, node, &scope };
    bool  first { true };
    for (auto const &stmt : node->statements) {
        if (!first) {
            block_scope.pop_back();
        }
        first = false;
        block_scope.execute(stmt);
    }
    for (auto const &defer : std::views::reverse(node->deferred_statements)) {
        block_scope.execute(defer);
        block_scope.pop_back();
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Call> const &node)
{
    if (auto const &extern_link = std::dynamic_pointer_cast<ExternLink>(node->function->implementation); extern_link != nullptr) {
        std::vector<Value> native_args;
        for (auto const &expression : node->arguments->expressions) {
            scope.execute(expression);
            native_args.emplace_back(scope.back());
            scope.pop_back();
        }
        if (auto const ret = native_call(as_utf8(extern_link->link_name), native_args, node->bound_type); ret) {
            scope.push_back(*ret);
            return;
        }
        fatal(L"Error executing native function `{}`", extern_link->link_name);
    }
    Value ret {};
    {
        Scope param_scope { scope.interpreter, node->function, &scope };
        for (auto const &expression : std::ranges::reverse_view(node->arguments->expressions)) {
            scope.execute(expression);
        }
        for (auto const &param_def : node->function->declaration->parameters) {
            param_scope.set(param_scope.values[param_def->name], scope.back());
            scope.pop_back();
        }
        trace(L"Executing function `{}`", node->function->name);
        param_scope.execute(node->function->implementation);
        ret = param_scope.back();
    }
    return scope.push_back(ret);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Constant> const &node)
{
    assert(node->bound_value.has_value());
    scope.push_back(*node->bound_value);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Identifier> const &node)
{
    trace(L"Identifier = `{}`", node->identifier);
    scope.push_back(scope.value(node->identifier));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<IfStatement> const &node)
{
    scope.execute(node->condition);
    Value res = scope.back();
    assert(res.type == TypeRegistry::boolean);
    auto const cond = as<bool>(res);
    scope.pop_back();
    if (cond) {
        scope.execute(node->if_branch);
    } else if (node->else_branch != nullptr) {
        scope.execute(node->else_branch);
    } else {
        scope.push_back(make_void());
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<LoopStatement> const &node)
{
    Value ret = make_void();
    while (true) {
        scope.execute(node->statement);
        if (scope.interpreter->break_) {
            if (scope.interpreter->break_->empty() || scope.interpreter->break_ == node->label) {
                scope.interpreter->break_.reset();
            }
            break;
        }
        if (scope.interpreter->continue_) {
            if (scope.interpreter->continue_->empty() || scope.interpreter->continue_ == node->label) {
                scope.interpreter->continue_.reset();
            } else {
                break;
            }
        }
        scope.pop_back();
    }
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Module> const &node)
{
    scope.execute(node->statements);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Program> const &node)
{
    for (auto &[_, mod] : node->modules) {
        scope.execute(mod);
    }
    Value ret = make_void();
    for (auto &[_, mod] : node->modules) {
        if (auto main = mod->ns->find_function(L"main", TypeRegistry::the().function_of(std::vector<pType> {}, TypeRegistry::i32)); main != nullptr) {
            auto call_expr = make_node<Call>(
                TokenLocation {},
                make_node<Identifier>(TokenLocation {}, L"main"),
                make_node<ExpressionList>(TokenLocation {}, SyntaxNodes {}));
            Scope mod_scope { scope.interpreter, call_expr, &scope };
            mod_scope.execute(call_expr);
            ret = mod_scope.back();
            mod_scope.pop_back();
            break;
        }
    }
    scope.push_back(ret);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<Return> const &node)
{
    scope.execute(node->expression);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<UnaryExpression> const &node)
{
    if (node->op == Operator::AddressOf) {
        auto const path = std::dynamic_pointer_cast<MemberPath>(node->operand);
        assert(path != nullptr);
        if (path->path.size() > 1) {
            fatal("Can't do structs yet");
        }
        auto const &ident = path->path[0];
        scope.push_back(scope.ptr_to(scope.ref_of(ident->identifier)));
        return;
    }
    if (node->op == Operator::Sizeof && node->operand->type == SyntaxNodeType::TypeSpecification) {
        scope.push_back(Value { TypeRegistry::i64, node->operand->bound_type->size_of() });
        return;
    }
    scope.execute(node->operand);
    auto const ret = evaluate(node->op, scope.back());
    scope.pop_back();
    scope.push_back(ret);
}

template<>
void execute_node(Scope &scope, std::shared_ptr<VariableDeclaration> const &node)
{
    Value init_value;
    if (node->initializer != nullptr) {
        scope.execute(node->initializer);
        init_value = scope.back();
        scope.pop_back();
    } else {
        init_value = make_value(node->bound_type);
    }
    scope.reassign(node->name, init_value);
    scope.push_back(scope.ref_of(node->name));
}

template<>
void execute_node(Scope &scope, std::shared_ptr<WhileStatement> const &node)
{
    Value ret = make_void();
    while (true) {
        scope.execute(node->condition);
        Value res = scope.back();
        assert(res.type == TypeRegistry::boolean);
        auto cond = as<bool>(res);
        scope.pop_back();
        if (!cond) {
            scope.push_back(ret);
            break;
        }

        scope.execute(node->statement);
        ret = scope.back();
        scope.pop_back();
        if (scope.interpreter->break_) {
            if (scope.interpreter->break_->empty() || scope.interpreter->break_ == node->label) {
                scope.interpreter->break_.reset();
                scope.push_back(ret);
            }
            break;
        }
        if (scope.interpreter->continue_) {
            if (scope.interpreter->continue_->empty() || scope.interpreter->continue_ == node->label) {
                scope.interpreter->break_.reset();
                continue;
            }
            break;
        }
    }
}

Scope::Scope(Interpreter *interpreter, pSyntaxNode owner, Scope *parent)
    : interpreter(interpreter)
    , parent(parent)
    , owner(std::move(owner))
    , bp(parent ? parent->bp : 0)
{
    allocate();
}

Scope::~Scope()
{
    release();
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
    case SyntaxNodeType::T: {                                    \
        trace("execute_node(" #T ")");                           \
        execute_node(*this, std::dynamic_pointer_cast<T>(node)); \
    } break;
        SyntaxNodeTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

void Scope::execute_block(std::shared_ptr<Block> const &block)
{
    bool  first { true };
    for (auto const &stmt : block->statements) {
        if (!first) {
            pop_back();
        }
        first = false;
        execute(stmt);
    }
    for (auto const &defer : std::views::reverse(block->deferred_statements)) {
        execute(defer);
        pop_back();
    }

}


ValueReference Scope::ref_of(std::wstring const &name) const
{
    if (values.contains(name)) {
        trace(L"ref_of({}) -> {}", name, values.at(name));
        return values.at(name);
    }
    if (parent != nullptr) {
        return parent->ref_of(name);
    }
    fatal("Variable `{}` not found");
}

Value Scope::ptr_to(ValueReference const reference) const
{
    Value &v = get(reference);
    return Value { TypeRegistry::the().referencing(v.type), as_ptr(&v) };
}

Value &Scope::value(std::wstring const &name) const
{
    return get(ref_of(name));
}

void Scope::add_value(std::wstring const &name, Value const &value)
{
    values.try_emplace(name, interpreter->stack.size());
    push(interpreter->stack, value, name);
}

void Scope::reassign(std::wstring const &name, Value const &v)
{
    if (values.contains(name)) {
        set(values[name], v);
        return;
    }
    if (parent != nullptr) {
        parent->reassign(name, v);
    }
}

pSyntaxNode Scope::name(std::wstring const &name) const
{
    assert(owner->ns != nullptr);
    return owner->ns->find_variable(name);
}

void Scope::allocate()
{
    uint64_t const old_bp = bp;
    bp = interpreter->stack.size();
    push(interpreter->stack, Value { old_bp }, L"old_bp ");
    for (auto const &name : owner->ns->variables | std::views::keys) {
        values.try_emplace(name, interpreter->stack.size());
        push(interpreter->stack, Value {}, name, false);
    }
    std::wcout << L"--- allocate\n";
    for (auto const &[name, ix] : values) {
        std::wcout << name << " = " << ix << std::endl;
    }
    dump(interpreter->stack);
}

void Scope::release()
{
    auto const new_bp = as<uint64_t>(interpreter->stack.get(static_cast<ValueReference>(bp)));
    interpreter->stack.stack.erase(interpreter->stack.stack.begin() + static_cast<ssize_t>(bp), interpreter->stack.stack.end());
    bp = new_bp;
}

void Scope::push_back(Value const &val) const
{
    interpreter->stack.push(val);
}

void Scope::push_back(ValueReference const ref) const
{
    interpreter->stack.push(ref);
}

void Scope::set(ValueReference const ref, Value const &val) const
{
    interpreter->stack.set(ref, val);
}

Value &Scope::back() const
{
    return interpreter->stack.get(-1);
}

Value &Scope::get(ValueReference const &ref) const
{
    return interpreter->stack.get(ref);
}

void Scope::pop_back(int const count) const
{
    interpreter->stack.pop_back(count);
}

}
