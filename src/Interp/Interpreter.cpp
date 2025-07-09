/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <memory>
#include <ranges>
#include <string>
#include <variant>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

#include <App/IR/IR.h>

#include <Interp/Interpreter.h>
#include <Interp/Native.h>

namespace Arwen::Interpreter {

using namespace Util;
using namespace Arwen;
using namespace Arwen::IR;

Scope &Interpreter::current_scope()
{
    assert(!scopes.empty());
    return scopes.back();
}

static void create_scope(Interpreter &interpreter, IRNode const &ir, std::vector<IRVariableDeclaration> const &variables)
{
    trace("create_scope");
    if (interpreter.scopes.empty()) {
        interpreter.scopes.emplace_back(interpreter, ir, variables);
    } else {
        auto parent = std::visit(
            overloads {
                [&interpreter](IR::pProgram const &program) -> std::optional<uint64_t> {
                    UNREACHABLE();
                },
                [&interpreter](IR::pModule const &module) -> std::optional<uint64_t> {
                    if (auto *program_scope { &interpreter.scopes[0] }; std::holds_alternative<pProgram>(program_scope->ir)) {
                        return 0;
                    }
                    return {};
                },
                [&interpreter](IR::pFunction const &function) -> std::optional<uint64_t> {
                    for (auto ix = 0; ix < interpreter.scopes.size(); ++ix) {
                        auto &s { interpreter.scopes[ix] };
                        if (std::holds_alternative<IR::pModule>(s.ir)) {
                            if (auto const &mod = std::get<IR::pModule>(s.ir); mod == function->module) {
                                return ix;
                            }
                        }
                    }
                    UNREACHABLE();
                },
                [&interpreter](auto const &ir) -> std::optional<uint64_t> {
                    return interpreter.scopes.size() - 1;
                },
            },
            ir);
        interpreter.scopes.emplace_back(interpreter, ir, variables, parent);
    }
}

Scope &Interpreter::new_scope(IRNode const &ir, std::vector<IRVariableDeclaration> const &variables)
{
    create_scope(*this, ir, variables);
    scopes.back().allocate();
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::AfterScopeStart, *this, std::monostate {});
    }
    return scopes.back();
}

Scope &Interpreter::emplace_scope(IRNode const &ir, std::vector<IRVariableDeclaration> const &variables)
{
    create_scope(*this, ir, variables);
    scopes.back().setup();
    return scopes.back();
}

void Interpreter::drop_scope(pType const &return_type)
{
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::OnScopeDrop, *this, return_type);
    }
    scopes.back().release(return_type);
    scopes.pop_back();
}

Value Interpreter::pop(pType const &type)
{
    auto offset = stack.top - alignat(type->size_of(), 8);
    auto ret = make_from_buffer(type, static_cast<void *>(stack.stack + offset));
    stack.discard(type->size_of());
    return ret;
}

void Interpreter::execute_operations(IRNode const &ir)
{
    // std::cerr << "Stack: (" << interpreter->stack.size() << "): ";
    // for (auto const &v : interpreter->stack) {s)
    //     std::wcerr << L"{ " << v.to_string() << "
    // }
    // std::cerr << "\n";
    auto const &operations = std::visit(
        overloads {
            [](std::monostate const &) -> std::vector<Operation> const & {
                fatal("Can't execute null IR Node");
            },
            [](pProgram const &) -> std::vector<Operation> const & {
                fatal("Can't execute program IR Node");
            },
            [](auto const &n) -> std::vector<Operation> const & {
                return n->operations;
            } },
        ir);

    if (!labels.contains(ir)) {
        uint64_t ip = 0;
        auto    &l { labels[ir] };
        for (auto const &op : operations) {
            if (std::holds_alternative<Operation::Label>(op.op)) {
                auto const [lbl] = std::get<Operation::Label>(op.op);
                l[lbl] = ip;
            }
            ++ip;
        }
    }
    while (call_stack.back().ip < operations.size()) {
        execute_op(operations[call_stack.back().ip], *this);
    }
}

Value Interpreter::execute(IR::IRNode const &ir)
{
    return std::visit(
        overloads {
            [](std::monostate const &) -> Value {
                fatal("Can't execute null IR Node");
            },
            [this](auto const &obj) -> Value {
                return execute_node(*this, obj);
            } },
        ir);
}

Value execute_node(Interpreter &interpreter, IR::pProgram const &program)
{
    trace(L"Running program {}", program->name);
    interpreter.new_scope(program);
    interpreter.call_stack.emplace_back(program, 0);
    IR::pFunction main { nullptr };
    for (auto const &mod : program->modules | std::views::values) {
        interpreter.execute(mod);
        if (main == nullptr && mod->functions.contains(L"main")) {
            main = mod->functions[L"main"];
        }
    }
    if (main != nullptr) {
        auto ret { execute_node(interpreter, main) };
        return ret;
    }
    return make_void();
}

Value execute_node(Interpreter &interpreter, IR::pModule const &module)
{
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::StartModule, interpreter, module);
    }
    interpreter.call_stack.emplace_back(module, 0);
    interpreter.new_scope(module, module->variables);
    interpreter.execute_operations(module);
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::EndModule, interpreter, module);
    }
    return interpreter.pop(module->syntax_node->bound_type);
}

Value execute_node(Interpreter &interpreter, IR::pFunction const &function)
{
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::StartFunction, interpreter, function);
    }
    Scope &param_scope = interpreter.new_scope(function);
    interpreter.call_stack.emplace_back(function, 0);
    interpreter.execute_operations(function);
    interpreter.call_stack.pop_back();
    interpreter.drop_scope(function->return_type);
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::EndFunction, interpreter, function);
    }
    return interpreter.pop(function->syntax_node->bound_type);
}

Value execute_ir(IRNode const &ir)
{
    Interpreter interpreter;
    return interpreter.execute(ir);
}

}
