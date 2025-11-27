/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <ranges>
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

static void create_scope(Interpreter &interpreter, pIR const &ir, Declarations const &variables)
{
    trace("create_scope");
    if (interpreter.scopes.empty()) {
        interpreter.scopes.emplace_back(interpreter, ir, variables);
    } else {
        auto parent = std::visit(
            overloads {
                [&interpreter](IR::Program const &program) -> std::optional<uint64_t> {
                    UNREACHABLE();
                },
                [&interpreter](IR::Module const &module) -> std::optional<uint64_t> {
                    if (auto *program_scope { &interpreter.scopes[0] }; is<IR::Program>(program_scope->ir)) {
                        return 0;
                    }
                    return {};
                },
                [&interpreter](IR::Function const &function) -> std::optional<uint64_t> {
                    for (auto ix = 0; ix < interpreter.scopes.size(); ++ix) {
                        auto &s { interpreter.scopes[ix] };
                        if (is<IR::Module>(s.ir)) {
                            if (s.ir == function.module) {
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
            ir->node);
        interpreter.scopes.emplace_back(interpreter, ir, variables, parent);
    }
}

Scope &Interpreter::new_scope(pIR const &ir, Declarations const &variables)
{
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::OnScopeStart, *this, std::monostate {});
    }
    create_scope(*this, ir, variables);
    scopes.back().allocate();
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::AfterScopeStart, *this, std::monostate {});
    }
    return scopes.back();
}

Scope &Interpreter::emplace_scope(pIR const &ir, Declarations const &variables)
{
    create_scope(*this, ir, variables);
    scopes.back().setup();
    return scopes.back();
}

void Interpreter::drop_scope()
{
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::OnScopeDrop, *this, std::monostate {});
    }
    scopes.back().release();
    scopes.pop_back();
    if (callback != nullptr) {
        callback(Interpreter::CallbackType::AfterScopeDrop, *this, std::monostate {});
    }
}

Value Interpreter::pop(pType const &type)
{
    trace(L"Interpreter::pop({})", type->to_string());
    auto aligned { alignat(type->size_of(), 8) };
    auto offset = stack.top - aligned;
    auto ret = make_from_buffer(type, static_cast<void *>(stack.stack + offset));
    stack.discard(aligned);
    return ret;
}

void Interpreter::move_in(void *ptr, size_t size, uint8_t reg)
{
    assert(reg < registers.size());
    auto num_regs { alignat(size, 8) / 8 };
    assert(reg + num_regs - 1 < registers.size());
    memcpy(registers.data() + reg, ptr, size);
}

uint64_t Interpreter::move_out(uint8_t reg)
{
    assert(reg < registers.size());
    return registers[reg];
}

Value Interpreter::move_out(pType const &type, uint8_t reg)
{
    trace(L"Interpreter::move_out({}, {})", reg, type->to_string());
    assert(reg < registers.size());
    auto num_regs { alignat(type->size_of(), 8) / 8 };
    assert(reg + num_regs - 1 < 19);
    auto ret = make_from_buffer(type, registers.data() + reg);
    trace(L"move_out() => {:t}", ret);
    return ret;
}

void Interpreter::execute_operations(pIR const &ir)
{
    // std::cerr << "Stack: (" << interpreter->stack.size() << "): ";
    // for (auto const &v : interpreter->stack) {s)
    //     std::wcerr << L"{ " << v.to_string() << "
    // }
    // std::cerr << "\n";

    trace(L"Executing {}", ir->name);
    if (!labels.contains(ir)) {
        uint64_t ip = 0;
        auto    &l { labels[ir] };
        for (auto const &op : ir->operations) {
            if (std::holds_alternative<Operation::Label>(op.op)) {
                auto const [lbl] = std::get<Operation::Label>(op.op);
                l[lbl] = ip;
            }
            ++ip;
        }
    }
    while (call_stack.back().ip < ir->operations.size()) {
        execute_op(ir->operations[call_stack.back().ip], ir, *this);
    }
}

Value execute_function(Interpreter &interpreter, pIR const &ir, IR::Function const &function)
{
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::StartFunction, interpreter, ir);
    }
    Scope &param_scope = interpreter.new_scope(ir);
    interpreter.call_stack.emplace_back(ir, 0);
    interpreter.execute_operations(ir);
    interpreter.call_stack.pop_back();
    interpreter.drop_scope();
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::EndFunction, interpreter, ir);
    }
    return interpreter.move_out(function.return_type);
}

Value execute_module(Interpreter &interpreter, pIR const &ir, IR::Module const &module)
{
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::StartModule, interpreter, ir);
    }
    interpreter.call_stack.emplace_back(ir, 0);
    interpreter.new_scope(ir, ir->variables);
    interpreter.execute_operations(ir);
    if (interpreter.callback != nullptr) {
        interpreter.callback(Interpreter::CallbackType::EndModule, interpreter, ir);
    }
    return interpreter.pop(ir->syntax_node->bound_type);
}

Value execute_program(Interpreter &interpreter, pIR const &ir, IR::Program const &program)
{
    trace(L"Running program {}", ir->name);
    interpreter.new_scope(ir);
    interpreter.call_stack.emplace_back(ir, 0);
    pIR main { nullptr };
    for (auto const &mod : program.modules | std::views::values) {
        interpreter.execute(mod);
        auto &m = get<IR::Module>(mod);
        if (main == nullptr && m.functions.contains(L"main")) {
            main = m.functions[L"main"];
            break;
        }
    }
    if (main != nullptr) {
        auto ret { execute_function(interpreter, main, get<Function>(main)) };
        return ret;
    }
    return make_void();
}

Value Interpreter::execute(pIR const &ir)
{
    assert(ir != nullptr);
    return std::visit(
        overloads {
            [this, &ir](IR::Program const &obj) -> Value {
                return execute_program(*this, ir, obj);
            },
            [this, &ir](IR::Module const &obj) -> Value {
                return execute_module(*this, ir, obj);
            },
            [this, &ir](IR::Function const &obj) -> Value {
                return execute_function(*this, ir, obj);
            } },
        ir->node);
}

Value execute_ir(IRNodes const &ir)
{
    Interpreter interpreter;
    return interpreter.execute(ir.program);
}

}
