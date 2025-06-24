/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "App/Value.h"
#include <cstdint>
#include <memory>
#include <ranges>
#include <variant>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

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

Scope &Interpreter::new_scope(IRNode const &ir, uint64_t variables)
{
    trace("new_scope");
    if (scopes.empty()) {
        scopes.emplace_back(this, variables, ir);
    } else {
        auto parent = std::visit(
            overloads {
                [this](IR::pProgram const &program) -> std::optional<uint64_t> {
                    UNREACHABLE();
                },
                [this](IR::pModule const &module) -> std::optional<uint64_t> {
                    if (auto *program_scope { &scopes[0] }; std::holds_alternative<pProgram>(program_scope->ir)) {
                        return 0;
                    }
                    return {};
                },
                [this](IR::pFunction const &function) -> std::optional<uint64_t> {
                    for (auto ix = 0; ix < scopes.size(); ++ix) {
                        auto &s { scopes[ix] };
                        if (std::holds_alternative<IR::pModule>(s.ir)) {
                            if (auto const &mod = std::get<IR::pModule>(s.ir); mod == function->module) {
                                return ix;
                            }
                        }
                    }
                    UNREACHABLE();
                },
                [this](auto const &ir) -> std::optional<uint64_t> {
                    return scopes.size() - 1;
                },
            },
            ir);
        scopes.emplace_back(this, variables, ir, parent);
    }
    scopes.back().allocate();
    return scopes.back();
}

void Interpreter::drop_scope()
{
    scopes.back().release();
    scopes.pop_back();
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

    std::map<uint64_t, uint64_t> labels;
    uint64_t                     ip = 0;
    for (auto const &op : operations) {
        if (std::holds_alternative<Operation::Label>(op.op)) {
            auto const [lbl] = std::get<Operation::Label>(op.op);
            labels[lbl] = ip;
        }
        ++ip;
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
            [this, &ir](pProgram const &program) -> Value {
                trace(L"Running program {}", program->name);
                new_scope(ir, 0);
                call_stack.emplace_back(ir, 0);
                IR::pFunction main { nullptr };
                for (auto const &mod : program->modules | std::views::values) {
                    execute(mod);
                    if (main == nullptr && mod->functions.contains(L"main")) {
                        main = mod->functions[L"main"];
                    }
                }
                if (main != nullptr) {
                    Scope &param_scope = new_scope(main, 0);
                    call_stack.emplace_back(main, 0);
                    auto ret { execute(main) };
                    call_stack.pop_back();
                    drop_scope();
                    return ret;
                }
                return make_void();
            },
            [this, &ir](IR::pModule const &mod) -> Value {
                trace(L"Initializing module {}", mod->name);
                new_scope(ir, 0);
                call_stack.emplace_back(ir, 0);
                execute_operations(ir);
                auto ret { stack.get(-1) };
                stack.pop_back();
                return ret;
            },
            [this, &ir](pFunction const &n) -> Value {
                trace(L"Executing function {}", n->name);
                execute_operations(ir);
                auto ret { stack.get(-1) };
                stack.pop_back();
                return ret;
            } },
        ir);
}

Value execute_ir(IRNode const &ir)
{
    Interpreter interpreter;
    return interpreter.execute(ir);
}

}
