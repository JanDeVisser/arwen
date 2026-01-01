/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ranges>

#include <Util/Align.h>

#include <App/Parser.h>
#include <App/QBE/Native.h>
#include <App/QBE/QBE.h>

namespace Arwen::QBE {

using namespace Util;
using namespace Arwen;

pType type_from_qbe_type(ILBaseType const &t)
{
    switch (t) {
    case ILBaseType::V:
        return TypeRegistry::void_;
    case ILBaseType::B:
    case ILBaseType::SB:
        return TypeRegistry::i8;
    case ILBaseType::UB:
        return TypeRegistry::u8;
    case ILBaseType::H:
    case ILBaseType::SH:
        return TypeRegistry::i16;
    case ILBaseType::UH:
        return TypeRegistry::u16;
    case ILBaseType::W:
    case ILBaseType::SW:
        return TypeRegistry::i32;
    case ILBaseType::UW:
        return TypeRegistry::u32;
    case ILBaseType::L:
        return TypeRegistry::i64;
    case ILBaseType::S:
        return TypeRegistry::f32;
    case ILBaseType::D:
        return TypeRegistry::f64;
    default:
        UNREACHABLE();
    }
}

pType type_from_qbe_type(std::wstring const &t)
{
    trace(L"type_from_qbe_type `{}`", t);
    if (t == L"slice_t") {
        return TypeRegistry::string;
    }
    UNREACHABLE();
}

pType type_from_qbe_type(ILType t)
{
    return std::visit(
        [](auto const &inner) -> pType {
            return type_from_qbe_type(inner);
        },
        t);
}

bool is_pointer(ILValue const &value)
{
    return std::holds_alternative<ILBaseType>(value.type)
        && (std::get<ILBaseType>(value.type) == ILBaseType::L);
}

void *Frame::allocate(size_t bytes, size_t alignment)
{
    intptr_t ptr { alignat(stack_pointer, alignment) };
    if (ptr + bytes > STACK_SIZE) {
        fatal("Stack overflow");
    }
    auto ret = stack.data() + ptr;
    stack_pointer = ptr + bytes;
    trace("Allocated {} bytes aligned at {}. New stack pointer {}", bytes, alignment, stack_pointer);
    return ret;
}

void *Frame::allocate(pType const &type)
{
    if (type->size_of() == 0) {
        return nullptr;
    }
    trace(L"Allocating `{}`, size {}, align {}", type->to_string(), type->size_of(), type->align_of());
    return allocate(type->size_of(), type->align_of());
}

void Frame::release(void *ptr)
{
    auto const *p = static_cast<uint8_t *>(ptr);
    if (p < stack.data()) {
        fatal("Stack underflow");
    }
    if (p - stack.data() > stack_pointer) {
        fatal("Releasing unallocated stack space");
    }
    auto bytes = stack_pointer - (p - stack.data());
    stack_pointer = p - stack.data();
    trace("Released {} bytes. New stack pointer {}", bytes, stack_pointer);
}

void assign(pFrame const &frame, ILValue const &val_ref, Value const &v)
{
    if (v.type->size_of() == 0) {
        return;
    }
    std::visit(
        overloads {
            [frame, &v](Local const &local) {
                if (frame->locals.size() < local.var + 1) {
                    frame->locals.resize(local.var + 1);
                }
                trace(L"{} -> %v{}", v.to_string(), local.var);
                frame->locals[local.var] = v;
            },
            [frame, &v](ILValue::Variable const &variable) {
                trace(L"{} -> %{}$", v.to_string(), variable.name);
                frame->variables[variable.name] = v;
            },
            [](auto const &inner) {
                UNREACHABLE();
            } },
        val_ref.inner);
}

Value get(pFrame const &frame, ILValue const &val_ref)
{
    return std::visit(
        overloads {
            [&frame](Local const &local) -> Value {
                if (frame->locals.size() < local.var + 1) {
                    fatal("No local value with id `{}`in frame", local.var);
                }
                auto v = frame->locals[local.var];
                trace(L"{} <- %v{}", v.to_string(), local.var);
                return frame->locals[local.var];
            },
            [&frame](ILValue::Variable const &variable) -> Value {
                if (frame->variables.contains(variable.name)) {
                    auto v = frame->variables[variable.name];
                    trace(L"{} <- %{}$", v.to_string(), variable.name);
                    return v;
                }
                fatal(L"No variable with name `{}` in frame", variable.name);
                return {};
            },
            [&frame](ILValue::Global const &global) -> Value {
                auto const &vm = *(frame.repo);
                auto const &globals = vm.globals[frame->file.id];
                if (globals.contains(global.name)) {
                    return globals.at(global.name);
                }
                fatal(L"No global with name `{}`", global.name);
                return {};
            },
            [](int64_t const &int_val) -> Value {
                Value v { int_val };
                trace(L"{} <- int {}", v.to_string(), int_val);
                return v;
            },
            [](double const &dbl_val) -> Value {
                return { dbl_val };
            },
            [](auto const &inner) -> Value {
                UNREACHABLE();
            } },
        val_ref.inner);
}

void store(pFrame const &frame, pType type, void *target, ILValue const &src_ref)
{
    Value src = get(frame, src_ref);
    if (type == TypeRegistry::string) {
        src = make_from_buffer(type, as<void *>(src));
    }
    void *src_ptr = address_of(src);
    assert(src_ptr != nullptr);
    trace(L"memcpy({}, {} @{}, {})", target, src, src_ptr, type->to_string());
    memcpy(target, src_ptr, type->size_of());
}

VM::VM(ILProgram const &program)
    : program(program)
{
    globals.resize(program.files.size());
}

size_t VM::size() const
{
    return frames.size();
}

bool VM::empty() const
{
    return frames.empty();
}

pFrame VM::new_frame(ILFile const &file, ILFunction const &function)
{
    frames.emplace_back(*this, file, function);
    return { this };
}

void VM::release_frame()
{
    frames.pop_back();
}

Frame const &VM::operator[](size_t ix) const
{
    return frames[ix];
}

Frame &VM::operator[](size_t ix)
{
    return frames[ix];
}

using pFrame = Ptr<Frame, VM>;

struct ExecError {
    std::wstring message;
};

using ExecTermination = std::variant<Value, ExecError>;
using ExecResult = std::expected<void, ExecTermination>;

template<typename InstrDef>
ExecResult execute(ILFunction const &il, pFrame const &frame, InstrDef const &instruction)
{
    NYI("Unimplemented execute() for {}", typeid(InstrDef).name());
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, AllocDef const &instruction)
{
    void *ptr = frame->allocate(instruction.bytes, instruction.alignment);
    assign(frame, instruction.target, ptr);
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, BlitDef const &instruction)
{
    assert(is_pointer(instruction.src) && is_pointer(instruction.dest));
    auto src = static_cast<uint8_t *>(as<void *>(get(frame, instruction.src)));
    auto dest = static_cast<uint8_t *>(as<void *>(get(frame, instruction.dest)));
    for (auto ix = 0; ix < instruction.bytes; ++ix) {
        *(uint8_t *) (dest + ix) = *(uint8_t *) (src + ix);
    }
    ++frame->ip;
    return {};
}

ExecResult native_call(ILFunction const &il, pFrame const &frame, CallDef const &instruction)
{
    intptr_t           depth { 0 };
    std::vector<pType> types;
    for (auto const &arg : instruction.args) {
        auto type = type_from_qbe_type(arg.type);
        depth += alignat(type->size_of(), 8);
        types.push_back(type);
    }
    auto  return_type = type_from_qbe_type(instruction.target.type);
    void *args = frame->allocate(depth, 8);
    for (char *ptr = static_cast<char *>(args); auto const &arg : instruction.args) {
        auto type = type_from_qbe_type(arg.type);
        store(frame, type, ptr, arg);
        ptr += alignat(type->size_of(), 8);
    }
    void *ret_ptr = frame->allocate(return_type);
    trace(L"Native call: {} -> {}", instruction.name, return_type);
    if (native_call(as_utf8(instruction.name), args, types, ret_ptr, return_type)) {
        assign(frame, instruction.target, make_from_buffer(return_type, ret_ptr));
        frame->release(args);
        ++frame->ip;
        return {};
    }
    return std::unexpected(ExecError {});
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, CallDef const &instruction)
{
    if (auto colon = instruction.name.rfind(L':'); colon != std::wstring_view::npos) {
        return native_call(il, frame, instruction);
    }
    NYI("Can only execute native functions");
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, CopyDef const &instruction)
{
    assign(frame, instruction.target, get(frame, instruction.expr));
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, ExprDef const &instruction)
{
    auto lhs = get(frame, instruction.lhs);
    auto rhs = get(frame, instruction.rhs);

    Operator arwen_op;
    switch (instruction.op) {
#undef S
#define S(Op, Str, ArwenOp) \
    case ILOperation::Op:   \
        arwen_op = ArwenOp; \
        break;
        ILOPERATIONS(S)
#undef S
    default:
        UNREACHABLE();
    }
    assign(frame, instruction.target, evaluate(lhs, arwen_op, rhs));
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, JmpDef const &instruction)
{
    frame->ip = il.labels[instruction.label];
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, JnzDef const &instruction)
{
    auto expr = get(frame, instruction.expr);
    frame->ip = (as<bool>(expr))
        ? il.labels[instruction.on_true]
        : il.labels[instruction.on_false];
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, LabelDef const &instruction)
{
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, LoadDef const &instruction)
{
    auto type { type_from_qbe_type(instruction.target.type) };
    auto src = as<intptr_t>(get(frame, instruction.pointer));
    assign(frame, instruction.target, make_from_buffer(type, (void *) (src)));
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, RetDef const &instruction)
{
    if (instruction.expr) {
        return std::unexpected(get(frame, instruction.expr.value()));
    }
    return std::unexpected(Value {});
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, StoreDef const &instruction)
{
    auto type { type_from_qbe_type(instruction.target.type) };
    auto target = as<void *>(get(frame, instruction.target));
    store(frame, type, target, instruction.expr);
    ++frame->ip;
    return {};
}

ExecResult execute(ILFunction const &il, pFrame const &frame, ILInstruction const &instruction)
{
    return std::visit([&il, &frame](auto const &impl) -> ExecResult {
        trace("impl {}", typeid(impl).name());
        return execute(il, frame, impl);
    },
        instruction.impl);
}

ExecutionResult execute_qbe(VM &vm, ILFile const &file, ILFunction const &function, std::vector<Value> const &args)
{
    auto frame { vm.new_frame(file, function) };
    while (true) {
        trace(L"function `{}`[{}] ip {}", function.name, function.instructions.size(), frame->ip);
        if (auto res = execute(function, frame, function.instructions[frame->ip]); !res.has_value()) {
            return std::visit(
                overloads {
                    [](Value const &ret_value) -> ExecutionResult {
                        return ret_value;
                    },
                    [](ExecError const &error) -> ExecutionResult {
                        return std::unexpected(error.message);
                    } },
                res.error());
        }
        assert(frame->ip < function.instructions.size());
    }
}

ExecutionResult execute_qbe(VM &vm)
{
    for (auto const &[file_ix, file] : std::ranges::views::enumerate(vm.program.files)) {
        if (file.has_main) {
            for (auto const &function : file.functions) {
                if (function.name == L"main") {
                    trace("Setting globals");
                    for (auto const &[ix, s] : std::ranges::views::enumerate(file.strings)) {
                        trace(L"str_{} = {} @{}", ix + 1, s, (void *) s.data());
                        vm.globals[file_ix][std::format(L"str_{}", ix + 1)] = Value { (void *) s.data() };
                    }
                    for (auto const &[ix, s] : std::ranges::views::enumerate(file.cstrings)) {
                        trace("cstr_{} = {} @{}", ix + 1, s, (void *) s.data());
                        vm.globals[file_ix][std::format(L"cstr_{}", ix + 1)] = Value { (void *) s.data() };
                    }
                    return execute_qbe(vm, file, function, {});
                }
            }
        }
    }
    return L"No main function";
}

}
