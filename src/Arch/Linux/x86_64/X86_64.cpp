/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Pipe.h>
#include <Util/Process.h>
#include <Util/Utf8.h>

#include <App/Config.h>
#include <App/Operator.h>
#include <App/Type.h>

#include <App/IR/IR.h>

#include <Arch/AssemblyWriter.h>
#include <Arch/Linux/x86_64/X86_64.h>

namespace Arwen::X86_64 {

namespace fs = std::filesystem;

#undef S
#define S(R, N64, N32, N16) Register Register::R { Reg::R };
REGISTERS(S)
#undef S

std::array<Register, 6> param_regs = { Register::rdi, Register::rsi, Register::rdx, Register::rcx, Register::r8, Register::r9 };
std::array<Register, 6> scratch_regs = { Register::r10, Register::r11, Register::r12, Register::r13, Register::r14, Register::r15 };
std::array<Register, 5> regs_to_be_saved = { Register::rbx, Register::r12, Register::r13, Register::r14, Register::r15 };
std::array<Register, 4> alu_regs = { Register::rax, Register::rbx, Register::rcx, Register::rdx };

void Function::analyze(std::vector<IR::Operation> const &operations)
{
    if (function) {
        for (auto const &[name, type] : get<IR::Function>(function).parameters) {
            stack_depth += alignat(type->size_of(), 16);
            variables[name] = stack_depth;
        }
    }

    std::vector<uint64_t> depths;
    auto                  depth = stack_depth;
    for (auto const &op : operations) {
        std::visit(
            overloads {
                [this, &depth, &depths](IR::Operation::ScopeBegin const &impl) -> void {
                    depths.emplace_back(depth);
                    for (auto const &[name, type] : impl.payload) {
                        depth += alignat(type->size_of(), 16);
                        variables[name] = depth;
                    }
                    stack_depth = std::max(stack_depth, depth);
                },
                [&depth, &depths](IR::Operation::ScopeEnd const &impl) -> void {
                    depth = depths.back();
                    depths.pop_back();
                },
                [](auto const &impl) -> void {
                } },
            op.op);
    }
}

void push_to_stack(Function &function, size_t size, Register from_reg)
{
    if (size == 0) {
        return;
    }
    auto num_regs { words_needed(size) };

    for (int ix = 0; ix < num_regs; ++ix) {
        function.writer.add_instruction(L"pushq", L"%{}", from_reg + ix);
    }
}

void pop_from_stack(Function &function, size_t size, Registers const &to_regs)
{
    if (to_regs.empty()) {
        return;
    }
    assert(to_regs.size() == words_needed(size));
    for (auto reg : to_regs) {
        function.writer.add_instruction(L"popq", L"%{}", reg);
    }
}

int move_into_stack(Function &function, size_t size, Registers const &regs, uint64_t to_pos)
{
    if (size == 0) {
        return 0;
    }
    assert(regs.size() == words_needed(size));
    auto ix = 0;
    for (auto reg : regs) {
        function.writer.add_instruction(L"movq", L"{}(%rbp),{}", to_pos + ix * 8, reg);
    }
    return regs.size();
}

void Function::skeleton()
{
    writer.activate_prolog();
    writer.add_label(name);
    writer.add_label(std::format(L"_{}", name));
    if (!naked) {
        writer.add_instruction(L"pushq", L"%rbp");
        writer.add_instruction(L"movq", L"%rsp,%rbp");
        if (stack_depth > 0) {
            writer.add_instruction(L"subq", L"${}, %rsp", stack_depth);
        }
        writer.add_instruction(L"movq", L"$0,-{}(%rbp)", Function::scope_depth_index);
        writer.add_instruction(L"movq", L"$0,-{}(%rbp)", Function::return_value_index);
    }
    if (function) {
        int reg_ix = 0;
        for (auto const &[name, type] : get<IR::Function>(function).parameters) {
            auto num_regs { words_needed(type->size_of()) };
            for (int ix = 0; ix < num_regs; ++ix) {
                writer.add_instruction(L"movq", L"-{}(%rbp),%{}", variables[name] + ix * 8, param_regs[reg_ix++]);
            }
        }
    }
    writer.activate_epilog();
    writer.add_instruction(L"movq", L"-{}(%rbp),%rax", Function::return_value_index);
    if (!naked) {
        writer.add_instruction(L"movq", L"%rbp,%rsp");
        writer.add_instruction(L"popq", L"%rbp");
    }
    writer.add_instruction(L"ret");
    writer.activate_code();
}

void debug_stack(Function &function, std::string prefix)
{
    // std::string c { std::format("{} | [{}] ", prefix, function.stack.size()) };
    // for (auto const &e : function.stack | std::views::reverse) {
    //     std::visit(
    //         overloads {
    //             [&c](RegisterAllocation const &alloc) {
    //                 c += std::format("{}/{} ", alloc.reg, alloc.num_regs);
    //             },
    //             [&c](VarPointer const &ptr) {
    //                 c += std::format("VarPointer {} ", ptr);
    //             } },
    //         e);
    // }
    // std::cout << c << std::endl;
}

ValueStackEntry Function::push_reg(pType const &type)
{
    return push_reg(type->size_of());
}

ValueStackEntry Function::push_reg(size_t size)
{
    auto      num { words_needed(size) };
    Registers ret {};

    for (auto r : scratch_regs) {
        if (!regs[r]) {
            ret.push_back(r);
            save_regs[r] = true;
            if (ret.size() >= num) {
                break;
            }
        }
    }
    if (ret.size() == num) {
        for (auto r : ret) {
            save_regs[r] = true;
        }
        stack.emplace_back(ret);
        return ret;
    }
    stack.emplace_back(StackAllocation { size });
    return stack.back();
}

ValueStackEntry Function::pop_reg(pType const &type)
{
    return pop_reg(type->size_of());
}

ValueStackEntry Function::pop_reg(size_t size)
{
    assert(!stack.empty());
    ValueStackEntry const ret = stack.back();
    if (std::holds_alternative<Registers>(ret)) {
        auto registers { std::get<Registers>(ret) };
        for (auto r : registers) {
            regs[r] = false;
        }
    }
    stack.pop_back();
    // debug_stack(*this, std::format("pop_reg({}) {}/{}", size, ret.reg, ret.num_regs));
    return ret;
}

// Pushes the value currently in x0... to the register stack
void Function::push(pType const &type)
{
    if (type == TypeRegistry::void_) {
        return;
    }
    push(type->size_of());
}

void Function::push(size_t size)
{
    if (size == 0) {
        return;
    }
    auto w = words_needed(size);
    assert(w <= 4);
    std::visit(
        overloads {
            [this](Registers const &dest) -> void {
                auto ix = 0;
                for (auto r : dest) {
                    writer.add_instruction(L"movq", L"%{},%{}", alu_regs[ix], r);
                    ++ix;
                }
            },
            [this, w](StackAllocation const &alloc) -> void {
                assert(w == words_needed(alloc.size));
                auto ix = w;
                while (ix > 0) {
                    writer.add_instruction(L"pushq", L"%{}", alu_regs[ix]);
                    --ix;
                }
            },
            [](auto) {
                UNREACHABLE();
            } },
        push_reg(size));
}

// Pops the value currently at the top of the value stack to x0..
void Function::pop(pType const &type)
{
    if (type != TypeRegistry::void_) {
        pop(type->size_of());
    }
}

void Function::pop(size_t size)
{
    if (size > 0) {
        pop(size,
            Registers {
                std::from_range,
                alu_regs | std::ranges::views::take(words_needed(size)) });
    }
}

void Function::pop(pType const &type, Register target)
{
    if (type != TypeRegistry::void_) {
        pop(type->size_of(), Registers { target });
    }
}

void Function::pop(size_t size, Register target)
{
    if (size > 0) {
        pop(size, Registers { target });
    }
}

void Function::pop(pType const &type, Registers const &targets)
{
    if (type != TypeRegistry::void_) {
        pop(type->size_of(), targets);
    }
}

void Function::pop(size_t size, Registers const &targets)
{
    if (size == 0) {
        return;
    }
    assert(!stack.empty());
    auto w = words_needed(size);
    assert(targets.size() == w);
    ValueStackEntry alloc = pop_reg(size);
    std::visit(
        overloads {
            [this, &size, &targets](Registers const &srcs) {
                assert(srcs.size() == targets.size());
                for (auto [src, target] : std::ranges::views::zip(srcs, targets)) {
                    save_regs[target] = true;
                    writer.add_instruction(L"movq", L"%{},%{}", src, target);
                }
            },
            [this, &targets](StackAllocation const &alloc) {
                for (auto reg : targets) {
                    save_regs[reg] = true;
                    writer.add_instruction(L"popq", L"%{}", reg);
                }
            },
            [this, &size, &targets](VarPointer const &) {
                deref(size, targets);
            } },
        alloc);
}

// Pops the variable reference off the value stack and moves the value
// into x0...
VarPointer Function::deref(pType const &type, Registers const &targets)
{
    if (type == TypeRegistry::void_) {
        return 0;
    }
    return deref(type->size_of(), targets);
}

VarPointer Function::deref(pType const &type)
{
    if (type == TypeRegistry::void_) {
        return 0;
    }
    return deref(type->size_of());
}

VarPointer Function::deref(size_t size)
{
    if (size > 0) {
        return deref(size,
            Registers {
                std::from_range,
                alu_regs | std::ranges::views::take(words_needed(size)) });
    }
    return 0;
}

VarPointer Function::deref(size_t size, Registers const &targets)
{
    if (size == 0) {
        return 0;
    }
    assert(words_needed(size) == targets.size());
    assert(!stack.empty());
    assert(std::holds_alternative<VarPointer>(stack.back()));
    auto ptr { std::get<VarPointer>(stack.back()) };
    auto ret { ptr };
    stack.pop_back();
    // debug_stack(*this, std::format("deref({}, {}) VarPointer {}", size, target, ptr));
    auto num_regs { words_needed(size) };
    auto num { num_regs };
    while (num > 0) {
        writer.add_instruction(L"movq", L"-{}(%rbp%),%{}", ptr, targets[num_regs - num - 1]);
        ptr -= 8;
        ++num;
    }
    return ret;
}

// Pops the variable reference off the value stack, then pops an allocation
// and moves x0... into the variable
VarPointer Function::assign(pType const &type)
{
    if (type == TypeRegistry::void_) {
        return 0;
    }
    return assign(type->size_of());
}

VarPointer Function::assign(size_t size)
{
    if (size == 0) {
        return 0;
    }

    // Pop the variable reference:
    assert(!stack.empty());
    assert(std::holds_alternative<VarPointer>(stack.back()));
    auto ptr { std::get<VarPointer>(stack.back()) };
    auto ret { ptr };
    stack.pop_back();
    debug_stack(*this, std::format("assign({}) VarPointer {}", size, ptr));

    // Pop the top of the value stack into rax...
    pop(size, Registers {});

    // Move rax... into the variable:
    auto num_regs { words_needed(size) };
    auto num { num_regs };
    while (num > 0) {
        writer.add_instruction(L"movq", L"%{}, -{}(%rbp)", alu_regs[num_regs - num], ptr);
        --num;
        ptr += 8;
    }
    return ret;
}

void Object::add_directive(std::wstring_view const directive, std::wstring_view const args)
{
    prolog += std::format(L"{}\t{}\n", directive, args);
    if (directive == L".global") {
        prolog += std::format(L"{}\t{}\n", directive, std::format(L"_{}", args));
        has_exports = true;
        if (args == L"main") {
            has_main = true;
        }
    }
}

int Object::add_string(std::wstring_view const str)
{
    return add_string(std::wstring { str });
}

int Object::add_string(std::wstring const &str)
{
    if (strings.contains(str)) {
        return strings.at(str);
    }
    auto id = next_label++;
    text += std::format(L".align 2\nstr_{}:\n\t.byte\t", id);
    for (auto *ptr = str.data(); *ptr != 0; ++ptr) {
        auto c = *ptr;
        for (auto ix = 0; ix < sizeof(c); ++ix) {
            if (ptr != str.data() || ix > 0) {
                text += L",";
            }
            text += std::format(L"0x{:x}", c & 0xFF);
            c >>= 8;
        }
    }
    text += '\n';
    strings[str] = id;
    return id;
}

template<typename Impl>
void generate_op(Function &function, Impl const &impl)
{
}

template<>
void generate_op(Function &function, IR::Operation::AssignFromRef const &impl)
{
    // function.stack.push_back(function.assign(impl.payload));
    function.assign(impl.payload);
    debug_stack(function, "AssignFromRef");
}

template<>
void generate_op(Function &function, IR::Operation::AssignValue const &impl)
{
    // function.stack.push_back(function.assign(impl.payload));
    function.assign(impl.payload);
    debug_stack(function, "AssignValue");
}

template<>
void generate_op(Function &function, IR::Operation::BinaryOperator const &impl)
{
    generate_binop(function, impl.payload.lhs, impl.payload.op, impl.payload.rhs);
}

template<>
void generate_op(Function &function, IR::Operation::Break const &impl)
{
    function.writer.add_instruction(L"movq", L"${},-{}(%rbp)", impl.payload.depth.value_or(-1), Function::scope_depth_index);
    function.writer.add_instruction(L"jmp", L"lbl_{}", impl.payload.scope_end);
}

void generate_call(Function &function, IR::Operation::CallOp const &call)
{
    std::vector<Registers> allocations;
    int                    reg_ix = 0;
    for (auto const &[_, param_type] : call.parameters) {
        auto num_regs = words_needed(param_type->size_of());
        assert(reg_ix + num_regs <= param_regs.size());
        allocations.emplace_back(
            Registers {
                std::from_range,
                std::ranges::subrange(param_regs.begin() + reg_ix, param_regs.begin() + (reg_ix + num_regs)) });
        reg_ix += num_regs;
    }
    for (auto const &[dest, type] : std::views::zip(
                                        allocations,
                                        call.parameters
                                            | std::ranges::views::transform([](auto const &decl) {
                                                  return decl.type;
                                              }))
            | std::views::reverse) {
        function.pop(type, dest);
    }
    std::wstring name { call.name };
    if (auto colon = call.name.rfind(L':'); colon != std::wstring::npos) {
        name.erase(0, colon + 1);
    }
    function.writer.add_instruction(L"call", std::format(L"{}", name));
    function.push(call.return_type);
}

template<>
void generate_op(Function &function, IR::Operation::Call const &impl)
{
    generate_call(function, impl.payload);
}

void generate_op(Function &function, IR::Operation::Dereference const &impl)
{
    function.deref(impl.payload);
    function.push(impl.payload);
}

template<>
void generate_op(Function &function, IR::Operation::Discard const &impl)
{
    if (impl.payload == TypeRegistry::void_) {
        return;
    }
    while (!function.stack.empty()) {
        auto entry = function.pop_reg(impl.payload);
        if (auto *ptr = std::get_if<StackAllocation>(&entry); ptr != nullptr) {
            function.writer.add_instruction(L"add", L"%rsp,{}", words_needed(ptr->size * 8));
        }
    }
}

template<>
void generate_op(Function &function, IR::Operation::Jump const &impl)
{
    function.writer.add_instruction(L"jmp", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::JumpF const &impl)
{
    pop<bool>(function);
    function.writer.add_instruction(L"test", L"%rax");
    function.writer.add_instruction(L"je", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::JumpT const &impl)
{
    pop<bool>(function);
    function.writer.add_instruction(L"test", L"%rax");
    function.writer.add_instruction(L"jne", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::Label const &impl)
{
    function.writer.add_label(std::format(L"lbl_{}", impl.payload));
}

void generate_op(Function &function, IR::Operation::NativeCall const &impl)
{
    generate_call(function, impl.payload);
}

void generate_op(Function &function, IR::Operation::Pop const &impl)
{
    function.pop(impl.payload->size_of());
    function.writer.add_instruction(L"movq", L"%rax,-{}(%rbp)", Function::return_value_index);
    while (!function.stack.empty()) {
        auto entry = function.pop_reg(impl.payload);
        if (auto *ptr = std::get_if<StackAllocation>(&entry); ptr != nullptr) {
            function.writer.add_instruction(L"add", L"%rsp,{}", words_needed(ptr->size * 8));
        }
    }
}

template<>
void generate_op(Function &function, IR::Operation::PushConstant const &impl)
{
    if (impl.payload.type == TypeRegistry::void_) {
        return;
    }
    std::visit(
        overloads {
            [&function, &impl](IntType const &int_type) -> void {
                auto entry = function.push_reg(impl.payload.type);
                std::visit(
                    overloads {
                        [&function, int_type, &impl](Registers const &regs) {
                            auto reg = regs[0];
                            switch (int_type.width_bits) {
                            case 8:
                                function.writer.add_instruction(L"movq", L"${},%{}",
                                    static_cast<int>((int_type.is_signed) ? as<int8_t>(impl.payload) : as<uint8_t>(impl.payload)), reg);
                                break;
                            case 16:
                                function.writer.add_instruction(L"movq", L"${},%{}",
                                    (int_type.is_signed) ? as<int16_t>(impl.payload) : as<uint16_t>(impl.payload), reg);
                                break;
                            case 32:
                                function.writer.add_instruction(L"movq", L"${},%{}",
                                    (int_type.is_signed) ? as<int32_t>(impl.payload) : as<uint32_t>(impl.payload), reg);
                                break;
                            case 64:
                                function.writer.add_instruction(L"movq", L"${},%{}",
                                    (int_type.is_signed) ? as<int64_t>(impl.payload) : as<uint64_t>(impl.payload), reg);
                                break;
                            }
                        },
                        [&function, int_type, &impl](StackAllocation const &alloc) {
                            switch (int_type.width_bits) {
                            case 8:
                                function.writer.add_instruction(L"pushb", L"${}",
                                    static_cast<int>((int_type.is_signed) ? as<int8_t>(impl.payload) : as<uint8_t>(impl.payload)));
                                break;
                            case 16:
                                function.writer.add_instruction(L"pushw", L"${}",
                                    (int_type.is_signed) ? as<int16_t>(impl.payload) : as<uint16_t>(impl.payload));
                                break;
                            case 32:
                                function.writer.add_instruction(L"pushl", L"${}",
                                    (int_type.is_signed) ? as<int32_t>(impl.payload) : as<uint32_t>(impl.payload));
                                break;
                            case 64:
                                function.writer.add_instruction(L"movq", L"${},%rax",
                                    (int_type.is_signed) ? as<int64_t>(impl.payload) : as<uint64_t>(impl.payload));
                                function.writer.add_instruction(L"pushq", L"%rax");
                                break;
                            }
                        },
                        [](auto) {
                            UNREACHABLE();
                        } },
                    entry);
            },
            [&function, &impl](SliceType const &slice_type) -> void {
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(impl.payload);
                auto str_id = function.object.add_string(std::wstring_view { static_cast<wchar_t *>(slice.ptr), static_cast<size_t>(slice.size) });
                std::visit(
                    overloads {
                        [&function, str_id, slice](Registers const &regs) {
                            assert(!regs.empty());
                            function.writer.add_instruction(L"leaq", L"str_{}(%rip),%{}", str_id, regs[0]);
                            function.writer.add_instruction(L"movq", L"${},%{}", slice.size, regs[1]);
                        },
                        [&function, str_id, slice](StackAllocation const &alloc) {
                            function.writer.add_instruction(L"pushq", L"str_{}(%rip)", str_id);
                            function.writer.add_instruction(L"pushq", L"${}", slice.size);
                        },
                        [](auto) {
                            UNREACHABLE();
                        } },
                    function.push_reg(slice_type.size_of()));
            },
            [&function, &impl](auto const &) -> void {
                function.writer.add_comment(std::format(L"PushConstant {}", impl.payload.type->to_string()));
            } },
        impl.payload.type->description);
}

template<>
void generate_op(Function &function, IR::Operation::PushVarAddress const &impl)
{
    function.stack.push_back(function.variables[impl.payload.name] + impl.payload.offset);
    debug_stack(function, std::format("PushVarAddress {}+{}", as_utf8(impl.payload.name), impl.payload.offset));
}

template<>
void generate_op(Function &function, IR::Operation::ScopeBegin const &)
{
    function.writer.add_instruction(L"movq", L"$0,-{}(%rbp)", Function::scope_depth_index);
}

template<>
void generate_op(Function &function, IR::Operation::ScopeEnd const &impl)
{
    function.writer.add_text(std::format(
        LR"(
    xor %rax,%rax
    test -{}(%rbp),%rax
    je  1f
    jmp lbl_{}
1:)",
        Function::scope_depth_index,
        impl.payload.enclosing_end));
}

template<>
void generate_op(Function &function, IR::Operation::UnaryOperator const &impl)
{
    generate_unary(function, impl.payload.operand, impl.payload.op);
}

void Function::generate(std::vector<IR::Operation> const &operations)
{
    analyze(operations);
    object.add_directive(L".global", name);
    skeleton();
    regs.fill(false);
    save_regs.fill(false);
    for (auto const &op : operations) {
        std::visit(
            [this](auto const &impl) -> void {
                writer.add_comment(as_wstring(typeid(decltype(impl)).name()));
                trace("Generating x86_64 code for op {}", typeid(decltype(impl)).name());
                generate_op(*this, impl);
            },
            op.op);
    }
    std::wstring pop_saved_regs;
    writer.activate_prolog();
    for (auto s : regs_to_be_saved) {
        if (save_regs[s]) {
            writer.add_instruction(L"pushq", L"%{}", s);
            pop_saved_regs = std::format(L"popq %{}\n{}", s, pop_saved_regs);
        }
    }
    writer.activate_code();
    if (!pop_saved_regs.empty()) {
        writer.add_text(pop_saved_regs);
    }
}

std::wostream &operator<<(std::wostream &os, Function const &function)
{
    os << function.writer.prolog;
    os << function.writer.code;
    os << function.writer.epilog << std::endl;
    return os;
}

void Object::generate()
{
    for (auto const &[name, f] : get<IR::Module>(module).functions) {
        auto &function = functions.emplace_back(name, *this, f);
        function.generate(f->operations);
    }
    if (!module->operations.empty()) {
        auto &mod_init = functions.emplace_back(std::format(L"_{}_init", module->name), *this);
        mod_init.naked = true;
        mod_init.generate(module->operations);
    }
}

std::wostream &operator<<(std::wostream &os, Object const &object)
{
    if (!object.prolog.empty()) {
        os << object.prolog << "\n";
    }
    for (auto const &f : object.functions) {
        os << f;
    }
    os << "\n";
    if (!object.text.empty()) {
        os << object.text << "\n";
    }
    if (!object.data.empty()) {
        os << object.data << "\n";
    }
    return os;
}

bool Object::save_and_assemble() const
{
    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    fs::path path { dot_arwen / file_name };
    path.replace_extension("s");
    {
        std::fstream s(path, std::ios::out | std::ios::binary);
        if (!s.is_open()) {
            log_error("Could not open assembly file `{}`", path.string());
            return false;
        }
        std::wstringstream ss;
        ss << *this;
        s << as_utf8(ss.view());
        if (s.fail() || s.bad()) {
            log_error("Could not write assembly file `{}`: {}", path.string(), strerror(errno));
            return false;
        }
    }
    fs::path o_file { path };
    o_file.replace_extension("o");
    if (module != nullptr) {
        info(L"[X86_64] Assembling `{}`", module->name);
    } else {
        info("[X86_64] Assembling root module");
    }
    Util::Process as { "as", path.string(), "-o", o_file.string() };
    if (auto res = as.execute(); !res.has_value()) {
        log_error("`as` execution failed: {}", res.error().description);
        return false;
    } else if (res.value() != 0) {
        log_error("`as` returned {}:\n{}", res.value(), as.stderr());
        return false;
    }
    if (!has_option("keep-assembly")) {
        fs::remove(path);
    }
    return true;
}

bool Executable::generate()
{
    auto  program_obj_name = std::format(L"__program_{}", program->name);
    auto &static_init = objects.emplace_back(program_obj_name);
    auto &prog_init = static_init.functions.emplace_back(L"__static_init", static_init);
    prog_init.naked = true;
    prog_init.generate(program->operations);
    for (auto const &[name, mod] : get<IR::Program>(program).modules) {
        if (!mod->operations.empty()) {
            prog_init.writer.add_instruction(L"call", std::format(L"_{}_init", name));
        }
        objects.emplace_back(name, mod).generate();
    }

    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    std::vector<fs::path> o_files;
    for (auto const &obj : objects) {
        if (obj.has_exports) {
            if (!obj.save_and_assemble()) {
                return false;
            }
            fs::path path { dot_arwen / obj.file_name };
            o_files.push_back(path.replace_extension("o"));
        }
    }

    if (!o_files.empty()) {
        fs::path program_path { as_utf8(program->name) };
        program_path.replace_extension();
        info("[X86_64] Linking `{}`", program_path.string());

        std::vector<std::string> ld_args {
            "-o",
            fs::path { as_utf8(program->name) }.replace_extension("").string(),
            // "-no-pie",
            std::format("-L{}/lib", Arwen::arwen_dir().string()),
            "-larwenrt",
        };
        for (auto const &o : o_files) {
            ld_args.push_back(o.string());
        }

        Util::Process link { "cc", ld_args };
        if (auto res = link.execute(); !res.has_value()) {
            log_error("Linker execution failed: {}", res.error().description);
            return false;
        } else if (res.value() != 0) {
            log_error("Linking failed:\n{}", link.stderr());
        }
        if (!has_option("keep-objects")) {
            for (auto const &o : o_files) {
                fs::remove(o);
            }
        }
    }
    return true;
}

std::wostream &operator<<(std::wostream &os, Executable &executable)
{
    for (auto const &obj : executable.objects) {
        os << obj.file_name << ":\n\n";
        os << obj;
    }
    return os;
}

bool generate_x86_64(IR::IRNodes const &program)
{
    Executable executable { program.entry_point };
    return executable.generate();
}
}
