/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <bitset>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <print>
#include <sstream>
#include <string>
#include <string_view>

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

#include <Arch/Arm64/Arm64.h>
#include <variant>

namespace Arwen::Arm64 {

namespace fs = std::filesystem;

void Function::add_instruction(std::wstring_view const mnemonic, std::wstring_view const param)
{
    *active += std::format(L"\t{}\t{}\n", mnemonic, param);
}

void Function::add_instruction(std::wstring_view const mnemonic)
{
    *active += std::format(L"\t{}\n", mnemonic);
}

void Function::add_text(std::wstring_view const &text)
{
    if (text.empty()) {
        return;
    }
    auto t = strip(text);
    for (auto line : split(t, '\n')) {
        if (line.empty()) {
            *active += '\n';
            continue;
        }
        line = strip(line);
        if (line[0] == ';') {
            *active += std::format(L"\t{}\n", line);
            continue;
        }
        if ((line[0] == '.') || line.ends_with(L":")) {
            *active += std::format(L"{}\n", line);
            continue;
        }
        for (auto const &p : split(strip(line), ' ')) {
            if (p.empty()) {
                continue;
            }
            *active += std::format(L"\t{}", p);
        }
        *active += '\n';
    }
}

void Function::add_label(std::wstring_view const label)
{
    *active += std::format(L"\n{}:\n", label);
}

void Function::add_directive(std::wstring_view const directive, std::wstring_view const args)
{
    *active += std::format(L"{}\t{}\n", directive, args);
}

void Function::add_comment(std::wstring_view const comment)
{
    if (comment.empty()) {
        return;
    }
    *active += '\n';
    for (auto const line : split(strip(comment), '\n')) {
        *active += std::format(L"\t; {}\n", strip(line));
    }
}

bool Function::empty() const
{
    return code.empty();
}

bool Function::has_text() const
{
    return !empty();
}

void Function::activate_prolog()
{
    active = &prolog;
}

void Function::activate_code()
{
    active = &code;
}

void Function::activate_epilog()
{
    active = &epilog;
}

void Function::analyze(std::vector<IR::Operation> const &operations)
{
    if (function) {
        for (auto const &[name, type] : function->parameters) {
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

void Function::emit_return()
{
    add_instruction(L"mov", L"sp,fp");
    add_instruction(L"ldp", L"fp,lr,[sp],16");
    add_instruction(L"ret");
}

void Function::skeleton()
{
    activate_prolog();
    add_label(name);
    add_instruction(L"stp", L"fp,lr,[sp,#-16]!");
    add_instruction(L"mov", L"fp,sp");
    if (stack_depth > 0) {
        add_instruction(L"sub", L"sp,sp,#{}", stack_depth);
    }

    activate_epilog();
    emit_return();

    activate_code();
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

RegisterAllocation Function::push_reg(pType const &type)
{
    return push_reg(type->size_of());
}

RegisterAllocation Function::push_reg(size_t size)
{
    auto               num { words_needed(size) };
    RegisterAllocation ret { -1, num };
    for (int reg = 9; reg + num < 16; ++reg) {
        bool available { true };
        for (auto ix = reg; ix < reg + num; ++ix) {
            if (regs[ix]) {
                available = { false };
                break;
            }
        }
        if (available) {
            for (auto ix = reg; ix < reg + num; ++ix) {
                regs[ix] = true;
            }
            ret.reg = reg;
            break;
        }
    }
    stack.push_back(ret);
    debug_stack(*this, std::format("push_reg({}) {}/{}", size, ret.reg, ret.num_regs));
    return ret;
}

RegisterAllocation Function::pop_reg(pType const &type)
{
    return pop_reg(type->size_of());
}

RegisterAllocation Function::pop_reg(size_t size)
{
    assert(!stack.empty());
    assert(std::holds_alternative<RegisterAllocation>(stack.back()));

    auto ret { std::get<RegisterAllocation>(stack.back()) };
    if (ret.reg != -1) {
        for (auto ix = ret.reg; ix < ret.reg + ret.num_regs; ++ix) {
            regs[ix] = false;
        }
    }
    stack.pop_back();
    debug_stack(*this, std::format("pop_reg({}) {}/{}", size, ret.reg, ret.num_regs));
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
    if (auto dest = push_reg(size); dest.reg > 0) {
        for (auto i = 0; i < dest.num_regs; ++i) {
            add_instruction(L"mov", L"x{},x{}", dest.reg + i, i);
        }
    } else {
        auto num { dest.num_regs };
        while (num > 0) {
            if (num > 1) {
                add_instruction(L"stp", L"x{},x{},[sp,#16]!", dest.num_regs - num, dest.num_regs - num + 1);
                num -= 2;
            } else {
                add_instruction(L"str", L"x{},[sp,#16]!", dest.num_regs - num);
                --num;
            }
        }
    }
}

// Pops the value currently at the top of the value stack to x0..
int Function::pop(pType const &type, int target)
{
    if (type == TypeRegistry::void_) {
        return 0;
    }
    return pop(type->size_of(), target);
}

int Function::pop(size_t size, int target)
{
    if (size == 0) {
        return 0;
    }
    assert(!stack.empty());
    std::visit(
        overloads {
            [this, &size, &target](RegisterAllocation const &) {
                if (auto src = pop_reg(size); src.reg > 0) {
                    for (auto i = 0; i < src.num_regs; ++i) {
                        add_instruction(L"mov", L"x{},x{}", target + i, src.reg + i);
                    }
                } else {
                    auto num { src.num_regs };
                    while (num > 0) {
                        if (num > 1) {
                            add_instruction(L"ldp", L"x{},x{},[sp],#16", target + src.num_regs - num, target + src.num_regs - num + 1);
                            num -= 2;
                        } else {
                            add_instruction(L"str", L"x{},[sp],#16", target + src.num_regs - num);
                            --num;
                        }
                    }
                }
            },
            [this, &size, &target](VarPointer const &) {
                deref(size, target);
            } },
        stack.back());
    return target + words_needed(size);
}

// Pops the variable reference off the value stack and moves the value
// into x0...
VarPointer Function::deref(pType const &type, int target)
{
    if (type == TypeRegistry::void_) {
        return 0;
    }
    return deref(type->size_of(), target);
}

VarPointer Function::deref(size_t size, int target)
{
    if (size == 0) {
        return 0;
    }
    assert(!stack.empty());
    assert(std::holds_alternative<VarPointer>(stack.back()));
    auto ptr { std::get<VarPointer>(stack.back()) };
    auto ret { ptr };
    stack.pop_back();
    debug_stack(*this, std::format("deref({}, {}) VarPointer {}", size, target, ptr));
    auto num_regs { words_needed(size) };
    auto num { num_regs };
    while (num > 0) {
        if (num > 1) {
            add_instruction(L"ldp", L"x{},x{},[fp,-{}]", target + num_regs - num, target + num_regs - num + 1, ptr);
            num -= 2;
            ptr += 2;
        } else {
            add_instruction(L"ldr", L"x{},[fp,-{}]", target + num_regs - num, ptr);
            --num;
            ++ptr;
        }
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

    // Pop the top of the value stack into x0...
    pop(size);

    // Move x0... into the variable:
    auto num_regs { words_needed(size) };
    auto num { num_regs };
    while (num > 0) {
        if (num > 1) {
            add_instruction(L"stp", L"x{},x{},[fp,-{}]", num_regs - num, num_regs - num + 1, ptr);
            num -= 2;
            ptr += 2;
        } else {
            add_instruction(L"str", L"x{},[fp,-{}]", num_regs - num, ptr);
            --num;
            ++ptr;
        }
    }
    return ret;
}

void Object::add_directive(std::wstring_view const directive, std::wstring_view const args)
{
    if (directive == L".global") {
        has_exports = true;
        if (args == L"main") {
            has_main = true;
        }
    }
    prolog += std::format(L"{}\t{}\n", directive, args);
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
        if (auto reg = function.pop_reg(impl.payload); reg.reg < 0) {
            function.add_instruction(L"add", L"sp,{}", alignat(reg.num_regs * 8, 16));
        }
    }
}

template<>
void generate_op(Function &function, IR::Operation::Jump const &impl)
{
    function.add_instruction(L"b", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::JumpF const &impl)
{
    pop<bool>(function);
    function.add_instruction(L"mov", L"x1,xzr");
    function.add_instruction(L"cmp", L"x0,x1");
    function.add_instruction(L"b.eq", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::JumpT const &impl)
{
    pop<bool>(function);
    function.add_instruction(L"mov", L"x1,xzr");
    function.add_instruction(L"cmp", L"x0,x1");
    function.add_instruction(L"b.ne", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::Label const &impl)
{
    function.add_label(std::format(L"lbl_{}", impl.payload));
}

void generate_op(Function &function, IR::Operation::NativeCall const &impl)
{
    auto const                     &op = impl.payload;
    std::vector<RegisterAllocation> allocations;
    int                             reg { 0 };
    for (auto const &[_, param_type] : op.parameters) {
        allocations.emplace_back(reg, words_needed(param_type->size_of()));
        reg += words_needed(param_type->size_of());
    }
    for (auto [dest, type] : std::views::zip(
                                 allocations,
                                 op.parameters
                                     | std::views::transform([](auto const &decl) {
                                           return decl.type;
                                       }))
            | std::views::reverse) {
        function.pop(type, dest.reg);
    }
    std::wstring name { op.name };
    if (auto colon = op.name.rfind(L':'); colon != std::wstring::npos) {
        name.erase(0, colon + 1);
    }
    function.add_instruction(L"bl", std::format(L"_{}", name));
    function.push(op.return_type);
}

void generate_op(Function &function, IR::Operation::Pop const &impl)
{
    while (!function.stack.empty()) {
        if (auto reg = function.pop_reg(impl.payload); reg.reg < 0) {
            function.add_instruction(L"add", L"sp,{}", alignat(reg.num_regs * 8, 16));
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
                if (auto reg { function.push_reg(impl.payload.type) }; reg.reg > 0) {
                    switch (int_type.width_bits) {
                    case 8:
                        function.add_instruction(L"mov", L"w{},#{}", reg.reg, static_cast<int>((int_type.is_signed) ? as<int8_t>(impl.payload) : as<uint8_t>(impl.payload)));
                        break;
                    case 16:
                        function.add_instruction(L"mov", L"w{},#{}", reg.reg, (int_type.is_signed) ? as<int16_t>(impl.payload) : as<uint16_t>(impl.payload));
                        break;
                    case 32:
                        function.add_instruction(L"mov", L"w{},#{}", reg.reg, (int_type.is_signed) ? as<int32_t>(impl.payload) : as<uint32_t>(impl.payload));
                        break;
                    case 64:
                        function.add_instruction(L"mov", L"x{},#{}", reg.reg, (int_type.is_signed) ? as<int64_t>(impl.payload) : as<uint64_t>(impl.payload));
                        break;
                    }
                } else {
                    switch (int_type.width_bits) {
                    case 8:
                        function.add_instruction(L"mov", L"w0,#{}", (int_type.is_signed) ? as<int8_t>(impl.payload) : as<uint8_t>(impl.payload));
                        break;
                    case 16:
                        function.add_instruction(L"mov", L"w0,#{}", (int_type.is_signed) ? as<int16_t>(impl.payload) : as<uint16_t>(impl.payload));
                        break;
                    case 32:
                        function.add_instruction(L"mov", L"w0,#{}", (int_type.is_signed) ? as<int32_t>(impl.payload) : as<uint32_t>(impl.payload));
                        break;
                    case 64:
                        function.add_instruction(L"mov", L"x0,#{}", (int_type.is_signed) ? as<int64_t>(impl.payload) : as<uint64_t>(impl.payload));
                        break;
                    }
                    function.add_instruction(L"str", L"x0,[sp,#-16]!", reg.reg);
                }
            },
            [&function, &impl](SliceType const &slice_type) -> void {
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(impl.payload);
                auto str_id = function.object.add_string(std::wstring_view { static_cast<wchar_t *>(slice.ptr), static_cast<size_t>(slice.size) });
                auto reg { function.push_reg(slice_type.size_of()) };
                if (reg.reg > 0) {
                    function.add_instruction(L"adr", L"x{},str_{}", reg.reg, str_id);
                    function.add_instruction(L"mov", L"x{},#{}", reg.reg + 1, slice.size);
                } else {
                    function.add_instruction(L"adr", L"x0,str_{}", str_id);
                    function.add_instruction(L"mov", L"x1,#{}", slice.size);
                    function.add_instruction(L"stp", L"x{},x{},[sp,#-16]!", reg.reg, reg.reg + 1);
                }
            },
            [&function, &impl](auto const &) -> void {
                function.add_comment(std::format(L"PushConstant {}", impl.payload.type->to_string()));
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
void generate_op(Function &function, IR::Operation::Return const &impl)
{
    function.add_comment(L"Return from function");
    function.emit_return();
}

template<>
void generate_op(Function &function, IR::Operation::ScopeBegin const &)
{
}

template<>
void generate_op(Function &function, IR::Operation::ScopeEnd const &)
{
}

template<>
void generate_op(Function &function, IR::Operation::Sub const &impl)
{
    function.add_comment(L"Calling in-function subroutine. Saving x0 in callee-saved register");
    function.add_instruction(L"mov", L"x19,x0");
    function.add_instruction(L"bl", std::format(L"lbl_{}", impl.payload));
    function.add_instruction(L"mov", L"x0,x19");
}

template<>
void generate_op(Function &function, IR::Operation::SubRet const &impl)
{
    function.add_comment(L"Return from in-function subroutine");
    function.add_instruction(L"ret");
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
    regs.reset();
    for (auto const &op : operations) {
        std::visit(
            [this](auto const &impl) -> void {
                add_comment(as_wstring(typeid(decltype(impl)).name()));
                // std::cerr << typeid(decltype(impl)).name() << std::endl;
                generate_op(*this, impl);
            },
            op.op);
    }
}

std::wostream &operator<<(std::wostream &os, Function const &function)
{
    os << std::format(L"{}", function.prolog);
    os << std::format(L"{}", function.code);
    os << std::format(L"{}", function.epilog) << std::endl;
    return os;
}

void Object::generate(IR::pModule module)
{
    auto &mod_init = functions.emplace_back(std::format(L"_{}_init", module->name), *this);
    mod_init.generate(module->operations);

    for (auto const &[name, f] : module->functions) {
        auto &function = functions.emplace_back(name, *this, f);
        function.generate(f->operations);
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

std::expected<bool, ARM64Error> Object::save_and_assemble() const
{
    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    fs::path path { dot_arwen / file_name };
    path.replace_extension("s");
    {
        std::fstream s(path, std::ios::out | std::ios::binary);
        if (!s.is_open()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::IOError, std::format("Could not open assembly file `{}`", path.string()) });
        }
        std::wstringstream ss;
        ss << *this;
        s << as_utf8(ss.view());
        if (s.fail() || s.bad()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::IOError, std::format("Could not write assembly file `{}`: {}", path.string(), strerror(errno)) });
        }
    }
    fs::path o_file { path };
    o_file.replace_extension("o");
    if (module != nullptr) {
        std::wcerr << std::format(L"[ARM64] Assembling `{}`", module->name) << std::endl;
    } else {
        std::println("[ARM64] Assembling root module");
    }
    Util::Process as { "as", path.string(), "-o", o_file.string() };
    std::string   as_errors;
    as.on_stderr_read([&as_errors](Util::ReadPipe &pipe) {
        as_errors = std::move(pipe.current());
    });
    if (auto res = as.execute(); !res.has_value()) {
        return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("`as` execution failed: {}", res.error().description) });
    } else if (res.value() != 0) {
        return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("`as` returned {}:\n{}", res.value(), as_errors) });
    }
    if (!has_option("keep-assembly")) {
        fs::remove(path);
    }
    return true;
}

std::expected<void, ARM64Error> Executable::generate()
{
    auto &static_init = objects.emplace_back(L"__program");
    auto &prog_init = static_init.functions.emplace_back(L"__program_init", static_init);

    prog_init.generate(program->operations);
    for (auto const &[name, mod] : program->modules) {
        prog_init.add_instruction(L"bl", std::format(L"_{}_init", name));
        objects.emplace_back(name, mod).generate(mod);
    }

    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    std::vector<fs::path> o_files;
    for (auto const &obj : objects) {
        if (obj.has_exports) {
            if (auto res { obj.save_and_assemble() }; !res.has_value()) {
                return std::unexpected(res.error());
            }
            fs::path path { dot_arwen / obj.file_name };
            o_files.push_back(path.replace_extension("o"));
        }
    }

    if (!o_files.empty()) {
        std::string   sdk_path; // "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk";
        Util::Process p { "xcrun", "-sdk", "macosx", "--show-sdk-path" };
        p.on_stdout_read([&sdk_path](Util::ReadPipe &pipe) {
            sdk_path = strip(std::string_view { pipe.current() });
        });
        if (auto res = p.execute(); !res.has_value()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("`xcrun` execution failed: {}", res.error().description) });
        } else if (res.value() != 0) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("`xcrun` execution returned {}", res.value()) });
        }
        fs::path program_path { as_utf8(program->name) };
        program_path.replace_extension();
        std::println("[ARM64] Linking `{}`", program_path.string());
        std::println("[ARM64] SDK path: `{}`", sdk_path);

        std::vector<std::string> ld_args {
            "-o",
            fs::path { as_utf8(program->name) }.replace_extension("").string(),
            std::format("-L{}/lib", Arwen::arwen_dir().string()),
            "-larwenstart",
            "-larwenrt",
            "-lSystem",
            "-syslibroot",
            sdk_path,
            "-e",
            "_start",
            "-arch",
            "arm64",
        };
        for (auto const &o : o_files) {
            ld_args.push_back(o.string());
        }

        std::string   linker_errors;
        Util::Process link { "ld", ld_args };
        link.on_stderr_read([&linker_errors](Util::ReadPipe &pipe) {
            linker_errors = std::move(pipe.current());
        });
        if (auto res = link.execute(); !res.has_value()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("Linker execution failed: {}", res.error().description) });
        } else if (res.value() != 0) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::InternalError, std::format("Linking failed:\n{}", linker_errors) });
        }
        if (!has_option("keep-objects")) {
            for (auto const &o : o_files) {
                fs::remove(o);
            }
        }
    }
    return {};
}

std::wostream &operator<<(std::wostream &os, Executable &executable)
{
    for (auto const &obj : executable.objects) {
        os << obj.file_name << ":\n\n";
        os << obj;
    }
    return os;
}

std::expected<void, ARM64Error> generate_arm64(IR::pProgram program)
{
    Executable executable { program };
    return executable.generate();
}
}
