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
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Process.h>
#include <Util/Utf8.h>

#include <App/Config.h>
#include <App/Type.h>
#include <Util/Options.h>
#include <Util/Pipe.h>

#include <App/IR/IR.h>

#include <Arch/Arm64/Arm64.h>

namespace Arwen::Arm64 {

namespace fs = std::filesystem;

struct RegisterAllocation {
    int      reg;
    intptr_t num_regs;
};

struct Function {
    std::wstring                     name;
    struct Object                   &object;
    IR::pFunction                    function { nullptr };
    uint64_t                         stack_depth { 0 };
    std::map<std::wstring, uint64_t> variables;

    std::wstring  prolog;
    std::wstring  code;
    std::wstring  epilog;
    std::wstring *active { &code };

    std::bitset<28>                 regs;
    std::vector<RegisterAllocation> regstack;

    template<typename... Args>
    void add_instruction(std::wstring_view const mnemonic, std::wstring_view param_fmt, Args &&...args)
    {
        auto const fmt { std::format(L"\t{{}}\t{}\n", param_fmt) };
        *active += std::vformat(fmt, std::make_wformat_args(mnemonic, args...));
    }

    void add_instruction(std::wstring_view const mnemonic, std::wstring_view const param)
    {
        *active += std::format(L"\t{}\t{}\n", mnemonic, param);
    }

    void add_instruction(std::wstring_view const mnemonic)
    {
        *active += std::format(L"\t{}\n", mnemonic);
    }

    void add_text(std::wstring_view const &text)
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

    void add_label(std::wstring_view const label)
    {
        *active += std::format(L"\n{}:\n", label);
    }

    void add_directive(std::wstring_view const directive, std::wstring_view const args)
    {
        *active += std::format(L"{}\t{}\n", directive, args);
    }

    void add_comment(std::wstring_view const comment)
    {
        if (comment.empty()) {
            return;
        }
        *active += '\n';
        for (auto const line : split(strip(comment), '\n')) {
            *active += std::format(L"\t; {}\n", strip(line));
        }
    }

    [[nodiscard]] bool empty() const
    {
        return code.empty();
    }

    [[nodiscard]] bool has_text() const
    {
        return !empty();
    }

    void activate_prolog()
    {
        active = &prolog;
    }

    void activate_code()
    {
        active = &code;
    }

    void activate_epilog()
    {
        active = &epilog;
    }

    void emit_return()
    {
        add_instruction(L"mov", L"sp,fp");
        if (stack_depth > 0) {
            add_instruction(L"add", L"sp,sp,#{}", stack_depth);
        }
        add_instruction(L"ldp", L"fp,lr,[sp],16");
        add_instruction(L"ret");
    }

    void skeleton()
    {
        activate_prolog();
        add_label(name);
        add_instruction(L"stp", L"fp,lr,[sp,#-16]!");
        if (stack_depth > 0) {
            add_instruction(L"sub", L"sp,sp,#{}", stack_depth);
        }
        add_instruction(L"mov", L"fp,sp");

        activate_epilog();
        emit_return();

        activate_code();
    }

    RegisterAllocation push_reg(pType const &type)
    {
        auto               num { alignat(type->size_of() / 8, 1) };
        RegisterAllocation ret { -1, num };
        std::bitset<28>    mask;
        for (auto ix = 0; ix < num; ++ix) {
            mask = (mask << 1) | std::bitset<28> { 0x01 };
        }
        mask.flip() <<= 9;
        for (int reg = 9; reg < 16; ++reg) {
            if ((regs & mask) == regs) {
                regs |= mask;
                ret.reg = reg;
                break;
            }
            mask <<= 1;
        }
        regstack.push_back(ret);
        return ret;
    }

    RegisterAllocation pop_reg(pType const &type)
    {
        assert(!regstack.empty());
        auto            num { alignat(type->size_of() / 8, 1) };
        std::bitset<28> mask;
        for (auto ix = 0; ix < num; ++ix) {
            mask = (mask << 1) | std::bitset<28> { 0x01 };
        }
        mask.flip();
        auto ret { regstack.back() };
        if (ret.reg != -1) {
            mask <<= ret.reg;
            regs &= mask;
        }
        return ret;
    }
};

struct Object {
    std::wstring          file_name;
    IR::pModule           module { nullptr };
    std::vector<Function> functions;

    uint64_t                         next_label { 0 };
    std::wstring                     prolog;
    std::wstring                     text;
    std::wstring                     data;
    std::map<std::wstring, uint64_t> strings;
    bool                             has_exports { false };
    bool                             has_main { false };

    void add_directive(std::wstring_view const directive, std::wstring_view const args)
    {
        if (directive == L".global") {
            has_exports = true;
            if (args == L"main") {
                has_main = true;
            }
        }
        prolog += std::format(L"{}\t{}\n", directive, args);
    }

    int add_string(std::wstring_view const str)
    {
        return add_string(std::wstring { str });
    }

    int add_string(std::wstring const &str)
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
        strings[str] = id;
        return id;
    }

    template<typename Arg>
    void add_data(std::wstring_view const label, bool global, std::wstring_view type, bool is_static, Arg const &arg)
    {
        if (data.empty()) {
            data = L"\n\n.section __DATA,__data\n";
        }
        if (global) {
            data += format(L"\n.global {}", label);
        }
        data += format(L"\n.align 8\n{}:\n\t{}\t{}", label, type, arg);
        if (is_static) {
            data += L"\n\t.short 0";
        }
    }
};

void analyze(Function &function, std::vector<IR::Operation> const &operations)
{
    std::vector<uint64_t> depths;
    uint64_t              depth { 0 };
    if (function.function) {
        for (auto const &[name, type] : function.function->parameters) {
            function.variables[name] = function.stack_depth;
            depth += alignat(type->size_of(), 16);
        }
    }
    function.stack_depth = depth;
    for (auto const &op : operations) {
        std::visit(
            overloads {
                [&function, &depth, &depths](IR::Operation::ScopeBegin const &impl) -> void {
                    depths.emplace_back(depth);
                    for (auto const &[name, type] : impl.payload) {
                        function.variables[name] = function.stack_depth;
                        depth += alignat(type->size_of(), 16);
                    }
                    if (depth > function.stack_depth) {
                        function.stack_depth = depth;
                    }
                },
                [&function, &depth, &depths](IR::Operation::ScopeEnd const &impl) -> void {
                    depth = depths.back();
                    depths.pop_back();
                },
                [&function](auto const &impl) -> void {
                } },
            op.op);
    }
}

template<typename Impl>
void generate_op(Function &function, Impl const &impl)
{
}

template<>
void generate_op(Function &function, IR::Operation::Discard const &impl)
{
    if (impl.payload == TypeRegistry::void_) {
        return;
    }
    if (auto reg = function.pop_reg(impl.payload); reg.reg < 0) {
        function.add_instruction(L"add", L"sp,{}", alignat(reg.num_regs * 8, 16));
    }
}

template<>
void generate_op(Function &function, IR::Operation::Jump const &impl)
{
    function.add_instruction(L"b", std::format(L"lbl_{}", impl.payload));
}

template<>
void generate_op(Function &function, IR::Operation::Label const &impl)
{
    function.add_label(std::format(L"lbl_{}", impl.payload));
}

void pop(Function &function, pType const &type)
{
    if (type == TypeRegistry::void_) {
        return;
    }
    if (auto dest = function.push_reg(type); dest.reg > 0) {
        for (auto i = 0; i < dest.num_regs; ++i) {
            function.add_instruction(L"mov", L"x{},x{}", dest.reg + i, i);
        }
    } else {
        auto num { dest.num_regs };
        while (num > 0) {
            if (num > 1) {
                function.add_instruction(L"stp", L"x{},x{},[sp,#16]!", dest.num_regs - num, dest.num_regs - num + 1);
                num -= 2;
            } else {
                function.add_instruction(L"str", L"x{},[sp,#16]!", dest.num_regs - num);
                --num;
            }
        }
    }
}

void generate_op(Function &function, IR::Operation::NativeCall const &impl)
{
    auto const                     &op = impl.payload;
    std::vector<RegisterAllocation> allocations;
    int                             reg { 0 };
    for (auto const &[_, param_type] : op.parameters) {
        allocations.emplace_back(reg, alignat(param_type->size_of() / 8, 1));
        reg += alignat(param_type->size_of() / 8, 1);
    }
    for (auto [dest, type] : std::views::zip(
                                 allocations,
                                 op.parameters
                                     | std::views::transform([](auto const &decl) {
                                           return decl.type;
                                       }))
            | std::views::reverse) {
        auto src { function.pop_reg(type) };
        if (src.num_regs < 0) {
            auto num = src.num_regs;
            while (num > 0) {
                if (num % 2) {
                    function.add_instruction(L"ldr", L"x{},[sp],#16", dest.reg + num - 1);
                    --num;
                } else {
                    function.add_instruction(L"ldp", L"x{},x{},[sp],#16", dest.reg + num - 2, dest.reg + num - 1);
                    num -= 2;
                }
            }
        } else {
            for (auto i = 0; i < dest.num_regs; ++i) {
                function.add_instruction(L"mov", L"x{},x{}", dest.reg + i, src.reg + i);
            }
        }
    }
    if (auto colon = op.name.rfind(L':'); colon != std::wstring::npos) {
        function.add_instruction(L"bl", std::format(L"_{}", op.name.substr(colon + 1)));
    } else {
        function.add_instruction(L"bl", std::format(L"_{}", op.name));
    }
    pop(function, op.return_type);
}

void generate_op(Function &function, IR::Operation::Pop const &impl)
{
    pop(function, impl.payload);
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
                auto reg { function.push_reg(TypeRegistry::u32) };
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

void generate(Function &function, std::vector<IR::Operation> const &operations)
{
    analyze(function, operations);
    function.object.add_directive(L".global", function.name);
    function.skeleton();
    function.regs.reset();
    for (auto const &op : operations) {
        std::visit(
            [&function](auto const &impl) -> void {
                function.add_comment(as_wstring(typeid(decltype(impl)).name()));
                generate_op(function, impl);
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

void generate(Object &object, IR::pModule module)
{
    auto &mod_init = object.functions.emplace_back(std::format(L"_{}_init", module->name), object);
    generate(mod_init, module->operations);

    for (auto const &[name, f] : module->functions) {
        auto &function = object.functions.emplace_back(name, object, f);
        generate(function, f->operations);
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

std::expected<bool, ARM64Error> save_and_assemble(Object const &object)
{
    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    fs::path path { dot_arwen / object.file_name };
    path.replace_extension("s");
    {
        std::fstream s(path, std::ios::out | std::ios::binary);
        if (!s.is_open()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::IOError, std::format("Could not open assembly file `{}`", path.string()) });
        }
        std::wstringstream ss;
        ss << object;
        s << as_utf8(ss.view());
        if (s.fail() || s.bad()) {
            return std::unexpected(ARM64Error { ARM64ErrorCode::IOError, std::format("Could not write assembly file `{}`: {}", path.string(), strerror(errno)) });
        }
    }
    fs::path o_file { path };
    o_file.replace_extension("o");
    if (object.module != nullptr) {
        std::wcerr << std::format(L"[ARM64] Assembling `{}`", object.module->name) << std::endl;
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

struct Executable {
    std::vector<Object> objects;
};

std::expected<void, ARM64Error> generate(Executable &executable, IR::pProgram const &program)
{
    auto &static_init = executable.objects.emplace_back(L"__program");
    auto &prog_init = static_init.functions.emplace_back(L"__program_init", static_init);

    generate(prog_init, program->operations);
    for (auto const &[name, mod] : program->modules) {
        prog_init.add_instruction(L"bl", std::format(L"_{}_init", name));
        generate(executable.objects.emplace_back(name, mod), mod);
    }

    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    std::vector<fs::path> o_files;
    for (auto const &obj : executable.objects) {
        if (obj.has_exports) {
            if (auto res { save_and_assemble(obj) }; !res.has_value()) {
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
    Executable executable;
    return generate(executable, program);
}

}
