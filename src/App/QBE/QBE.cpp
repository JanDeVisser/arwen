/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ranges>
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
#include <App/Value.h>

#include <App/IR/IR.h>

#include <App/Parser.h>
#include <App/QBE/QBE.h>
#include <App/SyntaxNode.h>

namespace Arwen::QBE {

struct QBEContext {
    std::wstring              text;
    int                       next_label;
    int                       next_var;
    std::vector<std::wstring> strings;
    fs::path                  file_name;
    bool                      has_exports;
};

using QBEContexts = std::vector<QBEContext>;

GenResult generate_qbe_node(ASTNode const &, QBEContext &);
GenResult generate_qbe_nodes(ASTNodes const &, QBEContext &);

bool qbe_first_class_type(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &) -> bool {
                return true;
            },
            [](FloatType const &) -> bool {
                return true;
            },
            [](BoolType const &) -> bool {
                return true;
            },
            [](auto const &) -> bool {
                return false;
            },
        },
        type->description);
}

std::wstring_view qbe_type(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &descr) -> std::wstring_view {
                return (descr.width_bits < 64) ? L"w" : L"l";
            },
            [](BoolType const &) -> std::wstring_view {
                return L"w";
            },
            [](FloatType const &descr) -> std::wstring_view {
                return (descr.width_bits < 64) ? L"s" : L"d";
            },
            [](SliceType const &) -> std::wstring_view {
                return L":slice_t";
            },
            [](auto const &) -> std::wstring_view {
                return L"l";
            },
        },
        type->description);
}

template<class Node>
GenResult generate_qbe_node(ASTNode const &, Node const &, QBEContext &)
{
    fatal("Unimplemented QBE serialization for {}", typeid(Node).name());
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Block const &impl, QBEContext &ctx)
{
    ctx.text += std::format(L"@lbl_{}\n", ++ctx.next_label);
    for (auto const &s : impl.statements) {
        if (auto res = generate_qbe_node(s, ctx); !res) {
            return res;
        }
    }
    return {};
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Call const &impl, QBEContext &ctx)
{
    struct Argument {
        ASTNode n;
        int     var;
    };

    auto def = get<FunctionDefinition>(impl.function);
    auto decl = get<FunctionDeclaration>(def.declaration);

    std::vector<Argument> args;
    for (auto const &expression : get<ExpressionList>(impl.arguments).expressions) {
        if (is<Constant>(expression) && qbe_first_class_type(expression->bound_type)) {
            args.emplace_back(expression, 0);
            continue;
        }
        if (auto res = generate_qbe_node(expression, ctx); res) {
            if (res.value() == 0) {
                return std::unexpected(L"Argument expression must evaluate to value");
            }
            args.emplace_back(expression, res.value());
        } else {
            return res;
        }
    }
    ctx.text += L"    ";
    int ret = 0;
    if (n->bound_type != TypeRegistry::void_) {
        ret = ++ctx.next_var;
        ctx.text += std::format(L"%v{} = {} ", ret, qbe_type(n->bound_type));
    }
    auto name = std::visit(
        overloads {
            [](ExternLink const &link) -> std::wstring_view {
                if (auto colon = link.link_name.rfind(L':'); colon != std::wstring::npos) {
                    return std::wstring_view { link.link_name }.substr(colon + 1);
                }
                return link.link_name;
            },
            [&def](auto const &) -> std::wstring_view {
                return def.name;
            } },
        def.implementation->node);
    ctx.text += std::format(L"call ${}(", name);
    bool first = true;
    for (auto const &[expr, var] : args) {
        if (!first) {
            ctx.text += L", ";
        }
        first = false;
        ctx.text += std::format(L"{} ", qbe_type(expr->bound_type));
        if (var > 0) {
            ctx.text += std::format(L"%v{}", var);
        } else {
            if (auto res = generate_qbe_node(expr, ctx); !res) {
                return res;
            }
        }
    }
    ctx.text += L")\n";
    return ret;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Comptime const &impl, QBEContext &ctx)
{
    return generate_qbe_node(impl.statements, ctx);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Constant const &impl, QBEContext &ctx)
{
    if (impl.bound_value->type == TypeRegistry::void_) {
        return 0;
    }
    int var = 0;
    std::visit(
        overloads {
            [&var, &ctx, &impl](BoolType const &) -> void {
                ctx.text += std::format(L"{}", as<bool>(*impl.bound_value));
            },
            [&var, &ctx, &impl](IntType const &int_type) -> void {
                switch (int_type.width_bits) {
                case 8:
                case 16:
                case 32:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int32_t>(*impl.bound_value) : as<uint32_t>(*impl.bound_value));
                    break;
                case 64:
                    ctx.text += std::format(L"{}",
                        (int_type.is_signed) ? as<int64_t>(*impl.bound_value) : as<uint64_t>(*impl.bound_value));
                    break;
                }
            },
            [&var, &ctx, &impl](FloatType const &float_type) -> void {
                switch (float_type.width_bits) {
                case 32:
                    ctx.text += std::format(L"{}", as<float>(*impl.bound_value));
                    break;
                case 64:
                    ctx.text += std::format(L"{}", as<double>(*impl.bound_value));
                    break;
                }
            },
            [&ctx, &impl, &var](SliceType const &slice_type) -> void {
                var = ++ctx.next_var;
                ctx.text += L"    ";
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(*impl.bound_value);
                ctx.strings.emplace_back(std::wstring_view { static_cast<wchar_t *>(slice.ptr), static_cast<size_t>(slice.size) });
                auto str_id = ctx.strings.size();
                auto len_id = ++ctx.next_var;
                ctx.text += std::format(
                    LR"(%v{} = l alloc16 16
    storel $str_{}, %v{}
    %v{} = l add %v{}, 8
    storel {}, %v{}
)",
                    var, str_id, var, len_id, var, slice.size, len_id);
            },
            [](auto const &descr) -> void {
                UNREACHABLE();
            } },
        impl.bound_value->type->description);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Dummy const &impl, QBEContext &ctx)
{
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDefinition const &impl, QBEContext &ctx)
{
    if (!is<ExternLink>(impl.implementation)) {
        if (auto res = generate_qbe_node(impl.declaration, ctx); !res) {
            return res;
        }
        ctx.text += L" {\n@start\n";
        ctx.next_var = 0;
        ctx.next_label = 0;
        if (auto res = generate_qbe_node(impl.implementation, ctx); !res) {
            return res;
        }
        ctx.text += L"}\n\n";
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDeclaration const &impl, QBEContext &ctx)
{
    ctx.text += std::format(L"function {} ${}(", qbe_type(impl.return_type->bound_type), impl.name);
    auto first = true;
    for (auto const &param : impl.parameters) {
        auto p = get<Parameter>(param);
        if (!first) {
            ctx.text += L", ";
        }
        first = false;
        ctx.text += std::format(L"{} {}", qbe_type(param->bound_type), p.name);
    }
    ctx.text += ')';
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Identifier const &impl, QBEContext &ctx)
{
    auto              var = ++ctx.next_var;
    auto              size = n->bound_type->size_of();
    std::wstring_view code = std::visit(
        overloads {
            [](IntType const &int_type) -> std::wstring_view {
                switch (int_type.width_bits) {
                case 8:
                    return (int_type.is_signed) ? L"sb" : L"ub";
                    break;
                case 16:
                    return (int_type.is_signed) ? L"sh" : L"uh";
                    break;
                case 32:
                    return (int_type.is_signed) ? L"sw" : L"uw";
                    break;
                case 64:
                    return L"l";
                    break;
                default:
                    UNREACHABLE();
                }
            },
            [](FloatType const &float_type) -> std::wstring_view {
                return (float_type.width_bits == 64) ? L"d" : L"s";
            },
            [](auto const &) -> std::wstring_view {
                NYI("Identifier");
            } },
        n->bound_type->description);
    ctx.text += std::format(L"    %v{} = {} load{} %{}$\n", var, code.back(), code, impl.identifier);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Module const &impl, QBEContext &ctx)
{
    ctx.text += LR"(type :slice_t = { l, l }

)";
    if (auto res = generate_qbe_nodes(impl.statements, ctx); !res) {
        return res;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(ctx.strings)) {
        ctx.text += std::format(L"data $str_{} = {{ ", ix + 1);
        for (auto ch : s) {
            ctx.text += std::format(L"w {:d}, ", ch);
        }
        ctx.text += L"w 0 }\n";
    }
    return 0;
}

std::expected<void, std::wstring> save_and_assemble(QBEContext const &ctx)
{
    return {};
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Program const &impl, QBEContext &ctx)
{
    fs::path dot_arwen { ".arwen" };
    fs::create_directory(dot_arwen);
    QBEContexts           objects;
    std::vector<fs::path> o_files;

    for (auto const &[mod_name, mod] : impl.modules) {
        auto &mod_ctx = objects.emplace_back();
        mod_ctx.file_name = dot_arwen / mod_name;
        mod_ctx.file_name.replace_extension("ssa");
        if (auto res = generate_qbe_node(mod, mod_ctx); !res) {
            return res;
        }
        if (mod_ctx.has_exports) {
            {
                std::wofstream os { mod_ctx.file_name };
                os << mod_ctx.text << '\n';
            }
            fs::path s_file { mod_ctx.file_name };
            s_file.replace_extension("s");
            fs::path o_file { mod_ctx.file_name };
            o_file.replace_extension("o");

            info("[QBE] Compiling `{}`", mod_ctx.file_name.string());
            Util::Process qbe { "qbe", "-o", s_file.string(), mod_ctx.file_name.string() };
            if (auto res = qbe.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"qbe execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"qbe failed: {}", as_wstring(qbe.stderr())));
            }
            Util::Process as { "as", "-o", o_file.string(), s_file.string() };
            if (auto res = as.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"as execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"as failed: {}", as_wstring(as.stderr())));
            }
            if (!has_option("keep-assembly")) {
                fs::remove(mod_ctx.file_name);
            }
            o_files.push_back(o_file);
        }
    }

    if (!o_files.empty()) {
        fs::path program_path { as_utf8(impl.name) };
        program_path.replace_extension();
        info("[QBE] Linking `{}`", program_path.string());

        std::vector<std::string> ld_args {
            "-o",
            fs::path { as_utf8(impl.name) }.replace_extension("").string(),
            // "-no-pie",
            std::format("-L{}/lib", Arwen::arwen_dir().string()),
            "-larwenrt",
        };
        for (auto const &o : o_files) {
            ld_args.push_back(o.string());
        }

        Util::Process link { "cc", ld_args };
        if (auto res = link.execute(); !res.has_value()) {
            return std::unexpected(std::format(L"Linker execution failed: {}", as_wstring(res.error().description)));
        } else if (res.value() != 0) {
            return std::unexpected(std::format(L"Linking failed: {}", as_wstring(link.stderr())));
        }
        if (!has_option("keep-objects")) {
            for (auto const &o : o_files) {
                fs::remove(o);
            }
        }
    }
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, PublicDeclaration const &impl, QBEContext &ctx)
{
    ctx.text += L"export ";
    ctx.has_exports = true;
    return generate_qbe_node(impl.declaration, ctx);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Return const &impl, QBEContext &ctx)
{
    int var = 0;
    if (impl.expression != nullptr) {
        if (!is<Constant>(impl.expression) || !qbe_first_class_type(impl.expression->bound_type)) {
            if (auto res = generate_qbe_node(impl.expression, ctx); !res) {
                return res;
            } else {
                if (res.value() == 0) {
                    return std::unexpected(L"Expected variable index");
                }
                var = res.value();
            }
        }
    }
    ctx.text += L"    ret";
    if (impl.expression != nullptr) {
        if (var != 0) {
            ctx.text += std::format(L" %v{}", var);
        } else {
            ctx.text += ' ';
            if (auto res = generate_qbe_node(impl.expression, ctx); !res) {
                return res;
            }
        }
    }
    ctx.text += '\n';
    return 0;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, VariableDeclaration const &impl, QBEContext &ctx)
{
    int var = 0;
    if (impl.initializer && (!is<Constant>(impl.initializer) || !qbe_first_class_type(impl.initializer->bound_type))) {
        if (auto res = generate_qbe_node(impl.initializer, ctx); !res) {
            return res;
        } else {
            var = res.value();
        }
    }
    auto size = n->bound_type->size_of();
    char code = std::visit(
        overloads {
            [](IntType const &int_type) -> char {
                switch (int_type.width_bits) {
                case 8:
                    return 'b';
                case 16:
                    return 'h';
                case 32:
                    return 'w';
                default:
                    return 'l';
                }
            },
            [](FloatType const &float_type) -> char {
                return (float_type.width_bits == 64) ? 'd' : 's';
            },
            [](auto const &) -> char {
                NYI("VariableDeclaration");
            } },
        n->bound_type->description);
    ctx.text += std::format(L"    %{}$ = l alloc{} {}\n", impl.name, (size < 8) ? '4' : '8', size);
    if (impl.initializer) {
        ctx.text += std::format(L"    store{} ", code, impl.name);
        if (var != 0) {
            ctx.text += std::format(L"%v{}\n", var);
        } else {
            if (auto res = generate_qbe_node(impl.initializer, ctx); !res) {
                return res;
            }
        }
        ctx.text += std::format(L", %{}$\n", impl.name);
    }
    return var;
}

GenResult generate_qbe_node(ASTNode const &n, QBEContext &ctx)
{
    return std::visit(
        [&n, &ctx](auto const &impl) -> GenResult {
            return generate_qbe_node(n, impl, ctx);
        },
        n->node);
}

GenResult generate_qbe_nodes(ASTNodes const &nodes, QBEContext &ctx)
{
    for (auto const &n : nodes) {
        if (auto res = generate_qbe_node(n, ctx); !res) {
            return res;
        }
    }
    return 0;
}

GenResult generate_qbe(ASTNode const &n)
{
    QBEContext ctx {};
    return generate_qbe_node(n, ctx);
}
}
