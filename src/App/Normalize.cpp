/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <functional>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <string>

namespace Arwen {

template<class N>
ASTNode normalize(ASTNode const &n, N const &impl)
{
    return n;
}

template<>
ASTNode normalize(ASTNode const &n, BinaryExpression const &impl)
{
    auto make_expression_list = [&n]() -> ASTNode {
        ASTNodes                     nodes;
        std::function<void(ASTNode)> flatten;
        flatten = [&nodes, &flatten](ASTNode const &n) {
            if (auto const binary_expr = std::get_if<BinaryExpression>(&n->node); binary_expr != nullptr) {
                if (binary_expr->op == Operator::Sequence) {
                    flatten(binary_expr->lhs);
                    nodes.push_back(binary_expr->rhs);
                } else {
                    nodes.push_back(n);
                }
            } else {
                nodes.push_back(n);
            }
        };
        flatten(n);
        return make_node<ExpressionList>(n, nodes);
    };

    auto const_evaluate = [&n](ASTNode const &lhs, Operator op, ASTNode const &rhs) -> ASTNode {
        auto const &lhs_const = std::get_if<Constant>(&lhs->node);
        if (lhs_const == nullptr || !lhs_const->bound_value) {
            return n;
        }
        if (op == Operator::Cast) {
            if (is<TypeSpecification>(rhs)) {
                if (auto const cast_type = resolve(rhs); cast_type != nullptr) {
                    if (auto const coerced_maybe = lhs_const->bound_value.value().coerce(cast_type); coerced_maybe) {
                        return make_node<Constant>(n, *coerced_maybe);
                    }
                    n.repo->append(n->location, L"Cannot cast value to `{}`", cast_type);
                    return nullptr;
                }
            }
        }
        if (auto const &rhs_const = std::get_if<Constant>(&rhs->node); rhs_const != nullptr && rhs_const->bound_value) {
            auto ret = evaluate(lhs_const->bound_value.value(), op, rhs_const->bound_value.value());
            return make_node<Constant>(n, ret);
        }
        return n;
    };

    switch (impl.op) {
    case Operator::Call: {
        auto arg_list = normalize(impl.rhs);
        if (is<Void>(arg_list)) {
            arg_list = make_node<ExpressionList>(arg_list, ASTNodes {});
        }
        if (!is<ExpressionList>(arg_list)) {
            arg_list = make_node<ExpressionList>(arg_list, ASTNodes { arg_list });
        }
        assert(is<ExpressionList>(arg_list));
        arg_list = normalize(arg_list);
        auto call = make_node<Call>(n, normalize(impl.lhs), arg_list);
        return normalize(call);
    }
    case Operator::Sequence:
        return make_expression_list();
    default:
        if (assign_ops.contains(impl.op)) {
            auto const bin_expr = make_node<BinaryExpression>(
                n,
                normalize(impl.lhs),
                assign_ops[impl.op],
                normalize(impl.rhs));
            return make_node<BinaryExpression>(n, impl.lhs, Operator::Assign, normalize(bin_expr));
        }
        return const_evaluate(normalize(impl.lhs), impl.op, normalize(impl.rhs));
    }
}

template<>
ASTNode normalize(ASTNode const &n, Block const &impl)
{
    const_cast<ASTNode &>(n)->init_namespace();
    auto ret = make_node<Block>(n, normalize(impl.statements));
    return ret;
}

template<>
ASTNode normalize(ASTNode const &n, BoolConstant const &impl)
{
    return make_node<Constant>(n, Value { impl.value });
}

template<>
ASTNode normalize(ASTNode const &n, Comptime const &impl)
{
    Parser &parser = *(n.repo);
    auto    script = parse<Module>(parser, std::format("*Comptime* {}", n.id.value()), impl.script_text);

    if (!parser.errors.empty()) {
        log_error("Syntax error(s) found in @comptime block:");
        for (auto const &err : parser.errors) {
            log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
        }
        return n;
    }
    Module mod = get<Module>(script);
    auto   ret = make_node<Module>(n, mod.name, mod.source, normalize(mod.statements));
    info("@comptime block parsed");
    if (trace_on()) {
        dump(get<Module>(ret).statements, std::wcerr);
    }
    return ret;
}

template<>
ASTNode normalize(ASTNode const &n, DeferStatement const &impl)
{
    return make_node<DeferStatement>(n, normalize(impl.statement));
}

template<>
ASTNode normalize(ASTNode const &n, Embed const &impl)
{
    auto fname = as_utf8(impl.file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        return make_node<Constant>(n, make_value(contents));
    } else {
        n.error("Could not open `{}`: {}", fname, contents_maybe.error().to_string());
        return nullptr;
    }
}

template<>
ASTNode normalize(ASTNode const &n, EnumValue const &impl)
{
    return make_node<EnumValue>(n, impl.label, normalize(impl.value), normalize(impl.payload));
}

ASTNode normalize(ASTNode const &n, Enum const &impl)
{
    return make_node<Enum>(n, impl.name, normalize(impl.underlying_type), normalize(impl.values));
}

template<>
ASTNode normalize(ASTNode const &n, Error const &impl)
{
    return make_node<Error>(n, normalize(impl.expression));
}

template<>
ASTNode normalize(ASTNode const &n, ExpressionList const &impl)
{
    for (auto const &expr : impl.expressions) {
        assert(expr->status == ASTStatus::Normalized);
        if (expr->superceded_by == n) {
            expr->superceded_by = nullptr;
        }
    }
    return n;
}

template<>
ASTNode normalize(ASTNode const &n, ForStatement const &impl)
{
    n->init_namespace();
    auto range_expr = normalize(impl.range_expr);
    n->ns->register_variable(impl.range_variable, range_expr);
    auto ret = make_node<ForStatement>(n, impl.range_variable, range_expr, normalize(impl.statement));
    ret->ns->register_variable(impl.range_variable, range_expr);
    return n;
}

template<>
ASTNode normalize(ASTNode const &n, FunctionDeclaration const &impl)
{
    return make_node<FunctionDeclaration>(
        n,
        impl.name,
        normalize(impl.generics),
        normalize(impl.parameters),
        normalize(impl.return_type));
}

template<>
ASTNode normalize(ASTNode const &n, FunctionDefinition const &impl)
{
    n->init_namespace();
    n->ns->parent->ns->register_function(impl.name, n);
    auto ret = make_node<FunctionDefinition>(n, impl.name, normalize(impl.declaration), normalize(impl.implementation));
    ret->ns->parent->ns->register_function(impl.name, ret);
    return ret;
}

template<>
ASTNode normalize(ASTNode const &n, IfStatement const &impl)
{
    return make_node<IfStatement>(
        n,
        normalize(impl.condition),
        normalize(impl.if_branch),
        normalize(impl.else_branch));
}

template<>
ASTNode normalize(ASTNode const &n, Import const &impl)
{
    auto fname = impl.file_name;
    for (auto ix = 0; ix < fname.length(); ++ix) {
        if (fname[ix] == '.')
            fname[ix] = '/';
    }
    if (!fname.ends_with(L".arw")) {
        fname += L".arw";
    }
    if (auto contents_maybe = read_file_by_name<wchar_t>(as_utf8(fname)); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        auto        module = parse<Module>(*(n.repo), as_utf8(impl.file_name), std::move(contents));
        if (module) {
            module->location = n->location;
            return normalize(module);
        }
    } else {
        n.error(L"Could not open import file `{}`", fname);
    }
    return nullptr;
}

template<>
ASTNode normalize(ASTNode const &n, Include const &impl)
{
    auto fname = as_utf8(impl.file_name);
    if (auto contents_maybe = read_file_by_name<wchar_t>(fname); contents_maybe.has_value()) {
        auto const &contents = contents_maybe.value();
        auto        node = parse<Block>(*(n.repo), fname, std::move(contents));
        if (node) {
            node->location = n->location;
            return normalize(node);
        }
    } else {
        n.error(L"Could not open include file `{}`", impl.file_name);
    }
    return nullptr;
}

template<>
ASTNode normalize(ASTNode const &n, LoopStatement const &impl)
{
    return make_node<LoopStatement>(n, impl.label, normalize(impl.statement));
}

template<>
ASTNode normalize(ASTNode const &n, Module const &impl)
{
    n->init_namespace();
    return make_node<Module>(n, impl.name, impl.source, normalize(impl.statements));
}

template<>
ASTNode normalize(ASTNode const &n, Nullptr const &)
{
    return make_node<Constant>(n, Value { nullptr });
}

template<>
ASTNode normalize(ASTNode const &n, Number const &impl)
{
    switch (impl.number_type) {
    case NumberType::Decimal: {
        char      *end_ptr;
        auto const narrow_string = as_utf8(impl.number);
        auto const value = strtod(narrow_string.data(), &end_ptr);
        assert(end_ptr != narrow_string.data());
        return make_node<Constant>(n, Value { value });
    }
    default: {
        auto const value = string_to_integer<int64_t>(impl.number)
                               .or_else([&impl]() -> std::optional<int64_t> {
                                   fatal(L"Could not convert string `{}` to integer. This is unexpected", impl.number);
                                   return { 0 };
                               })
                               .value();
        return make_node<Constant>(n, Value { value });
    }
    }
}

template<>
ASTNode normalize(ASTNode const &n, Parameter const &impl)
{
    return make_node<Parameter>(n, impl.name, normalize(impl.type_name));
}

template<>
ASTNode normalize(ASTNode const &n, Program const &impl)
{
    n->init_namespace();
    for (auto const &t : TypeRegistry::the().types) {
        trace(L"Registering built-in type `{}`", t.id);
        n->ns->register_type(t.name, t.id);
    }
    auto                            statements = normalize(impl.statements);
    std::map<std::wstring, ASTNode> modules;
    for (auto &[name, mod] : impl.modules) {
        auto const normalized = normalize(mod);
        if (normalized != nullptr) {
            modules[std::get<Module>(mod->node).name] = normalized;
        }
    }
    return make_node<Program>(n, impl.name, modules, statements);
}

template<>
ASTNode normalize(ASTNode const &n, PublicDeclaration const &impl)
{
    return make_node<PublicDeclaration>(n, impl.name, normalize(impl.declaration));
}

template<>
ASTNode normalize(ASTNode const &n, QuotedString const &impl)
{
    switch (impl.quote_type) {
    case QuoteType::DoubleQuote: {
        assert(impl.string[0] == '"' && impl.string.back() == '"');
        std::wstring s;
        bool         escape { false };
        for (auto const ch : impl.string.substr(0, impl.string.length() - 1).substr(1)) {
            if (escape) {
                switch (ch) {
                case 'n':
                    s += '\n';
                    break;
                case 'r':
                    s += '\r';
                    break;
                case 't':
                    s += '\t';
                default:
                    s += ch;
                }
                escape = false;
            } else {
                if (ch == '\\') {
                    escape = true;
                } else {
                    s += ch;
                }
            }
        }
        return make_node<Constant>(n, make_value(s));
    }
    case QuoteType::SingleQuote:
        return make_node<Constant>(n, Value { impl.string[1] });
    default:
        UNREACHABLE();
    }
}

template<>
ASTNode normalize(ASTNode const &n, Return const &impl)
{
    return make_node<Return>(n, normalize(impl.expression));
}

template<>
ASTNode normalize(ASTNode const &n, StructMember const &impl)
{
    return make_node<StructMember>(n, impl.label, normalize(impl.member_type));
}

template<>
ASTNode normalize(ASTNode const &n, Struct const &impl)
{
    return make_node<Struct>(n, impl.name, normalize(impl.members));
}

template<>
ASTNode normalize(ASTNode const &n, TypeSpecification const &impl)
{
    auto description = std::visit(
        overloads {
            [](TypeNameNode const &d) -> TypeSpecificationDescription {
                return TypeNameNode { d.name, normalize(d.arguments) };
            },
            [](ReferenceDescriptionNode const &d) -> TypeSpecificationDescription {
                return ReferenceDescriptionNode { normalize(d.referencing) };
            },
            [](SliceDescriptionNode const &d) -> TypeSpecificationDescription {
                return SliceDescriptionNode { normalize(d.slice_of) };
            },
            [](ZeroTerminatedArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return ZeroTerminatedArrayDescriptionNode { normalize(d.array_of) };
            },
            [](ArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return ArrayDescriptionNode { normalize(d.array_of), d.size };
            },
            [](DynArrayDescriptionNode const &d) -> TypeSpecificationDescription {
                return DynArrayDescriptionNode { normalize(d.array_of) };
            },
            [](OptionalDescriptionNode const &d) -> TypeSpecificationDescription {
                return OptionalDescriptionNode { normalize(d.optional_of) };
            },
            [](ErrorDescriptionNode const &d) -> TypeSpecificationDescription {
                return ErrorDescriptionNode {
                    normalize(d.success),
                    normalize(d.error),
                };
            },
        },
        impl.description);
    return n;
}

template<>
ASTNode normalize(ASTNode const &n, UnaryExpression const &impl)
{
    auto normalized_operand = normalize(impl.operand);
    if (auto *operand_const = get_if<Constant>(normalized_operand); operand_const != nullptr) {
        auto res = evaluate(impl.op, operand_const->bound_value.value());
        return make_node<Constant>(n, res);
    }
    if (impl.op == Operator::Sizeof) {
        if (is<TypeSpecification>(normalized_operand)) {
            if (auto const type_maybe = resolve(normalized_operand); type_maybe != nullptr) {
                return make_node<Constant>(n, static_cast<int64_t>(type_maybe->size_of()));
            }
        }
    }
    return make_node<UnaryExpression>(n, impl.op, normalized_operand);
}

template<>
ASTNode normalize(ASTNode const &n, VariableDeclaration const &impl)
{
    return make_node<VariableDeclaration>(
        n,
        impl.name,
        normalize(impl.type_name),
        normalize(impl.initializer),
        impl.is_const);
}

template<>
ASTNode normalize(ASTNode const &n, WhileStatement const &impl)
{
    return make_node<WhileStatement>(n, impl.label, normalize(impl.condition), normalize(impl.statement));
}

template<>
ASTNode normalize(ASTNode const &n, Yield const &impl)
{
    return make_node<Yield>(n, impl.label, normalize(impl.statement));
}

/* ======================================================================== */

ASTNode normalize(ASTNode const &node)
{
    if (node == nullptr) {
        return nullptr;
    }
    trace(L"[->N] {}", node);
    ASTNode ret = node;
    if (node->status < ASTStatus::Normalized) {
        if (node->ns.has_value()) {
            node->id.repo->push_namespace(node);
        }
        char const *t = SyntaxNodeType_name(node->type());
        ret = std::visit([&node](auto impl) {
            return normalize(node, impl);
        },
            node->node);
        if (ret->ns.has_value()) {
            ret.repo->pop_namespace();
        }
        if (ret != nullptr && ret->status < ASTStatus::Normalized) {
            ret->status = ASTStatus::Normalized;
        }
    }
    trace(L"[N->] {}", ret);
    return ret;
}

ASTNodes normalize(ASTNodes const &nodes)
{
    ASTNodes normalized {};
    for (size_t ix = 0; ix < nodes.size(); ++ix) {
        auto ret = normalize(nodes[ix]);
        if (ret != nullptr) {
            normalized.push_back(ret);
        }
    }
    return normalized;
}

}
