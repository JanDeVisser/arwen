#include <concepts>
#include <cstdint>
#include <iostream>
#include <memory>
#include <print>
#include <string>
#include <string_view>
#include <vector>

#include <Util/IO.h>
#include <Util/Lexer.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Token.h>

namespace Arwen {

using namespace Util;

#define ArwenKeywords(S)       \
    S(AssignAnd, "&=")         \
    S(AssignDecrement, "-=")   \
    S(AssignDivide, "/=")      \
    S(AssignIncrement, "+=")   \
    S(AssignModulo, "%=")      \
    S(AssignMultiply, "*=")    \
    S(AssignOr, "|=")          \
    S(AssignShiftLeft, "<<=")  \
    S(AssignShiftRight, ">>=") \
    S(AssignXor, "^=")         \
    S(Equals, "==")            \
    S(GreaterEqual, ">=")      \
    S(LessEqual, "<=")         \
    S(LogicalAnd, "&&")        \
    S(LogicalOr, "||")         \
    S(NotEqual, "!=")          \
    S(ShiftLeft, "<<")         \
    S(ShiftRight, ">>")

enum class ArwenKeyword {
#undef S
#define S(KW, S) KW,
    ArwenKeywords(S)
#undef S
};

using Precedence = uint16_t;

enum class Position {
    Prefix,
    Infix,
    Postfix,
};

enum class Associativity {
    Left,
    Right,
};

#define Operators(S)    \
    S(Add)              \
    S(Assign)           \
    S(AssignAnd)        \
    S(AssignDecrement)  \
    S(AssignDivide)     \
    S(AssignIncrement)  \
    S(AssignModulo)     \
    S(AssignMultiply)   \
    S(AssignOr)         \
    S(AssignShiftLeft)  \
    S(AssignShiftRight) \
    S(AssignXor)        \
    S(BinaryAnd)        \
    S(BinaryOr)         \
    S(BinaryXor)        \
    S(Call)             \
    S(Divide)           \
    S(Equals)           \
    S(Greater)          \
    S(GreaterEqual)     \
    S(Idempotent)       \
    S(Invert)           \
    S(Less)             \
    S(LessEqual)        \
    S(LogicalAnd)       \
    S(LogicalOr)        \
    S(Modulo)           \
    S(Multiply)         \
    S(Negate)           \
    S(NotEqual)         \
    S(Sequence)         \
    S(ShiftLeft)        \
    S(ShiftRight)       \
    S(Subtract)

enum class Operator : int {
#undef S
#define S(O) O,
    Operators(S)
#undef S
};

char const *Operator_name(Operator op)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return #O;
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

using OperatorSymbol = std::variant<wchar_t, ArwenKeyword>;

struct OperatorDef {
    Operator       op;
    OperatorSymbol sym;
    Precedence     precedence;
    Position       position { Position::Infix };
    Associativity  associativity { Associativity::Left };
};

#define SyntaxNodeTypes(S) \
    S(BinaryExpression)    \
    S(Block)               \
    S(ExpressionList)      \
    S(Identifier)          \
    S(Module)              \
    S(Number)              \
    S(QuotedString)        \
    S(UnaryExpression)

enum class SyntaxNodeType {
#undef S
#define S(T) T,
    SyntaxNodeTypes(S)
#undef S
};

char const *SyntaxNodeType_name(SyntaxNodeType type)
{
    switch (type) {
#undef S
#define S(T)                \
    case SyntaxNodeType::T: \
        return #T;
        SyntaxNodeTypes(S)
#undef S
            default : UNREACHABLE();
    }
}

void print_indent(int indent)
{
    printf("%*.*s", indent, indent, "");
}

using pSyntaxNode = std::shared_ptr<struct SyntaxNode>;
using SyntaxNodes = std::vector<pSyntaxNode>;
using pBoundNode = std::shared_ptr<struct BoundNode>;

struct SyntaxNode {
    TokenLocation  location;
    SyntaxNodeType type;

    SyntaxNode() = delete;
    SyntaxNode(SyntaxNodeType type)
        : type(type)
    {
    }

    void dump(int indent = 0)
    {
        print_indent(indent);
        std::cout << SyntaxNodeType_name(type) << " ";
        header();
        std::cout << std::endl;
        dump_node(indent);
    }

    virtual void header()
    {
    }

    virtual void dump_node(int indent)
    {
    }

    virtual pBoundNode bind() = 0;
};

template<class Node, typename... Args>
    requires std::derived_from<Node, SyntaxNode>
static std::shared_ptr<Node> make_node(Args &&...args)
{
    return std::make_shared<Node>(args...);
}

struct BinaryExpression : SyntaxNode {
    pSyntaxNode lhs;
    Operator    op;
    pSyntaxNode rhs;

    BinaryExpression(pSyntaxNode lhs, Operator op, pSyntaxNode rhs)
        : SyntaxNode(SyntaxNodeType::BinaryExpression)
        , lhs(lhs)
        , op(op)
        , rhs(rhs)
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::cout << Operator_name(op);
    }

    void dump_node(int indent) override
    {
        lhs->dump(indent + 4);
        rhs->dump(indent + 4);
    }
};

struct Block : SyntaxNode {
    SyntaxNodes statements;

    Block(SyntaxNodes statements)
        : SyntaxNode(SyntaxNodeType::Block)
        , statements(std::move(statements))
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void dump_node(int indent) override
    {
        for (auto const &stmt : statements) {
            stmt->dump(indent + 4);
        }
    }
};

struct ExpressionList : SyntaxNode {
    SyntaxNodes expressions;

    ExpressionList(SyntaxNodes expressions)
        : SyntaxNode(SyntaxNodeType::ExpressionList)
        , expressions(std::move(expressions))
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void dump_node(int indent) override
    {
        for (auto const &stmt : expressions) {
            stmt->dump(indent + 4);
        }
    }
};

struct Identifier : SyntaxNode {
    std::wstring identifier;

    Identifier(std::wstring_view identifier)
        : SyntaxNode(SyntaxNodeType::Identifier)
        , identifier(identifier)
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::wcout << identifier;
    }
};

struct Module : SyntaxNode {
    std::string name;
    SyntaxNodes statements;

    Module(std::string_view name, SyntaxNodes statements)
        : SyntaxNode(SyntaxNodeType::Module)
        , name(name)
        , statements(std::move(statements))
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::cout << name;
    }

    void dump_node(int indent) override
    {
        for (auto const &stmt : statements) {
            stmt->dump(indent + 4);
        }
    }
};

struct Number : SyntaxNode {
    std::wstring number;
    NumberType   number_type;

    Number(std::wstring_view number, NumberType type)
        : SyntaxNode(SyntaxNodeType::Number)
        , number(number)
        , number_type(type)
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::wcout << number << L" ";
        std::cout << NumberType_name(number_type);
    }
};

struct QuotedString : SyntaxNode {
    std::wstring string;
    QuoteType    quote_type;

    QuotedString(std::wstring_view str, QuoteType type)
        : SyntaxNode(SyntaxNodeType::QuotedString)
        , string(str)
        , quote_type(type)
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::wcout << string;
    }
};

struct UnaryExpression : SyntaxNode {
    Operator    op;
    pSyntaxNode operand;

    UnaryExpression(Operator op, pSyntaxNode operand)
        : SyntaxNode(SyntaxNodeType::UnaryExpression)
        , op(op)
        , operand(operand)
    {
    }

    pBoundNode bind() override
    {
        return nullptr;
    }

    void header() override
    {
        std::cout << Operator_name(op);
    }

    void dump_node(int indent) override
    {
        operand->dump_node(indent + 4);
    }
};

struct Parser {
    using ArwenLexer = Lexer<std::wstring_view, EnumKeywords<std::wstring_view, NoKeywordCategory, ArwenKeyword>>;
    using Token = ArwenLexer::Token;

    static std::vector<OperatorDef> operators;
    std::wstring_view               text;
    ArwenLexer                      lexer {};
    std::shared_ptr<Module>         module;

    Parser() = default;

    pSyntaxNode parse(std::string_view name, std::wstring_view text)
    {
        this->text = text;
        lexer.push_source(text);

        SyntaxNodes statements;
        parse_statements(statements);
        return make_node<Module>(name, statements);
    }

    void parse_statements(SyntaxNodes &statements)
    {
        for (bool done = false; !done;) {
            auto const &t = lexer.peek();
            switch (t.kind) {
            case TokenKind::EndOfFile:
                done = true;
                break;
            case TokenKind::Identifier:
            case TokenKind::Number:
            case TokenKind::QuotedString:
                if (auto expr = parse_top_expression(); expr) {
                    statements.push_back(expr);
                }
                break;
            case TokenKind::Symbol:
                switch (t.symbol_code()) {
                case ';':
                    lexer.lex();
                    break;
                case '{': {
                    SyntaxNodes block;
                    parse_statements(block);
                    if (auto const &t = lexer.peek(); t.matches_symbol('}')) {
                        lexer.lex();
                        statements.push_back(make_node<Block>(block));
                    }
                } break;
                default:
                    if (auto expr = parse_top_expression(); expr) {
                        statements.push_back(expr);
                    } else {
                        std::cerr << "Unexpected symbol '" << t.symbol_code() << "'" << std::endl;
                        lexer.lex();
                    }
                    break;
                }
            default:
                lexer.lex();
                break;
            }
        }
    }

    std::wstring_view text_of(Token const &token)
    {
        return text.substr(token.location.index, token.location.length);
    }

    pSyntaxNode parse_top_expression()
    {
        auto lhs = parse_primary();
        return (lhs) ? parse_expr(lhs, 0) : nullptr;
    }

    pSyntaxNode parse_primary()
    {
        auto       &token = lexer.peek();
        pSyntaxNode ret { nullptr };
        switch (token.kind) {
        case TokenKind::Number: {
            ret = make_node<Number>(text_of(token), token.number_type());
            lexer.lex();
            break;
        }
        case TokenKind::QuotedString: {
            ret = make_node<QuotedString>(text_of(token), token.quoted_string().quote_type);
            lexer.lex();
            break;
        }
        case TokenKind::Identifier: {
            ret = make_node<Identifier>(text_of(token));
            lexer.lex();
            break;
        }
        case TokenKind::Symbol: {
            if (token.symbol_code() == L'(') {
                lexer.lex();
                ret = parse_top_expression();
                if (!lexer.peek().matches_symbol(')')) {
                    std::cerr << "Expected ')'" << std::endl;
                    return nullptr;
                }
                lexer.lex();
                break;
            }
            if (auto const op_maybe = check_prefix_op(); op_maybe) {
                auto &op = *op_maybe;
                lexer.lex();
                auto operand = parse_primary();
                if (!operand) {
                    std::cerr << "Expected operand following prefix operator " << Operator_name(op.op) << std::endl;
                    return nullptr;
                }
                ret = make_node<UnaryExpression>(op.op, operand);
            }
        }
        default:
            ret = nullptr;
        }
        if (ret == nullptr) {
            std::println("parse_primary: nullptr!");
        }
        return ret;
    }

    pSyntaxNode parse_expr(pSyntaxNode lhs, Precedence min_prec)
    {
        for (auto op_maybe = check_binop(min_prec); op_maybe; op_maybe = check_binop(min_prec)) {
            auto const &curr_op = *op_maybe;
            if (curr_op.op == Operator::Call) {
                // Don't lex the '(' sp parse_primary will return a
                // single expression, probably a binop with op = ','.
                auto param_list = parse_primary();
                return make_node<BinaryExpression>(lhs, curr_op.op, param_list);
            }
            lexer.lex();
            auto rhs = parse_primary();
            if (!rhs) {
                std::cerr << "Expected right hand side operand following infix operator " << Operator_name(curr_op.op) << std::endl;
                return nullptr;
            }
            for (auto next_op_maybe = check_binop(curr_op.precedence); next_op_maybe; next_op_maybe = check_binop(curr_op.precedence)) {
                auto const &next_op = *next_op_maybe;
                auto        next_prec = curr_op.precedence + (next_op.precedence > curr_op.precedence) ? 1 : 0;
                rhs = parse_expr(rhs, next_prec);
                if (!rhs) {
                    std::cerr << "Expected right hand side operand following infix operator " << Operator_name(next_op.op) << std::endl;
                    return nullptr;
                }
            }
            if (curr_op.op == Operator::Call) {
            }
            lhs = make_node<BinaryExpression>(lhs, curr_op.op, rhs);
        }
        return lhs;
    }

    std::optional<OperatorDef> check_binop(Precedence min_prec)
    {
        auto const &token = lexer.peek();
        if (token.kind != TokenKind::Symbol && token.kind != TokenKind::Keyword) {
            return {};
        }
        for (auto const &def : operators) {
            if (def.position != Position::Infix) {
                continue;
            }
            if (!std::visit(overloads {
                                [&token](wchar_t sym) { return token.matches_symbol(sym); },
                                [&token](ArwenKeyword sym) { return token.matches_keyword(sym); } },
                    def.sym)) {
                continue;
            }
            if (def.precedence < min_prec) {
                continue;
            }
            if (def.associativity == Associativity::Right || def.precedence > min_prec) {
                return def;
            }
            return def;
        }
        return {};
    }

    std::optional<OperatorDef> check_prefix_op()
    {
        auto const &token = lexer.peek();
        if (token.kind != TokenKind::Symbol && token.kind != TokenKind::Keyword) {
            return {};
        }
        for (auto const &def : operators) {
            if (def.position != Position::Prefix) {
                continue;
            }
            if (!std::visit(overloads {
                                [&token](wchar_t sym) { return token.matches_symbol(sym); },
                                [&token](ArwenKeyword sym) { return token.matches_keyword(sym); } },
                    def.sym)) {
                continue;
            }
            return def;
        }
        return {};
    }

    void parse_block()
    {
    }
};

std::vector<OperatorDef> Parser::operators {
    { Operator::Add, '+', 11 },
    { Operator::Assign, '=', 1, Position::Infix, Associativity::Right },
    { Operator::AssignAnd, ArwenKeyword::AssignAnd, 1, Position::Infix, Associativity::Right },
    { Operator::AssignDecrement, ArwenKeyword::AssignDecrement, 1, Position::Infix, Associativity::Right },
    { Operator::AssignDivide, ArwenKeyword::AssignDivide, 1, Position::Infix, Associativity::Right },
    { Operator::AssignIncrement, ArwenKeyword::AssignIncrement, 1, Position::Infix, Associativity::Right },
    { Operator::AssignModulo, ArwenKeyword::AssignModulo, 1, Position::Infix, Associativity::Right },
    { Operator::AssignMultiply, ArwenKeyword::AssignMultiply, 1, Position::Infix, Associativity::Right },
    { Operator::AssignOr, ArwenKeyword::AssignOr, 1, Position::Infix, Associativity::Right },
    { Operator::AssignShiftLeft, ArwenKeyword::AssignShiftLeft, 1, Position::Infix, Associativity::Right },
    { Operator::AssignShiftRight, ArwenKeyword::AssignShiftRight, 1, Position::Infix, Associativity::Right },
    { Operator::AssignXor, ArwenKeyword::AssignXor, 1, Position::Infix, Associativity::Right },
    { Operator::Call, '(', 15 },
    { Operator::Divide, '/', 12 },
    { Operator::Equals, ArwenKeyword::Equals, 8 },
    { Operator::Greater, '>', 8 },
    { Operator::GreaterEqual, ArwenKeyword::GreaterEqual, 8 },
    { Operator::Idempotent, '+', 14, Position::Prefix, Associativity::Right },
    { Operator::Invert, '!', 14, Position::Prefix, Associativity::Right },
    { Operator::Less, '<', 8 },
    { Operator::LessEqual, ArwenKeyword::LessEqual, 8 },
    { Operator::Modulo, '%', 12 },
    { Operator::Multiply, '*', 12 },
    { Operator::Negate, '-', 14, Position::Prefix, Associativity::Right },
    { Operator::NotEqual, ArwenKeyword::NotEqual, 8 },
    { Operator::Sequence, ',', 0 },
    { Operator::ShiftLeft, ArwenKeyword::ShiftLeft, 10 },
    { Operator::ShiftRight, ArwenKeyword::ShiftRight, 10 },
    { Operator::Subtract, '-', 11 },
};

void usage()
{
    std::cerr << "arwen -" << std::endl;
}

[[noreturn]] void usage(std::string_view msg)
{
    usage();
    std::cerr << std::endl
              << msg << std::endl;
    exit(1);
}

void compile_file(std::string_view file_name)
{
    auto contents_maybe = read_file_by_name<wchar_t>(file_name);
    if (contents_maybe.is_error()) {
        std::cerr << contents_maybe.error().to_string() << std::endl;
    }

    auto  &contents = contents_maybe.value();
    Parser parser;
    auto   mod = parser.parse(file_name, contents);
    if (mod) {
        mod->dump();
        return;
    }
    std::cerr << "Syntax error" << std::endl;
}

int main(int argc, char const **argv)
{
    int cmd_ix = parse_options(argc, argv);
    if (cmd_ix >= argc) {
        usage("No command specified");
    }
    if (!strcmp(argv[cmd_ix], "compile")) {
        if (cmd_ix >= argc - 1) {
            usage("arwen compile <main file name>");
        }
        compile_file(argv[cmd_ix + 1]);
    }
    return 0;
}

} // namespace Arwen

int main(int argc, char const **argv)
{
    return Arwen::main(argc, argv);
}
