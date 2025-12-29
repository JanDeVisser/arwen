/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <expected>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include <Util/Logging.h>

#include <App/SyntaxNode.h>

namespace Arwen::QBE {

using namespace Util;

enum class LocalType {
    Unknown,
    Value,
    Reference,
};

struct Local {
    LocalType type = LocalType::Unknown;
    int       var;

    static Local value(int var);
    static Local ref(int var);
};

#define ILBASETYPES(S) \
    S(V, 0x00, void)   \
    S(B, 0x04, b)      \
    S(SB, 0x05, sb)    \
    S(UB, 0x06, ub)    \
    S(H, 0x08, h)      \
    S(SH, 0x09, sh)    \
    S(UH, 0x0A, uh)    \
    S(W, 0x10, w)      \
    S(SW, 0x11, sw)    \
    S(UW, 0x12, uw)    \
    S(L, 0x20, l)      \
    S(S, 0x40, s)      \
    S(D, 0x80, d)

enum class ILBaseType {
#undef S
#define S(T, Code, Str) T = Code,
    ILBASETYPES(S)
#undef S
};

using ILType = std::variant<ILBaseType, std::wstring>;

std::wostream &operator<<(std::wostream &os, ILBaseType const &type);
std::wostream &operator<<(std::wostream &os, ILType const &type);

enum class ILInstructionType {
    Alloc,
    Blit,
    Call,
    Cast,
    Copy,
    Expr,
    Hlt,
    Jmp,
    Jnz,
    Label,
    Load,
    Phi,
    Ret,
    Store,
    VaArg,
    VaStart,
};

#define ILOPERATIONS(S) \
    S(Add, add)         \
    S(And, and)         \
    S(Div, div)         \
    S(Mul, mul)         \
    S(Neg, neg)         \
    S(Or, or)           \
    S(Mod, rem)         \
    S(Sar, sar)         \
    S(Shl, shl)         \
    S(Shr, shr)         \
    S(Sub, sub)         \
    S(UDiv, udiv)       \
    S(UMod, urem)       \
    S(Xor, xor)         \
    S(Equals, eq)       \
    S(NotEqual, ne)     \
    S(GreaterEqual, ge) \
    S(Greater, gt)      \
    S(LessEqual, le)    \
    S(Less, lt)

enum class ILOperation {
#undef S
#define S(Op, Str) Op,
    ILOPERATIONS(S)
#undef S
};

struct ILValue {
    struct Variable {
        std::wstring name;
    };

    struct Parameter {
        std::wstring name;
    };

    struct Global {
        std::wstring name;
    };

    struct Literal {
        std::wstring literal;
    };

    using ILValueInner = std::variant<
        Local,
        Global,
        Literal,
        Variable,
        Parameter,
        int64_t,
        double>;

    template<typename TypeDesc>
    static ILValue local(int var, TypeDesc td)
    {
        return { ILType { std::move(td) }, Local::value(var) };
    }

    static ILValue local_ref(int var)
    {
        return { ILType { ILBaseType::L }, Local::ref(var) };
    }

    template<typename TypeDesc>
    static ILValue global(std::wstring name, TypeDesc td)
    {
        return { ILType { std::move(td) }, Global { std::move(name) } };
    }

    template<typename TypeDesc>
    static ILValue literal(std::wstring literal, TypeDesc td)
    {
        return { ILType { std::move(td) }, Literal { std::move(literal) } };
    }

    static ILValue variable(std::wstring name)
    {
        return { ILType { ILBaseType::L }, Variable { std::move(name) } };
    }

    template<typename TypeDesc>
    static ILValue parameter(std::wstring name, TypeDesc td)
    {
        return { ILType { std::move(td) }, Parameter { std::move(name) } };
    }

    static ILValue string(int str_id)
    {
        return global(std::format(L"str_{}", str_id), ILBaseType::L);
    }

    static ILValue cstring(int str_id)
    {
        return global(std::format(L"cstr_{}", str_id), ILBaseType::L);
    }

    template<typename TypeDesc>
    static ILValue float_val(double d, TypeDesc td)
    {
        return { ILType { std::move(td) }, d };
    }

    template<typename TypeDesc>
    static ILValue integer(int64_t i, TypeDesc td)
    {
        return { ILType { std::move(td) }, i };
    }

    static ILValue null()
    {
        return { ILBaseType::V, 0 };
    }

    ILType       type;
    ILValueInner inner;
};

std::wostream &operator<<(std::wostream &os, ILValue const &value);
std::wostream &operator<<(std::wostream &os, ILOperation const &op);

struct AllocDef {
    size_t  alignment;
    size_t  bytes;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, AllocDef const &impl);
};

struct BlitDef {
    ILValue src;
    ILValue dest;
    size_t  bytes;

    friend std::wostream &operator<<(std::wostream &os, BlitDef const &impl);
};

struct CallDef {
    std::wstring         name;
    ILValue              target;
    std::vector<ILValue> args;

    friend std::wostream &operator<<(std::wostream &os, CallDef const &impl);
};

struct CastDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, CastDef const &impl);
};

struct CopyDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, CopyDef const &impl);
};

struct ExprDef {
    ILValue     lhs;
    ILValue     rhs;
    ILOperation op;
    ILValue     target;

    friend std::wostream &operator<<(std::wostream &os, ExprDef const &impl);
};

struct HltDef {
    friend std::wostream &operator<<(std::wostream &os, HltDef const &impl);
};

struct JmpDef {
    int label;

    friend std::wostream &operator<<(std::wostream &os, JmpDef const &impl);
};

struct JnzDef {
    ILValue expr;
    int     on_true;
    int     on_false;

    friend std::wostream &operator<<(std::wostream &os, JnzDef const &impl);
};

struct LabelDef {
    int label;

    friend std::wostream &operator<<(std::wostream &os, LabelDef const &impl);
};

struct LoadDef {
    ILValue pointer;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, LoadDef const &impl);
};

struct PhiDef {
    struct PhiArgDef {
        int     come_from;
        ILValue expr;

        friend std::wostream &operator<<(std::wostream &os, PhiArgDef const &impl);
    };

    std::vector<PhiArgDef> args;
    ILBaseType             type;
    ILValue                target;

    friend std::wostream &operator<<(std::wostream &os, PhiDef const &impl);
};

struct RetDef {
    std::optional<ILValue> expr;

    friend std::wostream &operator<<(std::wostream &os, RetDef const &impl);
};

struct StoreDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, StoreDef const &impl);
};

struct VaArgDef {
    std::wstring arglist;
    ILBaseType   type;
    ILValue      target;

    friend std::wostream &operator<<(std::wostream &os, VaArgDef const &impl);
};

struct VaStartDef {
    std::wstring arglist;

    friend std::wostream &operator<<(std::wostream &os, VaStartDef const &impl);
};

using ILInstructionImpl = std::variant<
    AllocDef,
    BlitDef,
    CallDef,
    CastDef,
    CopyDef,
    ExprDef,
    HltDef,
    JmpDef,
    JnzDef,
    LabelDef,
    LoadDef,
    PhiDef,
    RetDef,
    StoreDef,
    VaArgDef,
    VaStartDef>;

struct ILInstruction {
    ILInstructionImpl impl;

    friend std::wostream &operator<<(std::wostream &os, ILInstruction const &instr)
    {
        std::visit([&os](auto const &impl) { os << impl << '\n'; }, instr.impl);
        return os;
    }
};

struct ILParameter {
    std::wstring name;
    ILType       type;
};

struct ILFunction {
    std::wstring               name;
    ILType                     return_type;
    bool                       exported { false };
    std::vector<ILParameter>   parameters;
    std::vector<ILInstruction> instructions;
    friend std::wostream      &operator<<(std::wostream &os, ILFunction const &function);
};

struct ILFile {
    std::wstring              name;
    std::vector<pType>        types;
    std::vector<std::wstring> strings;
    std::vector<std::string>  cstrings;
    std::vector<ILFunction>   functions;
    bool                      has_exports { false };
    bool                      has_main { false };
    friend std::wostream     &operator<<(std::wostream &os, ILFunction const &function);
};

struct ILProgram {
    std::wstring        name;
    std::vector<ILFile> files;
};

using GenResult = std::expected<ILValue, std::wstring>;
GenResult generate_qbe(ASTNode const &node);

}
