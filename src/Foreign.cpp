/*
 * Copyright (c) 2023-2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <string_view>
#include <unistd.h>
#include <vector>

#include <IR/Foreign.h>
#include <Logging.h>
#include <Resolve.h>
#include <Result.h>
#include <Type/Type.h>
#include <Type/Value.h>

struct Trampoline {
    void (*fnc)();
    union {
        uint64_t    i;
        void const *ptr;
    } x[8];
    double d[8];
    union {
        uint64_t i;
        void    *p;
    } ret;
    double ret_flt;
};

extern "C" {
int trampoline(Trampoline *trampoline);
}

namespace Arwen {

Result<Value, ResolveError> foreign_call(std::string_view name, std::vector<Value> const &values, PrimitiveType ret_type)
{
    if (values.size() > 8) {
        fatal("Can't do foreign calls with more than 8 parameters");
    }
    Trampoline tramp;
    auto       fnc = TRY_EVAL(Resolver::get_resolver().resolve<void()>(name));
    if (!fnc) {
        fatal("Function '{}' not found", name);
    }
    tramp.fnc = *(fnc.target<void (*)()>());

    // Stage A - Initialization
    // This stage is performed exactly once, before processing of the arguments
    // commences.

    // A.1 The Next General-purpose Register Number (NGRN) is set to zero.
    size_t ngrn = 0;
    // A.2 The Next SIMD and Floating-point Register Number (NSRN) is set to
    // zero.
    size_t nsrn = 0;
    // A.3 The Next Scalable Predicate Register Number (NPRN) is set to zero.
    size_t nprn = 0;
    // A.4 The next stacked argument address (NSAA) is set to the current
    // stack-pointer value (SP).
    size_t nsaa = 0;

    for (auto const &value : values) {
        auto t = TypeRegistry::the()[value.type()].decay();

        // Stage B – Pre-padding and extension of arguments
        // For each argument in the list the first matching rule from the
        // following list is applied. If no rule matches the argument is used
        // unmodified.

        // B.1 If the argument type is a Pure Scalable Type, no change is made
        // at this stage.

        // B.2 If the argument type is a Composite Type whose size cannot be
        // statically determined by both the caller and the callee, the
        // argument is copied to memory and the argument is replaced by a
        // pointer to the copy. (There are no such types in C/C++ but they
        // exist in other languages or in language extensions).

        // B.3 If the argument type is an HFA or an HVA, then the argument is
        // used unmodified.
        // TODO

        // B.4 If the argument type is a Composite Type that is larger than 16
        // bytes, then the argument is copied to memory allocated by the caller
        // and the argument is replaced by a pointer to the copy.
        // TODO

        // B.5 If the argument type is a Composite Type then the size of the
        // argument is rounded up to the nearest multiple of 8 bytes.

        // B.6 If the argument is an alignment adjusted type its value is passed
        // as a copy of the actual value. The copy will have an alignment
        // defined as follows:
        //     • For a Fundamental Data Type, the alignment is the natural
        //       alignment of that type, after any promotions.
        //     • For a Composite Type, the alignment of the copy will have
        //       8-byte alignment if its natural alignment is ≤ 8 and 16-byte
        //       alignment if its natural alignment is ≥ 16.
        // The alignment of the copy is used for applying marshaling rules.

        // Stage C – Assignment of arguments to registers and stack
        // For each argument in the list the following rules are applied in
        // turn until the argument has been allocated. When an argument is
        // assigned to a register any unused bits in the register have
        // unspecified value. When an argument is assigned to a stack slot any
        // unused padding bytes have unspecified value.

        // C.1 If the argument is a Half-, Single-, Double- or Quad- precision
        // Floating-point or short vector type and the NSRN is less than 8, then
        // the argument is allocated to the least significant bits of register
        // v[NSRN]. The NSRN is incremented by one. The argument has now been
        // allocated.
        if (value.index() == static_cast<size_t>(PrimitiveType::Double) && nsrn < 8) {
            tramp.d[nsrn] = value.value<double>();
            ++nsrn;
            continue;
        }

        // C.2 If the argument is an HFA or an HVA and there are sufficient
        // unallocated SIMD and Floating-point registers (NSRN + number of
        // members ≤ 8), then the argument is allocated to SIMD and
        // Floating-point registers (with one register per member of the HFA or
        // HVA). The NSRN is incremented by the number of registers used. The
        // argument has now been allocated.
        // TODO

        // C.3 If the argument is an HFA or an HVA then the NSRN is set to 8 and
        // the size of the argument is rounded up to the nearest multiple of 8
        // bytes.
        // TODO

        // C.4 If the argument is an HFA, an HVA, a Quad-precision
        // Floating-point or short vector type then the NSAA is rounded up to
        // the next multiple of 8 if its natural alignment is ≤ 8 or the next
        // multiple of 16 if its natural alignment is ≥ 16.
        // TODO

        // C.5 If the argument is a Half- or Single- precision Floating Point
        // type, then the size of the argument is set to 8 bytes. The effect is
        // as if the argument had been copied to the least significant bits of a
        // 64-bit register and the remaining bits filled with unspecified
        // values.
        // Not supported

        // C.6 If the argument is an HFA, an HVA, a Half-, Single-, Double- or
        // Quad- precision Floating-point or short vector type, then the
        // argument is copied to memory at the adjusted NSAA. The NSAA is
        // incremented by the size of the argument. The argument has now been
        // allocated.
        // TODO

        // C.7 If the argument is a Pure Scalable Type that consists of NV
        // Scalable Vector Types and NP Scalable Predicate Types, if the
        // argument is named, if NSRN+NV ≤ 8, and if NPRN+NP ≤ 4, then the
        // Scalable Vector Types are allocated in order to
        // z[NSRN]...z[NSRN+NV-1] inclusive and the Scalable Predicate Types are
        // allocated in order to p[NPRN]...p[NPRN+NP-1] inclusive. The NSRN is
        // incremented by NV and the NPRN is incremented by NP. The argument has
        // now been allocated.
        // TODO

        // C.8 If the argument is a Pure Scalable Type that has not been
        // allocated by the rules above, then the argument is copied to memory
        // allocated by the caller and the argument is replaced by a pointer to
        // the copy (as for B.4 above). The argument is then allocated according
        // to the rules below.
        // TODO

        // C.9 If the argument is an Integral or Pointer Type, the size of the
        // argument is less than or equal to 8 bytes and the NGRN is less than
        // 8, the argument is copied to the least significant bits in x[NGRN].
        // The NGRN is incremented by one. The argument has now been allocated.
        if (ngrn < 8) {
            auto type = value.type();
            auto t = TypeRegistry::the()[type].decay();
            if (t.typespec.tag() == TypeKind::Primitive) {
                switch (type) {
                case BoolType:
                    tramp.x[ngrn++].i = value.value<bool>();
                    break;
#undef S
#define S(T, C, ...)                       \
    case T##Type:                          \
        tramp.x[ngrn++].i = value.as<C>(); \
        break;
                IntegerTypes(S)
#undef S
                    case PtrType:
                    tramp.x[ngrn++].ptr = value.value<void *>();
                    break;
                default:
                    UNREACHABLE();
                }
                continue;
            }
            if (t.typespec.tag() == TypeKind::Pointer) {
                tramp.x[ngrn++].ptr = value.value<void *>();
                continue;
            }
            if (t.typespec.tag() == TypeKind::Slice) {
                assert(ngrn < 7);
                SliceValue slice = value.value<SliceValue>();
                tramp.x[ngrn++].i = slice.len;
                tramp.x[ngrn++].ptr = slice.ptr;
                continue;
            }
            UNREACHABLE();
        }

        // C.10 If the argument has an alignment of 16 then the NGRN is rounded
        // up to the next even number.

        // C.11 If the argument is an Integral Type, the size of the argument
        // is equal to 16 and the NGRN is less than 7, the argument is copied to
        // x[NGRN] and x[NGRN+1]. x[NGRN] shall contain the lower addressed
        // double-word of the memory representation of the argument. The NGRN
        // is incremented by two. The argument has now been allocated.

        // C.12 If the argument is a Composite Type and the size in double-words
        // of the argument is not more than 8 minus NGRN, then the argument is
        // copied into consecutive general-purpose registers, starting at
        // x[NGRN]. The argument is passed as though it had been loaded into the
        // registers from a double-word-aligned address with an appropriate
        // sequence of LDR instructions loading consecutive registers from
        // memory (the contents of any unused parts of the registers are
        // unspecified by this standard). The NGRN is incremented by the number
        // of registers used. The argument has now been allocated.

        // C.13 The NGRN is set to 8.

        // C.14 The NSAA is rounded up to the larger of 8 or the Natural
        // Alignment of the argument’s type.

        // C.15 If the argument is a composite type then the argument is copied
        // to memory at the adjusted NSAA. The NSAA is incremented by the size
        // of the argument. The argument has now been allocated.

        // C.16 If the size of the argument is less than 8 bytes then the size
        // of the argument is set to 8 bytes. The effect is as if the argument
        // was copied to the least significant bits of a 64-bit register and the
        // remaining bits filled with unspecified values.

        // C.17 The argument is copied to memory at the adjusted NSAA. The NSAA
        // is incremented by the size of the argument. The argument has now been
        // allocated.
    }

    // std::println("fnc {}  {}", reinterpret_cast<void const *>(arwen$write), reinterpret_cast<void const *>(tramp.fnc));
    // for (auto ix = 0; ix < ngrn; ++ix) {
    //     std::println("x{}  {}", ix, tramp.x[ix].ptr);
    // }

    int trampoline_result = trampoline(&tramp);
    if (trampoline_result) {
        fatal("Error executing '{}'. Trampoline returned {}", name, trampoline_result);
    }
    switch (static_cast<TypeReference>(ret_type)) {
    case BoolType:
        return Value { static_cast<bool>(tramp.ret.i) };
    case DoubleType:
        return Value { tramp.ret_flt };
    case FloatType:
        return Value { static_cast<f32>(tramp.ret_flt) };
#undef S
#define S(T, C, ...) \
    case T##Type:    \
        return Value { static_cast<C>(tramp.ret.i) };
    IntegerTypes(S)
#undef S
        case PtrType:
        return Value { tramp.ret.p };
        default : UNREACHABLE();
    }
}

}
