/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Resolve.h>

#include <App/Type.h>
#include <App/Value.h>

#include <Interp/Native.h>

extern "C" {
struct Trampoline {
    Arwen::void_t fnc { nullptr };
    uint64_t      x[8] { 0, 0, 0, 0, 0, 0, 0, 0 };
    double        d[8] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    uint64_t      int_return_value { 0 };
    double        double_return_value { 0.0 };
};

int trampoline(Trampoline *);
}

namespace Arwen::Interpreter {
using namespace Util;
using namespace Arwen;

template<typename T>
T as(void *ptr, intptr_t offset)
{
    T ret;
    memcpy(&ret, static_cast<char *>(ptr) + offset, sizeof(T));
    return ret;
}

template<typename T>
void set(void *ptr, T val)
{
    memcpy(static_cast<char *>(ptr), &val, sizeof(T));
}

extern bool native_call(std::string_view name, void *params, std::vector<pType> const &types, void *return_value, pType const &return_type)
{
    if (types.size() > 8) {
        fatal("Can't do native calls with more than 8 parameters");
    }
    Trampoline t;
    if (auto const res = Resolver::get_resolver().resolve(std::string { name }); !res.has_value() || res.value() == nullptr) {
        fatal("Function `{}` not found", name);
    } else {
        t.fnc = res.value();
    }

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

    intptr_t offset { 0 };
    for (size_t ix = 0; ix < types.size(); ++ix) {
        pType const &type = types[ix];

        trace(L"native_call [{}]: {}", ix, type->name);
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
        switch (pType const &et = type; et->kind()) {
        case TypeKind::FloatType: {
            if (nsrn < 8) {
                if (et == TypeRegistry::f32) {
                    t.d[nsrn] = as<float>(params, offset);
                } else {
                    t.d[nsrn] = as<double>(params, offset);
                }
                ++nsrn;
            }
        } break;

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
        case TypeKind::IntType:
            if (ngrn < 8) {
                auto int_type = std::get<IntType>(et->description);
                switch (int_type.width_bits) {
                case 8:
                    t.x[ngrn] = (int_type.is_signed) ? as<int8_t>(params, offset) : as<uint8_t>(params, offset);
                    break;
                case 16:
                    t.x[ngrn] = (int_type.is_signed) ? as<int16_t>(params, offset) : as<uint16_t>(params, offset);
                    break;
                case 32:
                    t.x[ngrn] = (int_type.is_signed) ? as<int32_t>(params, offset) : as<uint32_t>(params, offset);
                    break;
                case 64:
                    t.x[ngrn] = (int_type.is_signed) ? as<int64_t>(params, offset) : as<uint64_t>(params, offset);
                    break;
                }
                // #undef S
                // #define S(W)                            \
//     if (et == TypeRegistry::i##W) {     \
//         t.x[ngrn] = as<int##W##_t>(v);  \
//     }                                   \
//     if (et == TypeRegistry::u##W) {     \
//         t.x[ngrn] = as<uint##W##_t>(v); \
//     }
                //                 BitWidths(S)
                // #undef S
                ++ngrn;
            }
            break;

        case TypeKind::BoolType:
            if (ngrn < 8) {
                t.x[ngrn] = as<bool>(params, offset);
                ++ngrn;
            }
            break;

        case TypeKind::PointerType:
        case TypeKind::ReferenceType:
        case TypeKind::ZeroTerminatedArray:
            if (ngrn < 8) {
                t.x[ngrn] = reinterpret_cast<intptr_t>(as<void *>(params, offset));
                ++ngrn;
            }
            break;

        case TypeKind::SliceType:
            if (ngrn < 7) {
                auto const &[ptr, size] = as<Slice>(params, offset);
                t.x[ngrn++] = reinterpret_cast<intptr_t>(ptr);
                t.x[ngrn++] = size;
            }
            break;

        case TypeKind::DynArray:
            if (ngrn < 6) {
                t.x[ngrn++] = reinterpret_cast<intptr_t>(as<DynamicArray>(params, offset).ptr);
                t.x[ngrn++] = as<DynamicArray>(params, offset).size;
                t.x[ngrn++] = as<DynamicArray>(params, offset).capacity;
            }
            break;

        case TypeKind::Array:
            if (ngrn < 8) {
                t.x[ngrn++] = reinterpret_cast<intptr_t>(as<StaticArray>(params, offset).ptr);
                t.x[ngrn++] = as<StaticArray>(params, offset).size;
            }
            break;

        default:
            NYI("more value types");

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

            // if (type_kind(et) == TK_AGGREGATE) {
            //     size_t size_in_double_words = alignat(typeid_sizeof(et->type_id), 8) / 8;
            //     if (size_in_double_words <= (8 - ngrn)) {
            //         uint64_t buffer[size_in_double_words];
            //         size_t   sz = datum_binary_image(v, buffer);
            //         assert(sz <= size_in_double_words * 8);
            //         for (size_t ix2 = 0; ix2 < size_in_double_words; ++ix2) {
            //             t.x[ngrn] = buffer[ix2];
            //             ++ngrn;
            //         }
            //         continue;
            //     }
            // }

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
        offset += alignat(type->size_of(), 8);
    }

    trace("Trampoline:");
    trace("  Function: {}", reinterpret_cast<intptr_t>(t.fnc));
    trace("  Integer Registers:");
    for (auto ix = 0; ix < 8; ++ix) {
        trace("    {}: 0x{:016x}", ix, t.x[ix]);
    }

    if (auto const trampoline_result = trampoline(&t); trampoline_result) {
        fatal("Error executing `{}`. Trampoline returned {}", name, trampoline_result);
    }
    trace("  Integer result: {}", t.int_return_value);

    switch (return_type->kind()) {
    case TypeKind::IntType: {
#undef S
#define S(W)                                                             \
    if (return_type == TypeRegistry::i##W) {                             \
        set(return_value, static_cast<int##W##_t>(t.int_return_value));  \
        return true;                                                     \
    }                                                                    \
    if (return_type == TypeRegistry::u##W) {                             \
        set(return_value, static_cast<uint##W##_t>(t.int_return_value)); \
        return true;                                                     \
    }
        BitWidths(S)
#undef S
            UNREACHABLE();
    } break;
    case TypeKind::FloatType: {
        if (return_type == TypeRegistry::f32) {
            set(return_value, static_cast<float>(t.double_return_value));
            return true;
        }
        if (return_type == TypeRegistry::f64) {
            set(return_value, t.double_return_value);
            return true;
        }
        UNREACHABLE();
    } break;
    case TypeKind::BoolType:
        set(return_value, static_cast<bool>(t.int_return_value));
        return true;
    case TypeKind::PointerType:
    case TypeKind::ReferenceType:
        set(return_value, reinterpret_cast<void *>(static_cast<intptr_t>(t.int_return_value)));
        return true;
    case TypeKind::VoidType:
        return true;
    default:
        UNREACHABLE();
    }
}

}
