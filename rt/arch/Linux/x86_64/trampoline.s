/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.global _trampoline
.global trampoline

.align 4

// param_regs = rdi, rsi, rdx, rcx, r8, r9
// scratch_regs = r10, r11, r12, r13, r14, r15
// regs_to_be_saved = rbx, r12, r13, r14, r15
// alu_regs = rax, rbx, rcx, rdx

_trampoline:
trampoline:
    // Set up stack
    pushq    %rbp
    movq    %rsp, %rbp

    // Get *X86_64Trampoline
    movq     %rdi, %r10

    // Load param registers
    movq    8(%r10), %rdi
    movq    16(%r10), %rsi
    movq    24(%r10), %rdx
    movq    32(%r10), %rcx
    movq    40(%r10), %r8
    movq    48(%r10), %r9

    // Load FP registers
    //movq      56(%r10), xmm0
    //movq      64(%r10), xmm1
    //movq      72(%r10), xmm2
    //movq      80(%r10), xmm3
    //movq      88(%r10), xmm4
    // movq      96(%r10), xmm5
    //movq      104(%r10), xmm6
    //movq      112(%r10), xmm7

    // Save r10 which holds the trampoline. r10 is caller saved
    pushq   %r10
    //movq    %r11, (%r10)          // Load function pointer in r11

    // Call function pointer
    call    *0(%r10)

    // Restore r10
    popq    %r10

    // Store return value
    movq    %rax, 120(%r10)
    //str     %xmm0, 128(%r10)

    // Return all good
    xorq    %rax,%rax

    // Restore SP and BP
    movq    %rbp,%rsp
    popq    %rbp
    ret
