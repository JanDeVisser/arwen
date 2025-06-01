/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

.include "arch/Darwin/arm64/syscalls.inc"

.align 4
.global scribble$write

//
// scribble$write - Call the write(2) syscall
//
// In:
fh      .req x0     // File descriptor
buffer  .req x1     // Pointer to buffer
num     .req x2     // Number of bytes to write

// Out:
//   x0: >0: Number of bytes written. Can be less than num
//       <0: -errno

// Work:
//   ---

scribble$write:
    stp         fp,lr,[sp,#-16]!
    mov         fp,sp
    mov         x16,syscall_write
    svc         #0x00
    mov         sp,fp
    ldp         fp,lr,[sp],#16
    ret
