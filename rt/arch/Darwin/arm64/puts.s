.align 4
.global _arwen$puts
.global arwen$puts

//
// puts - Print string
//
// In:
//   x0: Pointer to string buffer
//   w1: String length

// Out:
//   x0: Number of characters printed.

// Work:
//   x16: Syscall

_arwen$puts:
arwen$puts:
    stp     fp,lr,[sp,#-16]!
    mov     fp,sp

    cmp     x0,#0
    b.eq    __puts_print_null
    mov     x2,x1
    mov     x1,x0

__puts_print:
    mov     x0,#1
    mov     x16,#0x04
    svc     #0x00
    ldp     fp,lr,[sp],#16
    ret

    // Print '[[null]]' if the buffer is the null pointer:
__puts_print_null:
    adr     x1,__str_nullptr
    mov     x2,str_nullptr_len
    b       __puts_print

__str_nullptr:
    .string "[[null]]"

.equ str_nullptr_len,8
