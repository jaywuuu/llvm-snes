; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=aarch64-unknown-linux-gnu < %s | FileCheck %s

define i32 @fold_urem_positive_odd(i32 %x) {
; CHECK-LABEL: fold_urem_positive_odd:
; CHECK:       // %bb.0:
; CHECK-NEXT:    mov w8, #8969
; CHECK-NEXT:    movk w8, #22765, lsl #16
; CHECK-NEXT:    umull x8, w0, w8
; CHECK-NEXT:    lsr x8, x8, #32
; CHECK-NEXT:    sub w9, w0, w8
; CHECK-NEXT:    add w8, w8, w9, lsr #1
; CHECK-NEXT:    mov w9, #95
; CHECK-NEXT:    lsr w8, w8, #6
; CHECK-NEXT:    msub w0, w8, w9, w0
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 95
  ret i32 %1
}


define i32 @fold_urem_positive_even(i32 %x) {
; CHECK-LABEL: fold_urem_positive_even:
; CHECK:       // %bb.0:
; CHECK-NEXT:    mov w8, #16323
; CHECK-NEXT:    mov w9, #1060
; CHECK-NEXT:    movk w8, #63310, lsl #16
; CHECK-NEXT:    umull x8, w0, w8
; CHECK-NEXT:    lsr x8, x8, #42
; CHECK-NEXT:    msub w0, w8, w9, w0
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 1060
  ret i32 %1
}


; Don't fold if we can combine urem with udiv.
define i32 @combine_urem_udiv(i32 %x) {
; CHECK-LABEL: combine_urem_udiv:
; CHECK:       // %bb.0:
; CHECK-NEXT:    mov w8, #8969
; CHECK-NEXT:    movk w8, #22765, lsl #16
; CHECK-NEXT:    umull x8, w0, w8
; CHECK-NEXT:    lsr x8, x8, #32
; CHECK-NEXT:    sub w9, w0, w8
; CHECK-NEXT:    add w8, w8, w9, lsr #1
; CHECK-NEXT:    mov w9, #95
; CHECK-NEXT:    lsr w8, w8, #6
; CHECK-NEXT:    msub w9, w8, w9, w0
; CHECK-NEXT:    add w0, w9, w8
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 95
  %2 = udiv i32 %x, 95
  %3 = add i32 %1, %2
  ret i32 %3
}

; Don't fold for divisors that are a power of two.
define i32 @dont_fold_urem_power_of_two(i32 %x) {
; CHECK-LABEL: dont_fold_urem_power_of_two:
; CHECK:       // %bb.0:
; CHECK-NEXT:    and w0, w0, #0x3f
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 64
  ret i32 %1
}

; Don't fold if the divisor is one.
define i32 @dont_fold_urem_one(i32 %x) {
; CHECK-LABEL: dont_fold_urem_one:
; CHECK:       // %bb.0:
; CHECK-NEXT:    mov w0, wzr
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 1
  ret i32 %1
}

; Don't fold if the divisor is 2^32.
define i32 @dont_fold_urem_i32_umax(i32 %x) {
; CHECK-LABEL: dont_fold_urem_i32_umax:
; CHECK:       // %bb.0:
; CHECK-NEXT:    ret
  %1 = urem i32 %x, 4294967296
  ret i32 %1
}

; Don't fold i64 urem
define i64 @dont_fold_urem_i64(i64 %x) {
; CHECK-LABEL: dont_fold_urem_i64:
; CHECK:       // %bb.0:
; CHECK-NEXT:    mov x8, #58849
; CHECK-NEXT:    lsr x9, x0, #1
; CHECK-NEXT:    movk x8, #48148, lsl #16
; CHECK-NEXT:    movk x8, #33436, lsl #32
; CHECK-NEXT:    movk x8, #21399, lsl #48
; CHECK-NEXT:    umulh x8, x9, x8
; CHECK-NEXT:    mov w9, #98
; CHECK-NEXT:    lsr x8, x8, #4
; CHECK-NEXT:    msub x0, x8, x9, x0
; CHECK-NEXT:    ret
  %1 = urem i64 %x, 98
  ret i64 %1
}