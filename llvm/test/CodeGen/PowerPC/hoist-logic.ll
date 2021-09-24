; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -verify-machineinstrs -mtriple=powerpc64-unknown-linux-gnu | FileCheck %s
; RUN: llc < %s -verify-machineinstrs -mtriple=powerpc64-ibm-aix-xcoff | FileCheck %s

; This is good - eliminate an op by hoisting logic.

define i32 @lshr_or(i32 %x, i32 %y, i32 %z, i32* %p1, i32* %p2) {
; CHECK-LABEL: lshr_or:
; CHECK:       # %bb.0:
; CHECK-NEXT:    or 3, 3, 4
; CHECK-NEXT:    srw 3, 3, 5
; CHECK-NEXT:    blr
  %xt = lshr i32 %x, %z
  %yt = lshr i32 %y, %z
  %r = or i32 %xt, %yt
  ret i32 %r
}

; This is questionable - hoisting doesn't eliminate anything.
; It might result in an extra register move.

define i32 @lshr_or_multiuse1(i32 %x, i32 %y, i32 %z, i32* %p1, i32* %p2) {
; CHECK-LABEL: lshr_or_multiuse1:
; CHECK:       # %bb.0:
; CHECK-NEXT:    srw 7, 3, 5
; CHECK-NEXT:    srw 3, 4, 5
; CHECK-NEXT:    or 3, 7, 3
; CHECK-NEXT:    stw 7, 0(6)
; CHECK-NEXT:    blr
  %xt = lshr i32 %x, %z
  %yt = lshr i32 %y, %z
  store i32 %xt, i32* %p1
  %r = or i32 %xt, %yt
  ret i32 %r
}

; This is questionable - hoisting doesn't eliminate anything.

define i32 @lshr_multiuse2(i32 %x, i32 %y, i32 %z, i32* %p1, i32* %p2) {
; CHECK-LABEL: lshr_multiuse2:
; CHECK:       # %bb.0:
; CHECK-NEXT:    srw 3, 3, 5
; CHECK-NEXT:    srw 4, 4, 5
; CHECK-NEXT:    or 3, 3, 4
; CHECK-NEXT:    stw 4, 0(7)
; CHECK-NEXT:    blr
  %xt = lshr i32 %x, %z
  %yt = lshr i32 %y, %z
  store i32 %yt, i32* %p2
  %r = or i32 %xt, %yt
  ret i32 %r
}

; This is not profitable to hoist. We need an extra shift instruction.

define i32 @lshr_multiuse3(i32 %x, i32 %y, i32 %z, i32* %p1, i32* %p2) {
; CHECK-LABEL: lshr_multiuse3:
; CHECK:       # %bb.0:
; CHECK-NEXT:    srw 3, 3, 5
; CHECK-NEXT:    srw 4, 4, 5
; CHECK-NEXT:    stw 3, 0(6)
; CHECK-NEXT:    or 3, 3, 4
; CHECK-NEXT:    stw 4, 0(7)
; CHECK-NEXT:    blr
  %xt = lshr i32 %x, %z
  %yt = lshr i32 %y, %z
  store i32 %xt, i32* %p1
  store i32 %yt, i32* %p2
  %r = or i32 %xt, %yt
  ret i32 %r
}

