; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=riscv32 -mattr=+d -target-abi ilp32d -verify-machineinstrs \
; RUN:   < %s | FileCheck %s
; RUN: llc -mtriple=riscv64 -mattr=+d -target-abi lp64d -verify-machineinstrs \
; RUN:   < %s | FileCheck %s

define zeroext i1 @double_is_nan(double %a) nounwind {
; CHECK-LABEL: double_is_nan:
; CHECK:       # %bb.0:
; CHECK-NEXT:    feq.d a0, fa0, fa0
; CHECK-NEXT:    xori a0, a0, 1
; CHECK-NEXT:    ret
  %1 = fcmp uno double %a, 0.000000e+00
  ret i1 %1
}

define zeroext i1 @double_not_nan(double %a) nounwind {
; CHECK-LABEL: double_not_nan:
; CHECK:       # %bb.0:
; CHECK-NEXT:    feq.d a0, fa0, fa0
; CHECK-NEXT:    ret
  %1 = fcmp ord double %a, 0.000000e+00
  ret i1 %1
}
