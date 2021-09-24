; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -O0 -mtriple=x86_64-unknown-linux-gnux32 | FileCheck %s

target datalayout = "e-m:e-p:32:32-i64:64-f80:128-n8:16:32:64-S128"

%struct.a = type { i8 }

define void @_Z1bv(%struct.a* noalias sret(%struct.a) %agg.result) {
; CHECK-LABEL: _Z1bv:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    pushq %rax
; CHECK-NEXT:    .cfi_def_cfa_offset 16
; CHECK-NEXT:    # kill: def $edi killed $edi killed $rdi
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    movl %eax, {{[-0-9]+}}(%e{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    callq _Z1bv
; CHECK-NEXT:    movl {{[-0-9]+}}(%e{{[sb]}}p), %eax # 4-byte Reload
; CHECK-NEXT:    popq %rcx
; CHECK-NEXT:    .cfi_def_cfa_offset 8
; CHECK-NEXT:    retq
entry:
  call void @_Z1bv(%struct.a* sret(%struct.a) %agg.result)
  ret void
}
