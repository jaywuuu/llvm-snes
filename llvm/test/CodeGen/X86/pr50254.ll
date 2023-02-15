; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=i686-unknown | FileCheck %s --check-prefix=X86
; RUN: llc < %s -mtriple=x86_64-unknown | FileCheck %s --check-prefix=X64

@d.e = external dso_local unnamed_addr global i32, align 4

define void @PR50254() {
; X86-LABEL: PR50254:
; X86:       # %bb.0: # %entry
; X86-NEXT:    movswl d.e, %eax
; X86-NEXT:    xorl %ecx, %ecx
; X86-NEXT:    testb %cl, %cl
; X86-NEXT:    jne .LBB0_2
; X86-NEXT:  # %bb.1: # %for.end
; X86-NEXT:    movw %ax, d.e
; X86-NEXT:  .LBB0_2: # %for.body.1
; X86-NEXT:    retl
;
; X64-LABEL: PR50254:
; X64:       # %bb.0: # %entry
; X64-NEXT:    movswq d.e(%rip), %rax
; X64-NEXT:    xorl %ecx, %ecx
; X64-NEXT:    testb %cl, %cl
; X64-NEXT:    jne .LBB0_2
; X64-NEXT:  # %bb.1: # %for.end
; X64-NEXT:    movw %ax, d.e(%rip)
; X64-NEXT:  .LBB0_2: # %for.body.1
; X64-NEXT:    retq
entry:
  %load = load i16, ptr @d.e, align 4
  %xor1 = xor i16 %load, 0
  %xor2 = xor i64 undef, 3821908120
  %xor3 = xor i16 %load, -1
  %xor4 = sext i16 %xor3 to i64
  %xor5 = and i64 %xor4, 4294967295
  %xor6 = xor i64 %xor5, 3821908120
  br label %for.body

for.body:                                         ; preds = %entry
  br i1 undef, label %for.end, label %for.body.1

for.end:                                          ; preds = %for.body
  store i16 %xor1, ptr @d.e, align 4
  ret void

for.body.1:                                       ; preds = %for.body
  %add.1 = add i64 %xor6, undef
  ret void
}
