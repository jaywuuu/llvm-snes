; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=i686-unknown-unknown -mattr=+sse2 | FileCheck %s --check-prefix=CHECK --check-prefix=X86
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+sse2 | FileCheck %s --check-prefix=CHECK --check-prefix=X64

define i32 @extractelt_undef_insertelt(i32 %x, i32 %y) {
; CHECK-LABEL: extractelt_undef_insertelt:
; CHECK:       # %bb.0:
; CHECK-NEXT:    ret{{[l|q]}}
  %b = insertelement <4 x i32> zeroinitializer, i32 %x, i64 3
  %c = icmp uge i32 %y, %y
  %d = extractelement <4 x i32> %b, i1 %c
  ret i32 %d
}

define i8 @extractelt_bitcast(i32 %x) nounwind {
; X86-LABEL: extractelt_bitcast:
; X86:       # %bb.0:
; X86-NEXT:    movzbl {{[0-9]+}}(%esp), %eax
; X86-NEXT:    retl
;
; X64-LABEL: extractelt_bitcast:
; X64:       # %bb.0:
; X64-NEXT:    movl %edi, %eax
; X64-NEXT:    # kill: def $al killed $al killed $eax
; X64-NEXT:    retq
  %bc = bitcast i32 %x to <4 x i8>
  %ext = extractelement <4 x i8> %bc, i32 0
  ret i8 %ext
}

define i8 @extractelt_bitcast_extra_use(i32 %x, ptr %p) nounwind {
; X86-LABEL: extractelt_bitcast_extra_use:
; X86:       # %bb.0:
; X86-NEXT:    movl {{[0-9]+}}(%esp), %eax
; X86-NEXT:    movl {{[0-9]+}}(%esp), %ecx
; X86-NEXT:    movl %eax, (%ecx)
; X86-NEXT:    # kill: def $al killed $al killed $eax
; X86-NEXT:    retl
;
; X64-LABEL: extractelt_bitcast_extra_use:
; X64:       # %bb.0:
; X64-NEXT:    movl %edi, %eax
; X64-NEXT:    movl %edi, (%rsi)
; X64-NEXT:    # kill: def $al killed $al killed $eax
; X64-NEXT:    retq
  %bc = bitcast i32 %x to <4 x i8>
  store <4 x i8> %bc, ptr %p
  %ext = extractelement <4 x i8> %bc, i32 0
  ret i8 %ext
}

define i32 @trunc_i64_to_i32_le(i64 %x) {
; X86-LABEL: trunc_i64_to_i32_le:
; X86:       # %bb.0:
; X86-NEXT:    movl {{[0-9]+}}(%esp), %eax
; X86-NEXT:    retl
;
; X64-LABEL: trunc_i64_to_i32_le:
; X64:       # %bb.0:
; X64-NEXT:    movq %rdi, %rax
; X64-NEXT:    # kill: def $eax killed $eax killed $rax
; X64-NEXT:    retq
  %ins = insertelement <2 x i64> undef, i64 %x, i32 0
  %bc = bitcast <2 x i64> %ins to <4 x i32>
  %ext = extractelement <4 x i32> %bc, i32 0
  ret i32 %ext
}

define i16 @trunc_i64_to_i16_le(i64 %x) {
; X86-LABEL: trunc_i64_to_i16_le:
; X86:       # %bb.0:
; X86-NEXT:    movl {{[0-9]+}}(%esp), %eax
; X86-NEXT:    # kill: def $ax killed $ax killed $eax
; X86-NEXT:    retl
;
; X64-LABEL: trunc_i64_to_i16_le:
; X64:       # %bb.0:
; X64-NEXT:    movq %rdi, %rax
; X64-NEXT:    # kill: def $ax killed $ax killed $rax
; X64-NEXT:    retq
  %ins = insertelement <2 x i64> undef, i64 %x, i32 0
  %bc = bitcast <2 x i64> %ins to <8 x i16>
  %ext = extractelement <8 x i16> %bc, i32 0
  ret i16 %ext
}

define i8 @trunc_i32_to_i8_le(i32 %x) {
; X86-LABEL: trunc_i32_to_i8_le:
; X86:       # %bb.0:
; X86-NEXT:    movzbl {{[0-9]+}}(%esp), %eax
; X86-NEXT:    retl
;
; X64-LABEL: trunc_i32_to_i8_le:
; X64:       # %bb.0:
; X64-NEXT:    movl %edi, %eax
; X64-NEXT:    # kill: def $al killed $al killed $eax
; X64-NEXT:    retq
  %ins = insertelement <4 x i32> undef, i32 %x, i32 0
  %bc = bitcast <4 x i32> %ins to <16 x i8>
  %ext = extractelement <16 x i8> %bc, i32 0
  ret i8 %ext
}

