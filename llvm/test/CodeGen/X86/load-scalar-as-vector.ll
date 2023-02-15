; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-- -mattr=+sse2       | FileCheck %s --check-prefix=SSE
; RUN: llc < %s -mtriple=x86_64-- -mattr=+sse4.2     | FileCheck %s --check-prefix=SSE
; RUN: llc < %s -mtriple=x86_64-- -mattr=+avx        | FileCheck %s --check-prefix=AVX
; RUN: llc < %s -mtriple=x86_64-- -mattr=+avx2       | FileCheck %s --check-prefix=AVX
; RUN: llc < %s -mtriple=x86_64-- -mattr=+avx512dq,+avx512bw | FileCheck %s --check-prefix=AVX

define <4 x i32> @add_op1_constant(ptr %p) nounwind {
; SSE-LABEL: add_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    addl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: add_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    addl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = add i32 %x, 42
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

; Code and data size may increase by using more vector ops, so the transform is disabled here.

define <4 x i32> @add_op1_constant_optsize(ptr %p) nounwind optsize {
; SSE-LABEL: add_op1_constant_optsize:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    addl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: add_op1_constant_optsize:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    addl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = add i32 %x, 42
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <8 x i16> @add_op0_constant(ptr %p) nounwind {
; SSE-LABEL: add_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzwl (%rdi), %eax
; SSE-NEXT:    addl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: add_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzwl (%rdi), %eax
; AVX-NEXT:    addl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = add i16 42, %x
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <2 x i64> @sub_op0_constant(ptr %p) nounwind {
; SSE-LABEL: sub_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    subq (%rdi), %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sub_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    subq (%rdi), %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = sub i64 42, %x
  %r = insertelement <2 x i64> undef, i64 %b, i32 0
  ret <2 x i64> %r
}

define <16 x i8> @sub_op1_constant(ptr %p) nounwind {
; SSE-LABEL: sub_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %eax
; SSE-NEXT:    addb $-42, %al
; SSE-NEXT:    movzbl %al, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sub_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %eax
; AVX-NEXT:    addb $-42, %al
; AVX-NEXT:    movzbl %al, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i8, ptr %p
  %b = sub i8 %x, 42
  %r = insertelement <16 x i8> undef, i8 %b, i32 0
  ret <16 x i8> %r
}

define <4 x i32> @mul_op1_constant(ptr %p) nounwind {
; SSE-LABEL: mul_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    imull $42, (%rdi), %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: mul_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    imull $42, (%rdi), %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = mul i32 %x, 42
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <8 x i16> @mul_op0_constant(ptr %p) nounwind {
; SSE-LABEL: mul_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzwl (%rdi), %eax
; SSE-NEXT:    imull $42, %eax, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: mul_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzwl (%rdi), %eax
; AVX-NEXT:    imull $42, %eax, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = mul i16 42, %x
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <4 x i32> @and_op1_constant(ptr %p) nounwind {
; SSE-LABEL: and_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    andl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: and_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    andl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = and i32 %x, 42
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <2 x i64> @or_op1_constant(ptr %p) nounwind {
; SSE-LABEL: or_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movq (%rdi), %rax
; SSE-NEXT:    orq $42, %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: or_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movq (%rdi), %rax
; AVX-NEXT:    orq $42, %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = or i64 %x, 42
  %r = insertelement <2 x i64> undef, i64 %b, i32 0
  ret <2 x i64> %r
}

define <8 x i16> @xor_op1_constant(ptr %p) nounwind {
; SSE-LABEL: xor_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzwl (%rdi), %eax
; SSE-NEXT:    xorl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: xor_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzwl (%rdi), %eax
; AVX-NEXT:    xorl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = xor i16 %x, 42
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <4 x i32> @shl_op0_constant(ptr %p) nounwind {
; SSE-LABEL: shl_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %ecx
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    shll %cl, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: shl_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %ecx
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    shll %cl, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = shl i32 42, %x
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <16 x i8> @shl_op1_constant(ptr %p) nounwind {
; SSE-LABEL: shl_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %eax
; SSE-NEXT:    shlb $5, %al
; SSE-NEXT:    movzbl %al, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: shl_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %eax
; AVX-NEXT:    shlb $5, %al
; AVX-NEXT:    movzbl %al, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i8, ptr %p
  %b = shl i8 %x, 5
  %r = insertelement <16 x i8> undef, i8 %b, i32 0
  ret <16 x i8> %r
}

define <2 x i64> @lshr_op0_constant(ptr %p) nounwind {
; SSE-LABEL: lshr_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %ecx
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    shrq %cl, %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: lshr_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %ecx
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    shrq %cl, %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = lshr i64 42, %x
  %r = insertelement <2 x i64> undef, i64 %b, i32 0
  ret <2 x i64> %r
}

define <4 x i32> @lshr_op1_constant(ptr %p) nounwind {
; SSE-LABEL: lshr_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    shrl $17, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: lshr_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    shrl $17, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = lshr i32 %x, 17
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <8 x i16> @ashr_op0_constant(ptr %p) nounwind {
; SSE-LABEL: ashr_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %ecx
; SSE-NEXT:    movl $-42, %eax
; SSE-NEXT:    sarl %cl, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: ashr_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %ecx
; AVX-NEXT:    movl $-42, %eax
; AVX-NEXT:    sarl %cl, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = ashr i16 -42, %x
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <8 x i16> @ashr_op1_constant(ptr %p) nounwind {
; SSE-LABEL: ashr_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movswl (%rdi), %eax
; SSE-NEXT:    sarl $7, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: ashr_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movswl (%rdi), %eax
; AVX-NEXT:    sarl $7, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = ashr i16 %x, 7
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <4 x i32> @sdiv_op0_constant(ptr %p) nounwind {
; SSE-LABEL: sdiv_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    xorl %edx, %edx
; SSE-NEXT:    idivl (%rdi)
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sdiv_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    xorl %edx, %edx
; AVX-NEXT:    idivl (%rdi)
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = sdiv i32 42, %x
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <8 x i16> @sdiv_op1_constant(ptr %p) nounwind {
; SSE-LABEL: sdiv_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movswl (%rdi), %eax
; SSE-NEXT:    imull $-15603, %eax, %ecx # imm = 0xC30D
; SSE-NEXT:    shrl $16, %ecx
; SSE-NEXT:    addl %eax, %ecx
; SSE-NEXT:    movzwl %cx, %eax
; SSE-NEXT:    movswl %ax, %ecx
; SSE-NEXT:    shrl $15, %eax
; SSE-NEXT:    sarl $5, %ecx
; SSE-NEXT:    addl %eax, %ecx
; SSE-NEXT:    movd %ecx, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sdiv_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movswl (%rdi), %eax
; AVX-NEXT:    imull $-15603, %eax, %ecx # imm = 0xC30D
; AVX-NEXT:    shrl $16, %ecx
; AVX-NEXT:    addl %eax, %ecx
; AVX-NEXT:    movzwl %cx, %eax
; AVX-NEXT:    movswl %ax, %ecx
; AVX-NEXT:    shrl $15, %eax
; AVX-NEXT:    sarl $5, %ecx
; AVX-NEXT:    addl %eax, %ecx
; AVX-NEXT:    vmovd %ecx, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = sdiv i16 %x, 42
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <8 x i16> @srem_op0_constant(ptr %p) nounwind {
; SSE-LABEL: srem_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movw $42, %ax
; SSE-NEXT:    xorl %edx, %edx
; SSE-NEXT:    idivw (%rdi)
; SSE-NEXT:    # kill: def $dx killed $dx def $edx
; SSE-NEXT:    movd %edx, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: srem_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movw $42, %ax
; AVX-NEXT:    xorl %edx, %edx
; AVX-NEXT:    idivw (%rdi)
; AVX-NEXT:    # kill: def $dx killed $dx def $edx
; AVX-NEXT:    vmovd %edx, %xmm0
; AVX-NEXT:    retq
  %x = load i16, ptr %p
  %b = srem i16 42, %x
  %r = insertelement <8 x i16> undef, i16 %b, i32 0
  ret <8 x i16> %r
}

define <4 x i32> @srem_op1_constant(ptr %p) nounwind {
; SSE-LABEL: srem_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movslq (%rdi), %rax
; SSE-NEXT:    imulq $818089009, %rax, %rcx # imm = 0x30C30C31
; SSE-NEXT:    movq %rcx, %rdx
; SSE-NEXT:    shrq $63, %rdx
; SSE-NEXT:    sarq $35, %rcx
; SSE-NEXT:    addl %edx, %ecx
; SSE-NEXT:    imull $42, %ecx, %ecx
; SSE-NEXT:    subl %ecx, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: srem_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movslq (%rdi), %rax
; AVX-NEXT:    imulq $818089009, %rax, %rcx # imm = 0x30C30C31
; AVX-NEXT:    movq %rcx, %rdx
; AVX-NEXT:    shrq $63, %rdx
; AVX-NEXT:    sarq $35, %rcx
; AVX-NEXT:    addl %edx, %ecx
; AVX-NEXT:    imull $42, %ecx, %ecx
; AVX-NEXT:    subl %ecx, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = srem i32 %x, 42
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <4 x i32> @udiv_op0_constant(ptr %p) nounwind {
; SSE-LABEL: udiv_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    xorl %edx, %edx
; SSE-NEXT:    divl (%rdi)
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: udiv_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    xorl %edx, %edx
; AVX-NEXT:    divl (%rdi)
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = udiv i32 42, %x
  %r = insertelement <4 x i32> undef, i32 %b, i32 0
  ret <4 x i32> %r
}

define <2 x i64> @udiv_op1_constant(ptr %p) nounwind {
; SSE-LABEL: udiv_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movq (%rdi), %rax
; SSE-NEXT:    shrq %rax
; SSE-NEXT:    movabsq $-4392081922311798003, %rcx # imm = 0xC30C30C30C30C30D
; SSE-NEXT:    mulq %rcx
; SSE-NEXT:    shrq $4, %rdx
; SSE-NEXT:    movq %rdx, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: udiv_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movq (%rdi), %rax
; AVX-NEXT:    shrq %rax
; AVX-NEXT:    movabsq $-4392081922311798003, %rcx # imm = 0xC30C30C30C30C30D
; AVX-NEXT:    mulq %rcx
; AVX-NEXT:    shrq $4, %rdx
; AVX-NEXT:    vmovq %rdx, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = udiv i64 %x, 42
  %r = insertelement <2 x i64> undef, i64 %b, i32 0
  ret <2 x i64> %r
}

define <2 x i64> @urem_op0_constant(ptr %p) nounwind {
; SSE-LABEL: urem_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    xorl %edx, %edx
; SSE-NEXT:    divq (%rdi)
; SSE-NEXT:    movq %rdx, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: urem_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    xorl %edx, %edx
; AVX-NEXT:    divq (%rdi)
; AVX-NEXT:    vmovq %rdx, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = urem i64 42, %x
  %r = insertelement <2 x i64> undef, i64 %b, i32 0
  ret <2 x i64> %r
}

define <16 x i8> @urem_op1_constant(ptr %p) nounwind {
; SSE-LABEL: urem_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movzbl (%rdi), %eax
; SSE-NEXT:    movl %eax, %ecx
; SSE-NEXT:    shrb %cl
; SSE-NEXT:    movzbl %cl, %ecx
; SSE-NEXT:    imull $49, %ecx, %ecx
; SSE-NEXT:    shrl $10, %ecx
; SSE-NEXT:    imull $42, %ecx, %ecx
; SSE-NEXT:    subb %cl, %al
; SSE-NEXT:    movzbl %al, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: urem_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    movzbl (%rdi), %eax
; AVX-NEXT:    movl %eax, %ecx
; AVX-NEXT:    shrb %cl
; AVX-NEXT:    movzbl %cl, %ecx
; AVX-NEXT:    imull $49, %ecx, %ecx
; AVX-NEXT:    shrl $10, %ecx
; AVX-NEXT:    imull $42, %ecx, %ecx
; AVX-NEXT:    subb %cl, %al
; AVX-NEXT:    movzbl %al, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i8, ptr %p
  %b = urem i8 %x, 42
  %r = insertelement <16 x i8> undef, i8 %b, i32 0
  ret <16 x i8> %r
}

define <4 x float> @fadd_op1_constant(ptr %p) nounwind {
; SSE-LABEL: fadd_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; SSE-NEXT:    addss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fadd_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; AVX-NEXT:    vaddss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load float, ptr %p
  %b = fadd float %x, 42.0
  %r = insertelement <4 x float> undef, float %b, i32 0
  ret <4 x float> %r
}

define <2 x double> @fsub_op1_constant(ptr %p) nounwind {
; SSE-LABEL: fsub_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movsd {{.*#+}} xmm0 = mem[0],zero
; SSE-NEXT:    addsd {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fsub_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovsd {{.*#+}} xmm0 = mem[0],zero
; AVX-NEXT:    vaddsd {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load double, ptr %p
  %b = fsub double %x, 42.0
  %r = insertelement <2 x double> undef, double %b, i32 0
  ret <2 x double> %r
}

define <4 x float> @fsub_op0_constant(ptr %p) nounwind {
; SSE-LABEL: fsub_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; SSE-NEXT:    subss (%rdi), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fsub_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; AVX-NEXT:    vsubss (%rdi), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load float, ptr %p
  %b = fsub float 42.0, %x
  %r = insertelement <4 x float> undef, float %b, i32 0
  ret <4 x float> %r
}

define <4 x float> @fmul_op1_constant(ptr %p) nounwind {
; SSE-LABEL: fmul_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; SSE-NEXT:    mulss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fmul_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; AVX-NEXT:    vmulss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load float, ptr %p
  %b = fmul float %x, 42.0
  %r = insertelement <4 x float> undef, float %b, i32 0
  ret <4 x float> %r
}

define <2 x double> @fdiv_op1_constant(ptr %p) nounwind {
; SSE-LABEL: fdiv_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movsd {{.*#+}} xmm0 = mem[0],zero
; SSE-NEXT:    divsd {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fdiv_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovsd {{.*#+}} xmm0 = mem[0],zero
; AVX-NEXT:    vdivsd {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load double, ptr %p
  %b = fdiv double %x, 42.0
  %r = insertelement <2 x double> undef, double %b, i32 0
  ret <2 x double> %r
}

define <4 x float> @fdiv_op0_constant(ptr %p) nounwind {
; SSE-LABEL: fdiv_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    movss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; SSE-NEXT:    divss (%rdi), %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: fdiv_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    vmovss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; AVX-NEXT:    vdivss (%rdi), %xmm0, %xmm0
; AVX-NEXT:    retq
  %x = load float, ptr %p
  %b = fdiv float 42.0, %x
  %r = insertelement <4 x float> undef, float %b, i32 0
  ret <4 x float> %r
}

define <4 x float> @frem_op1_constant(ptr %p) nounwind {
; SSE-LABEL: frem_op1_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    pushq %rax
; SSE-NEXT:    movss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; SSE-NEXT:    movss {{.*#+}} xmm1 = mem[0],zero,zero,zero
; SSE-NEXT:    callq fmodf@PLT
; SSE-NEXT:    popq %rax
; SSE-NEXT:    retq
;
; AVX-LABEL: frem_op1_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    pushq %rax
; AVX-NEXT:    vmovss {{.*#+}} xmm0 = mem[0],zero,zero,zero
; AVX-NEXT:    vmovss {{.*#+}} xmm1 = mem[0],zero,zero,zero
; AVX-NEXT:    callq fmodf@PLT
; AVX-NEXT:    popq %rax
; AVX-NEXT:    retq
  %x = load float, ptr %p
  %b = frem float %x, 42.0
  %r = insertelement <4 x float> undef, float %b, i32 0
  ret <4 x float> %r
}

define <2 x double> @frem_op0_constant(ptr %p) nounwind {
; SSE-LABEL: frem_op0_constant:
; SSE:       # %bb.0:
; SSE-NEXT:    pushq %rax
; SSE-NEXT:    movsd {{.*#+}} xmm1 = mem[0],zero
; SSE-NEXT:    movsd {{.*#+}} xmm0 = mem[0],zero
; SSE-NEXT:    callq fmod@PLT
; SSE-NEXT:    popq %rax
; SSE-NEXT:    retq
;
; AVX-LABEL: frem_op0_constant:
; AVX:       # %bb.0:
; AVX-NEXT:    pushq %rax
; AVX-NEXT:    vmovsd {{.*#+}} xmm1 = mem[0],zero
; AVX-NEXT:    vmovsd {{.*#+}} xmm0 = mem[0],zero
; AVX-NEXT:    callq fmod@PLT
; AVX-NEXT:    popq %rax
; AVX-NEXT:    retq
  %x = load double, ptr %p
  %b = frem double 42.0, %x
  %r = insertelement <2 x double> undef, double %b, i32 0
  ret <2 x double> %r
}

; Try again with 256-bit types.

define <8 x i32> @add_op1_constant_v8i32(ptr %p) nounwind {
; SSE-LABEL: add_op1_constant_v8i32:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    addl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: add_op1_constant_v8i32:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    addl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = add i32 %x, 42
  %r = insertelement <8 x i32> undef, i32 %b, i32 0
  ret <8 x i32> %r
}

define <4 x i64> @sub_op0_constant_v4i64(ptr %p) nounwind {
; SSE-LABEL: sub_op0_constant_v4i64:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    subq (%rdi), %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sub_op0_constant_v4i64:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    subq (%rdi), %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = sub i64 42, %x
  %r = insertelement <4 x i64> undef, i64 %b, i32 0
  ret <4 x i64> %r
}

define <8 x i32> @mul_op1_constant_v8i32(ptr %p) nounwind {
; SSE-LABEL: mul_op1_constant_v8i32:
; SSE:       # %bb.0:
; SSE-NEXT:    imull $42, (%rdi), %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: mul_op1_constant_v8i32:
; AVX:       # %bb.0:
; AVX-NEXT:    imull $42, (%rdi), %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = mul i32 %x, 42
  %r = insertelement <8 x i32> undef, i32 %b, i32 0
  ret <8 x i32> %r
}

define <4 x i64> @or_op1_constant_v4i64(ptr %p) nounwind {
; SSE-LABEL: or_op1_constant_v4i64:
; SSE:       # %bb.0:
; SSE-NEXT:    movq (%rdi), %rax
; SSE-NEXT:    orq $42, %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: or_op1_constant_v4i64:
; AVX:       # %bb.0:
; AVX-NEXT:    movq (%rdi), %rax
; AVX-NEXT:    orq $42, %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = or i64 %x, 42
  %r = insertelement <4 x i64> undef, i64 %b, i32 0
  ret <4 x i64> %r
}

; Try again with 512-bit types.

define <16 x i32> @add_op1_constant_v16i32(ptr %p) nounwind {
; SSE-LABEL: add_op1_constant_v16i32:
; SSE:       # %bb.0:
; SSE-NEXT:    movl (%rdi), %eax
; SSE-NEXT:    addl $42, %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: add_op1_constant_v16i32:
; AVX:       # %bb.0:
; AVX-NEXT:    movl (%rdi), %eax
; AVX-NEXT:    addl $42, %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = add i32 %x, 42
  %r = insertelement <16 x i32> undef, i32 %b, i32 0
  ret <16 x i32> %r
}

define <8 x i64> @sub_op0_constant_v8i64(ptr %p) nounwind {
; SSE-LABEL: sub_op0_constant_v8i64:
; SSE:       # %bb.0:
; SSE-NEXT:    movl $42, %eax
; SSE-NEXT:    subq (%rdi), %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: sub_op0_constant_v8i64:
; AVX:       # %bb.0:
; AVX-NEXT:    movl $42, %eax
; AVX-NEXT:    subq (%rdi), %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = sub i64 42, %x
  %r = insertelement <8 x i64> undef, i64 %b, i32 0
  ret <8 x i64> %r
}

define <16 x i32> @mul_op1_constant_v16i32(ptr %p) nounwind {
; SSE-LABEL: mul_op1_constant_v16i32:
; SSE:       # %bb.0:
; SSE-NEXT:    imull $42, (%rdi), %eax
; SSE-NEXT:    movd %eax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: mul_op1_constant_v16i32:
; AVX:       # %bb.0:
; AVX-NEXT:    imull $42, (%rdi), %eax
; AVX-NEXT:    vmovd %eax, %xmm0
; AVX-NEXT:    retq
  %x = load i32, ptr %p
  %b = mul i32 %x, 42
  %r = insertelement <16 x i32> undef, i32 %b, i32 0
  ret <16 x i32> %r
}

define <8 x i64> @or_op1_constant_v8i64(ptr %p) nounwind {
; SSE-LABEL: or_op1_constant_v8i64:
; SSE:       # %bb.0:
; SSE-NEXT:    movq (%rdi), %rax
; SSE-NEXT:    orq $42, %rax
; SSE-NEXT:    movq %rax, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: or_op1_constant_v8i64:
; AVX:       # %bb.0:
; AVX-NEXT:    movq (%rdi), %rax
; AVX-NEXT:    orq $42, %rax
; AVX-NEXT:    vmovq %rax, %xmm0
; AVX-NEXT:    retq
  %x = load i64, ptr %p
  %b = or i64 %x, 42
  %r = insertelement <8 x i64> undef, i64 %b, i32 0
  ret <8 x i64> %r
}

