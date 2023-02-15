; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-linux-gnu | FileCheck %s

define void @add(ptr %pa, ptr %pb, ptr %pc) nounwind {
; CHECK-LABEL: add:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %rbx
; CHECK-NEXT:    movq %rdx, %rbx
; CHECK-NEXT:    movzwl (%rdi), %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm1
; CHECK-NEXT:    movzwl (%rsi), %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss %xmm1, %xmm0
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movw %ax, (%rbx)
; CHECK-NEXT:    popq %rbx
; CHECK-NEXT:    retq
  %a = load bfloat, ptr %pa
  %b = load bfloat, ptr %pb
  %add = fadd bfloat %a, %b
  store bfloat %add, ptr %pc
  ret void
}

define bfloat @add2(bfloat %a, bfloat %b) nounwind {
; CHECK-LABEL: add2:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %rax
; CHECK-NEXT:    movd %xmm1, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm1
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss %xmm1, %xmm0
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    popq %rax
; CHECK-NEXT:    retq
  %add = fadd bfloat %a, %b
  ret bfloat %add
}

define void @add_double(ptr %pa, ptr %pb, ptr %pc) nounwind {
; CHECK-LABEL: add_double:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %r14
; CHECK-NEXT:    pushq %rbx
; CHECK-NEXT:    pushq %rax
; CHECK-NEXT:    movq %rdx, %r14
; CHECK-NEXT:    movq %rsi, %rbx
; CHECK-NEXT:    movq {{.*#+}} xmm0 = mem[0],zero
; CHECK-NEXT:    callq __truncdfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movl %eax, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movq {{.*#+}} xmm0 = mem[0],zero
; CHECK-NEXT:    callq __truncdfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    cvtss2sd %xmm0, %xmm0
; CHECK-NEXT:    movsd %xmm0, (%r14)
; CHECK-NEXT:    addq $8, %rsp
; CHECK-NEXT:    popq %rbx
; CHECK-NEXT:    popq %r14
; CHECK-NEXT:    retq
  %la = load double, ptr %pa
  %a = fptrunc double %la to bfloat
  %lb = load double, ptr %pb
  %b = fptrunc double %lb to bfloat
  %add = fadd bfloat %a, %b
  %dadd = fpext bfloat %add to double
  store double %dadd, ptr %pc
  ret void
}

define double @add_double2(double %da, double %db) nounwind {
; CHECK-LABEL: add_double2:
; CHECK:       # %bb.0:
; CHECK-NEXT:    subq $24, %rsp
; CHECK-NEXT:    movsd %xmm1, {{[-0-9]+}}(%r{{[sb]}}p) # 8-byte Spill
; CHECK-NEXT:    callq __truncdfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movl %eax, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movq {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 8-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero
; CHECK-NEXT:    callq __truncdfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    cvtss2sd %xmm0, %xmm0
; CHECK-NEXT:    addq $24, %rsp
; CHECK-NEXT:    retq
  %a = fptrunc double %da to bfloat
  %b = fptrunc double %db to bfloat
  %add = fadd bfloat %a, %b
  %dadd = fpext bfloat %add to double
  ret double %dadd
}

define void @add_constant(ptr %pa, ptr %pc) nounwind {
; CHECK-LABEL: add_constant:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %rbx
; CHECK-NEXT:    movq %rsi, %rbx
; CHECK-NEXT:    movzwl (%rdi), %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movw %ax, (%rbx)
; CHECK-NEXT:    popq %rbx
; CHECK-NEXT:    retq
  %a = load bfloat, ptr %pa
  %add = fadd bfloat %a, 1.0
  store bfloat %add, ptr %pc
  ret void
}

define bfloat @add_constant2(bfloat %a) nounwind {
; CHECK-LABEL: add_constant2:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %rax
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    shll $16, %eax
; CHECK-NEXT:    movd %eax, %xmm0
; CHECK-NEXT:    addss {{\.?LCPI[0-9]+_[0-9]+}}(%rip), %xmm0
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    popq %rax
; CHECK-NEXT:    retq
  %add = fadd bfloat %a, 1.0
  ret bfloat %add
}

define void @store_constant(ptr %pc) nounwind {
; CHECK-LABEL: store_constant:
; CHECK:       # %bb.0:
; CHECK-NEXT:    movw $16256, (%rdi) # imm = 0x3F80
; CHECK-NEXT:    retq
  store bfloat 1.0, ptr %pc
  ret void
}

define void @fold_ext_trunc(ptr %pa, ptr %pc) nounwind {
; CHECK-LABEL: fold_ext_trunc:
; CHECK:       # %bb.0:
; CHECK-NEXT:    movzwl (%rdi), %eax
; CHECK-NEXT:    movw %ax, (%rsi)
; CHECK-NEXT:    retq
  %a = load bfloat, ptr %pa
  %ext = fpext bfloat %a to float
  %trunc = fptrunc float %ext to bfloat
  store bfloat %trunc, ptr %pc
  ret void
}

define bfloat @fold_ext_trunc2(bfloat %a) nounwind {
; CHECK-LABEL: fold_ext_trunc2:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %ext = fpext bfloat %a to float
  %trunc = fptrunc float %ext to bfloat
  ret bfloat %trunc
}

define <8 x bfloat> @addv(<8 x bfloat> %a, <8 x bfloat> %b) nounwind {
; CHECK-LABEL: addv:
; CHECK:       # %bb.0:
; CHECK-NEXT:    pushq %rbp
; CHECK-NEXT:    pushq %r14
; CHECK-NEXT:    pushq %rbx
; CHECK-NEXT:    subq $32, %rsp
; CHECK-NEXT:    movq %xmm1, %rax
; CHECK-NEXT:    movq %rax, %rcx
; CHECK-NEXT:    shrq $32, %rcx
; CHECK-NEXT:    shll $16, %ecx
; CHECK-NEXT:    movd %ecx, %xmm2
; CHECK-NEXT:    movq %xmm0, %rcx
; CHECK-NEXT:    movq %rcx, %rdx
; CHECK-NEXT:    shrq $32, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm3
; CHECK-NEXT:    addss %xmm2, %xmm3
; CHECK-NEXT:    movss %xmm3, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movq %rax, %rdx
; CHECK-NEXT:    shrq $48, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm2
; CHECK-NEXT:    movq %rcx, %rdx
; CHECK-NEXT:    shrq $48, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm3
; CHECK-NEXT:    addss %xmm2, %xmm3
; CHECK-NEXT:    movss %xmm3, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movl %eax, %edx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm2
; CHECK-NEXT:    movl %ecx, %edx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm3
; CHECK-NEXT:    addss %xmm2, %xmm3
; CHECK-NEXT:    movss %xmm3, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    andl $-65536, %eax # imm = 0xFFFF0000
; CHECK-NEXT:    movd %eax, %xmm2
; CHECK-NEXT:    andl $-65536, %ecx # imm = 0xFFFF0000
; CHECK-NEXT:    movd %ecx, %xmm3
; CHECK-NEXT:    addss %xmm2, %xmm3
; CHECK-NEXT:    movss %xmm3, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    pshufd {{.*#+}} xmm1 = xmm1[2,3,2,3]
; CHECK-NEXT:    movq %xmm1, %rax
; CHECK-NEXT:    movq %rax, %rcx
; CHECK-NEXT:    shrq $32, %rcx
; CHECK-NEXT:    shll $16, %ecx
; CHECK-NEXT:    movd %ecx, %xmm1
; CHECK-NEXT:    pshufd {{.*#+}} xmm0 = xmm0[2,3,2,3]
; CHECK-NEXT:    movq %xmm0, %rcx
; CHECK-NEXT:    movq %rcx, %rdx
; CHECK-NEXT:    shrq $32, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm0
; CHECK-NEXT:    addss %xmm1, %xmm0
; CHECK-NEXT:    movss %xmm0, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movq %rax, %rdx
; CHECK-NEXT:    shrq $48, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm0
; CHECK-NEXT:    movq %rcx, %rdx
; CHECK-NEXT:    shrq $48, %rdx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm1
; CHECK-NEXT:    addss %xmm0, %xmm1
; CHECK-NEXT:    movss %xmm1, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    movl %eax, %edx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm0
; CHECK-NEXT:    movl %ecx, %edx
; CHECK-NEXT:    shll $16, %edx
; CHECK-NEXT:    movd %edx, %xmm1
; CHECK-NEXT:    addss %xmm0, %xmm1
; CHECK-NEXT:    movss %xmm1, {{[-0-9]+}}(%r{{[sb]}}p) # 4-byte Spill
; CHECK-NEXT:    andl $-65536, %eax # imm = 0xFFFF0000
; CHECK-NEXT:    movd %eax, %xmm1
; CHECK-NEXT:    andl $-65536, %ecx # imm = 0xFFFF0000
; CHECK-NEXT:    movd %ecx, %xmm0
; CHECK-NEXT:    addss %xmm1, %xmm0
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %ebx
; CHECK-NEXT:    shll $16, %ebx
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movzwl %ax, %r14d
; CHECK-NEXT:    orl %ebx, %r14d
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %ebp
; CHECK-NEXT:    shll $16, %ebp
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movzwl %ax, %ebx
; CHECK-NEXT:    orl %ebp, %ebx
; CHECK-NEXT:    shlq $32, %rbx
; CHECK-NEXT:    orq %r14, %rbx
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %ebp
; CHECK-NEXT:    shll $16, %ebp
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movzwl %ax, %r14d
; CHECK-NEXT:    orl %ebp, %r14d
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %ebp
; CHECK-NEXT:    shll $16, %ebp
; CHECK-NEXT:    movd {{[-0-9]+}}(%r{{[sb]}}p), %xmm0 # 4-byte Folded Reload
; CHECK-NEXT:    # xmm0 = mem[0],zero,zero,zero
; CHECK-NEXT:    callq __truncsfbf2@PLT
; CHECK-NEXT:    movd %xmm0, %eax
; CHECK-NEXT:    movzwl %ax, %eax
; CHECK-NEXT:    orl %ebp, %eax
; CHECK-NEXT:    shlq $32, %rax
; CHECK-NEXT:    orq %r14, %rax
; CHECK-NEXT:    movq %rax, %xmm0
; CHECK-NEXT:    movq %rbx, %xmm1
; CHECK-NEXT:    punpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; CHECK-NEXT:    addq $32, %rsp
; CHECK-NEXT:    popq %rbx
; CHECK-NEXT:    popq %r14
; CHECK-NEXT:    popq %rbp
; CHECK-NEXT:    retq
  %add = fadd <8 x bfloat> %a, %b
  ret <8 x bfloat> %add
}
