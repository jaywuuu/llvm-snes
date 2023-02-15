; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-apple-darwin -mcpu=skx | FileCheck %s

define i32 @mask32(i32 %x) {
; CHECK-LABEL: mask32:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    notl %eax
; CHECK-NEXT:    retq
  %m0 = bitcast i32 %x to <32 x i1>
  %m1 = xor <32 x i1> %m0, <i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1>
  %ret = bitcast <32 x i1> %m1 to i32
  ret i32 %ret
}

define i64 @mask64(i64 %x) {
; CHECK-LABEL: mask64:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    notq %rax
; CHECK-NEXT:    retq
  %m0 = bitcast i64 %x to <64 x i1>
  %m1 = xor <64 x i1> %m0, <i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1>
  %ret = bitcast <64 x i1> %m1 to i64
  ret i64 %ret
}

define void @mask32_mem(ptr %ptr) {
; CHECK-LABEL: mask32_mem:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    kmovd (%rdi), %k0
; CHECK-NEXT:    knotd %k0, %k0
; CHECK-NEXT:    kmovd %k0, (%rdi)
; CHECK-NEXT:    retq
  %x = load i32, ptr %ptr, align 4
  %m0 = bitcast i32 %x to <32 x i1>
  %m1 = xor <32 x i1> %m0, <i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1>
  %ret = bitcast <32 x i1> %m1 to i32
  store i32 %ret, ptr %ptr, align 4
  ret void
}

define void @mask64_mem(ptr %ptr) {
; CHECK-LABEL: mask64_mem:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    kmovq (%rdi), %k0
; CHECK-NEXT:    knotq %k0, %k0
; CHECK-NEXT:    kmovq %k0, (%rdi)
; CHECK-NEXT:    retq
  %x = load i64, ptr %ptr, align 4
  %m0 = bitcast i64 %x to <64 x i1>
  %m1 = xor <64 x i1> %m0, <i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1,
                            i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1, i1 -1>
  %ret = bitcast <64 x i1> %m1 to i64
  store i64 %ret, ptr %ptr, align 4
  ret void
}

define i32 @mand32(i32 %x, i32 %y) {
; CHECK-LABEL: mand32:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    andl %esi, %eax
; CHECK-NEXT:    xorl %esi, %edi
; CHECK-NEXT:    orl %edi, %eax
; CHECK-NEXT:    retq
  %ma = bitcast i32 %x to <32 x i1>
  %mb = bitcast i32 %y to <32 x i1>
  %mc = and <32 x i1> %ma, %mb
  %md = xor <32 x i1> %ma, %mb
  %me = or <32 x i1> %mc, %md
  %ret = bitcast <32 x i1> %me to i32
  ret i32 %ret
}

define i32 @mand32_mem(ptr %x, ptr %y) {
; CHECK-LABEL: mand32_mem:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    kmovd (%rdi), %k0
; CHECK-NEXT:    kmovd (%rsi), %k1
; CHECK-NEXT:    kandd %k1, %k0, %k2
; CHECK-NEXT:    kxord %k1, %k0, %k0
; CHECK-NEXT:    kord %k0, %k2, %k0
; CHECK-NEXT:    kmovd %k0, %eax
; CHECK-NEXT:    retq
  %ma = load <32 x i1>, ptr %x
  %mb = load <32 x i1>, ptr %y
  %mc = and <32 x i1> %ma, %mb
  %md = xor <32 x i1> %ma, %mb
  %me = or <32 x i1> %mc, %md
  %ret = bitcast <32 x i1> %me to i32
  ret i32 %ret
}

define i64 @mand64(i64 %x, i64 %y) {
; CHECK-LABEL: mand64:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    andq %rsi, %rax
; CHECK-NEXT:    xorq %rsi, %rdi
; CHECK-NEXT:    orq %rdi, %rax
; CHECK-NEXT:    retq
  %ma = bitcast i64 %x to <64 x i1>
  %mb = bitcast i64 %y to <64 x i1>
  %mc = and <64 x i1> %ma, %mb
  %md = xor <64 x i1> %ma, %mb
  %me = or <64 x i1> %mc, %md
  %ret = bitcast <64 x i1> %me to i64
  ret i64 %ret
}

define i64 @mand64_mem(ptr %x, ptr %y) {
; CHECK-LABEL: mand64_mem:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    kmovq (%rdi), %k0
; CHECK-NEXT:    kmovq (%rsi), %k1
; CHECK-NEXT:    kandq %k1, %k0, %k2
; CHECK-NEXT:    kxorq %k1, %k0, %k0
; CHECK-NEXT:    korq %k0, %k2, %k0
; CHECK-NEXT:    kmovq %k0, %rax
; CHECK-NEXT:    retq
  %ma = load <64 x i1>, ptr %x
  %mb = load <64 x i1>, ptr %y
  %mc = and <64 x i1> %ma, %mb
  %md = xor <64 x i1> %ma, %mb
  %me = or <64 x i1> %mc, %md
  %ret = bitcast <64 x i1> %me to i64
  ret i64 %ret
}

define i32 @test_v32i1_add(i32 %x, i32 %y) {
; CHECK-LABEL: test_v32i1_add:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    xorl %esi, %eax
; CHECK-NEXT:    retq
  %m0 = bitcast i32 %x to <32 x i1>
  %m1 = bitcast i32 %y to <32 x i1>
  %m2 = add <32 x i1> %m0,  %m1
  %ret = bitcast <32 x i1> %m2 to i32
  ret i32 %ret
}

define i32 @test_v32i1_sub(i32 %x, i32 %y) {
; CHECK-LABEL: test_v32i1_sub:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    xorl %esi, %eax
; CHECK-NEXT:    retq
  %m0 = bitcast i32 %x to <32 x i1>
  %m1 = bitcast i32 %y to <32 x i1>
  %m2 = sub <32 x i1> %m0,  %m1
  %ret = bitcast <32 x i1> %m2 to i32
  ret i32 %ret
}

define i32 @test_v32i1_mul(i32 %x, i32 %y) {
; CHECK-LABEL: test_v32i1_mul:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    andl %esi, %eax
; CHECK-NEXT:    retq
  %m0 = bitcast i32 %x to <32 x i1>
  %m1 = bitcast i32 %y to <32 x i1>
  %m2 = mul <32 x i1> %m0,  %m1
  %ret = bitcast <32 x i1> %m2 to i32
  ret i32 %ret
}

define i64 @test_v64i1_add(i64 %x, i64 %y) {
; CHECK-LABEL: test_v64i1_add:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    xorq %rsi, %rax
; CHECK-NEXT:    retq
  %m0 = bitcast i64 %x to <64 x i1>
  %m1 = bitcast i64 %y to <64 x i1>
  %m2 = add <64 x i1> %m0,  %m1
  %ret = bitcast <64 x i1> %m2 to i64
  ret i64 %ret
}

define i64 @test_v64i1_sub(i64 %x, i64 %y) {
; CHECK-LABEL: test_v64i1_sub:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    xorq %rsi, %rax
; CHECK-NEXT:    retq
  %m0 = bitcast i64 %x to <64 x i1>
  %m1 = bitcast i64 %y to <64 x i1>
  %m2 = sub <64 x i1> %m0,  %m1
  %ret = bitcast <64 x i1> %m2 to i64
  ret i64 %ret
}

define i64 @test_v64i1_mul(i64 %x, i64 %y) {
; CHECK-LABEL: test_v64i1_mul:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    andq %rsi, %rax
; CHECK-NEXT:    retq
  %m0 = bitcast i64 %x to <64 x i1>
  %m1 = bitcast i64 %y to <64 x i1>
  %m2 = mul <64 x i1> %m0,  %m1
  %ret = bitcast <64 x i1> %m2 to i64
  ret i64 %ret
}

define <32 x i1> @bitcast_f32_to_v32i1(float %x) {
; CHECK-LABEL: bitcast_f32_to_v32i1:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    vmovd %xmm0, %eax
; CHECK-NEXT:    kmovd %eax, %k0
; CHECK-NEXT:    vpmovm2b %k0, %ymm0
; CHECK-NEXT:    retq
  %a = bitcast float %x to <32 x i1>
  ret <32 x i1> %a
}

define <64 x i1> @bitcast_f64_to_v64i1(double %x) {
; CHECK-LABEL: bitcast_f64_to_v64i1:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    vmovq %xmm0, %rax
; CHECK-NEXT:    kmovq %rax, %k0
; CHECK-NEXT:    vpmovm2b %k0, %zmm0
; CHECK-NEXT:    retq
  %a = bitcast double %x to <64 x i1>
  ret <64 x i1> %a
}

define float @bitcast_v32i1_to_f32(<32 x i1> %x) {
; CHECK-LABEL: bitcast_v32i1_to_f32:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    vpsllw $7, %ymm0, %ymm0
; CHECK-NEXT:    vpmovmskb %ymm0, %eax
; CHECK-NEXT:    vmovd %eax, %xmm0
; CHECK-NEXT:    vzeroupper
; CHECK-NEXT:    retq
  %a = bitcast <32 x i1> %x to float
  ret float %a
}

define double @bitcast_v64i1_to_f64(<64 x i1> %x) {
; CHECK-LABEL: bitcast_v64i1_to_f64:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    vpsllw $7, %zmm0, %zmm0
; CHECK-NEXT:    vpmovb2m %zmm0, %k0
; CHECK-NEXT:    kmovq %k0, %rax
; CHECK-NEXT:    vmovq %rax, %xmm0
; CHECK-NEXT:    vzeroupper
; CHECK-NEXT:    retq
  %a = bitcast <64 x i1> %x to double
  ret double %a
}

