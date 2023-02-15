; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=thumbv8.1m.main-none-none-eabi -mattr=+mve.fp %s -o - -opaque-pointers | FileCheck %s

; VLDRB.8
define arm_aapcs_vfpcc void @unscaled_v16i8_i8(i8* %base, <16 x i8>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    vldrb.u8 q1, [r1]
; CHECK-NEXT:    vstrb.8 q0, [r0, q1]
; CHECK-NEXT:    bx lr
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %offs.zext = zext <16 x i8> %offs to <16 x i32>
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.zext
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @unscaled_v8i8_i8(i8* %base, <8 x i8>* %offptr, <8 x i8> %input) {
; CHECK-LABEL: unscaled_v8i8_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    vldrb.u16 q1, [r1]
; CHECK-NEXT:    vmovlb.u8 q0, q0
; CHECK-NEXT:    vstrb.16 q0, [r0, q1]
; CHECK-NEXT:    bx lr
entry:
  %offs = load <8 x i8>, <8 x i8>* %offptr, align 1
  %offs.zext = zext <8 x i8> %offs to <8 x i32>
  %ptrs = getelementptr inbounds i8, i8* %base, <8 x i32> %offs.zext
  call void @llvm.masked.scatter.v8i8.v8p0i8(<8 x i8> %input, <8 x i8*> %ptrs, i32 1, <8 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

; Expand
define arm_aapcs_vfpcc void @unscaled_v2i8_i8(i8* %base, <2 x i8>* %offptr, <2 x i8> %input) {
; CHECK-LABEL: unscaled_v2i8_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    ldrb r2, [r1]
; CHECK-NEXT:    vmov.i32 q1, #0xff
; CHECK-NEXT:    ldrb r1, [r1, #1]
; CHECK-NEXT:    vmov q2[2], q2[0], r2, r1
; CHECK-NEXT:    vmov r2, s0
; CHECK-NEXT:    vand q1, q2, q1
; CHECK-NEXT:    vmov r1, s4
; CHECK-NEXT:    strb r2, [r0, r1]
; CHECK-NEXT:    vmov r1, s6
; CHECK-NEXT:    vmov r2, s2
; CHECK-NEXT:    strb r2, [r0, r1]
; CHECK-NEXT:    bx lr
entry:
  %offs = load <2 x i8>, <2 x i8>* %offptr, align 1
  %offs.zext = zext <2 x i8> %offs to <2 x i32>
  %ptrs = getelementptr inbounds i8, i8* %base, <2 x i32> %offs.zext
  call void @llvm.masked.scatter.v2i8.v2p0i8(<2 x i8> %input, <2 x i8*> %ptrs, i32 1, <2 x i1> <i1 true, i1 true>)
  ret void
}

; Expand - sext offsets
define arm_aapcs_vfpcc void @unscaled_v16i8_sext(i8* %base, <16 x i8>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_sext:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    vldrb.s32 q1, [r1]
; CHECK-NEXT:    vldrb.s32 q3, [r1, #8]
; CHECK-NEXT:    vmov.u8 r6, q0[0]
; CHECK-NEXT:    vmov.u8 r5, q0[4]
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vadd.i32 q3, q3, r0
; CHECK-NEXT:    vmov r2, r3, d2
; CHECK-NEXT:    vmov.u8 r7, q0[6]
; CHECK-NEXT:    vmov r12, lr, d3
; CHECK-NEXT:    vldrb.s32 q1, [r1, #4]
; CHECK-NEXT:    vadd.i32 q2, q1, r0
; CHECK-NEXT:    vldrb.s32 q1, [r1, #12]
; CHECK-NEXT:    vmov r4, r8, d4
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov r0, r9, d5
; CHECK-NEXT:    strb r6, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[1]
; CHECK-NEXT:    strb r2, [r3]
; CHECK-NEXT:    vmov.u8 r6, q0[2]
; CHECK-NEXT:    vmov r2, r10, d6
; CHECK-NEXT:    strb.w r6, [r12]
; CHECK-NEXT:    vmov.u8 r6, q0[3]
; CHECK-NEXT:    vmov.u8 r3, q0[8]
; CHECK-NEXT:    strb.w r6, [lr]
; CHECK-NEXT:    vmov r6, r1, d7
; CHECK-NEXT:    strb r5, [r4]
; CHECK-NEXT:    vmov.u8 r5, q0[5]
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    vmov r5, r4, d2
; CHECK-NEXT:    strb r7, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[7]
; CHECK-NEXT:    strb.w r0, [r9]
; CHECK-NEXT:    vmov r0, r7, d3
; CHECK-NEXT:    strb r3, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[9]
; CHECK-NEXT:    strb.w r2, [r10]
; CHECK-NEXT:    vmov.u8 r2, q0[10]
; CHECK-NEXT:    strb r2, [r6]
; CHECK-NEXT:    vmov.u8 r2, q0[11]
; CHECK-NEXT:    strb r2, [r1]
; CHECK-NEXT:    vmov.u8 r1, q0[12]
; CHECK-NEXT:    strb r1, [r5]
; CHECK-NEXT:    vmov.u8 r1, q0[13]
; CHECK-NEXT:    strb r1, [r4]
; CHECK-NEXT:    vmov.u8 r1, q0[14]
; CHECK-NEXT:    strb r1, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[15]
; CHECK-NEXT:    strb r0, [r7]
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, pc}
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %offs.sext = sext <16 x i8> %offs to <16 x i32>
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.sext
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

; Expand - sext offsets
define arm_aapcs_vfpcc void @unscaled_v16i8_i16(i8* %base, <16 x i16>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_i16:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    vldrh.s32 q1, [r1]
; CHECK-NEXT:    vldrh.s32 q3, [r1, #16]
; CHECK-NEXT:    vmov.u8 r6, q0[0]
; CHECK-NEXT:    vmov.u8 r5, q0[4]
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vadd.i32 q3, q3, r0
; CHECK-NEXT:    vmov r2, r3, d2
; CHECK-NEXT:    vmov.u8 r7, q0[6]
; CHECK-NEXT:    vmov r12, lr, d3
; CHECK-NEXT:    vldrh.s32 q1, [r1, #8]
; CHECK-NEXT:    vadd.i32 q2, q1, r0
; CHECK-NEXT:    vldrh.s32 q1, [r1, #24]
; CHECK-NEXT:    vmov r4, r8, d4
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov r0, r9, d5
; CHECK-NEXT:    strb r6, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[1]
; CHECK-NEXT:    strb r2, [r3]
; CHECK-NEXT:    vmov.u8 r6, q0[2]
; CHECK-NEXT:    vmov r2, r10, d6
; CHECK-NEXT:    strb.w r6, [r12]
; CHECK-NEXT:    vmov.u8 r6, q0[3]
; CHECK-NEXT:    vmov.u8 r3, q0[8]
; CHECK-NEXT:    strb.w r6, [lr]
; CHECK-NEXT:    vmov r6, r1, d7
; CHECK-NEXT:    strb r5, [r4]
; CHECK-NEXT:    vmov.u8 r5, q0[5]
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    vmov r5, r4, d2
; CHECK-NEXT:    strb r7, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[7]
; CHECK-NEXT:    strb.w r0, [r9]
; CHECK-NEXT:    vmov r0, r7, d3
; CHECK-NEXT:    strb r3, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[9]
; CHECK-NEXT:    strb.w r2, [r10]
; CHECK-NEXT:    vmov.u8 r2, q0[10]
; CHECK-NEXT:    strb r2, [r6]
; CHECK-NEXT:    vmov.u8 r2, q0[11]
; CHECK-NEXT:    strb r2, [r1]
; CHECK-NEXT:    vmov.u8 r1, q0[12]
; CHECK-NEXT:    strb r1, [r5]
; CHECK-NEXT:    vmov.u8 r1, q0[13]
; CHECK-NEXT:    strb r1, [r4]
; CHECK-NEXT:    vmov.u8 r1, q0[14]
; CHECK-NEXT:    strb r1, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[15]
; CHECK-NEXT:    strb r0, [r7]
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, pc}
entry:
  %offs = load <16 x i16>, <16 x i16>* %offptr, align 2
  %offs.sext = sext <16 x i16> %offs to <16 x i32>
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.sext
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

; Could be manually scaled offsets
define arm_aapcs_vfpcc void @unscaled_v16i8_scaled(i32* %base, <16 x i8>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_scaled:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    vldrb.u32 q1, [r1]
; CHECK-NEXT:    vldrb.u32 q3, [r1, #8]
; CHECK-NEXT:    vmov.u8 r6, q0[0]
; CHECK-NEXT:    vmov.u8 r7, q0[4]
; CHECK-NEXT:    vshl.i32 q1, q1, #2
; CHECK-NEXT:    vshl.i32 q3, q3, #2
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vadd.i32 q3, q3, r0
; CHECK-NEXT:    vmov r2, r3, d2
; CHECK-NEXT:    vmov r12, lr, d3
; CHECK-NEXT:    vldrb.u32 q1, [r1, #4]
; CHECK-NEXT:    vshl.i32 q1, q1, #2
; CHECK-NEXT:    vadd.i32 q2, q1, r0
; CHECK-NEXT:    vldrb.u32 q1, [r1, #12]
; CHECK-NEXT:    vmov r4, r8, d4
; CHECK-NEXT:    vmov.u8 r1, q0[6]
; CHECK-NEXT:    vshl.i32 q1, q1, #2
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov r0, r9, d5
; CHECK-NEXT:    strb r6, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[1]
; CHECK-NEXT:    strb r2, [r3]
; CHECK-NEXT:    vmov.u8 r6, q0[2]
; CHECK-NEXT:    vmov r2, r10, d6
; CHECK-NEXT:    strb.w r6, [r12]
; CHECK-NEXT:    vmov.u8 r6, q0[3]
; CHECK-NEXT:    vmov.u8 r3, q0[8]
; CHECK-NEXT:    strb.w r6, [lr]
; CHECK-NEXT:    vmov r6, r5, d7
; CHECK-NEXT:    strb r7, [r4]
; CHECK-NEXT:    vmov.u8 r7, q0[5]
; CHECK-NEXT:    strb.w r7, [r8]
; CHECK-NEXT:    vmov r7, r4, d2
; CHECK-NEXT:    strb r1, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[7]
; CHECK-NEXT:    strb.w r0, [r9]
; CHECK-NEXT:    vmov r0, r1, d3
; CHECK-NEXT:    strb r3, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[9]
; CHECK-NEXT:    strb.w r2, [r10]
; CHECK-NEXT:    vmov.u8 r2, q0[10]
; CHECK-NEXT:    strb r2, [r6]
; CHECK-NEXT:    vmov.u8 r2, q0[11]
; CHECK-NEXT:    strb r2, [r5]
; CHECK-NEXT:    vmov.u8 r2, q0[12]
; CHECK-NEXT:    strb r2, [r7]
; CHECK-NEXT:    vmov.u8 r2, q0[13]
; CHECK-NEXT:    strb r2, [r4]
; CHECK-NEXT:    vmov.u8 r2, q0[14]
; CHECK-NEXT:    strb r2, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[15]
; CHECK-NEXT:    strb r0, [r1]
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, pc}
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 4
  %offs.zext = zext <16 x i8> %offs to <16 x i32>
  %ptrs32 = getelementptr inbounds i32, i32* %base, <16 x i32> %offs.zext
  %ptrs = bitcast <16 x i32*> %ptrs32 to <16 x i8*>
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

; Expand - large offsets
define arm_aapcs_vfpcc void @unscaled_v16i8_i8_next(i8* %base, <16 x i32>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_i8_next:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, lr}
; CHECK-NEXT:    vldrw.u32 q1, [r1]
; CHECK-NEXT:    vldrw.u32 q3, [r1, #32]
; CHECK-NEXT:    vmov.u8 r6, q0[0]
; CHECK-NEXT:    vmov.u8 r5, q0[4]
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vadd.i32 q3, q3, r0
; CHECK-NEXT:    vmov r2, r3, d2
; CHECK-NEXT:    vmov.u8 r7, q0[6]
; CHECK-NEXT:    vmov r12, lr, d3
; CHECK-NEXT:    vldrw.u32 q1, [r1, #16]
; CHECK-NEXT:    vadd.i32 q2, q1, r0
; CHECK-NEXT:    vldrw.u32 q1, [r1, #48]
; CHECK-NEXT:    vmov r4, r8, d4
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov r0, r9, d5
; CHECK-NEXT:    strb r6, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[1]
; CHECK-NEXT:    strb r2, [r3]
; CHECK-NEXT:    vmov.u8 r6, q0[2]
; CHECK-NEXT:    vmov r2, r10, d6
; CHECK-NEXT:    strb.w r6, [r12]
; CHECK-NEXT:    vmov.u8 r6, q0[3]
; CHECK-NEXT:    vmov.u8 r3, q0[8]
; CHECK-NEXT:    strb.w r6, [lr]
; CHECK-NEXT:    vmov r6, r1, d7
; CHECK-NEXT:    strb r5, [r4]
; CHECK-NEXT:    vmov.u8 r5, q0[5]
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    vmov r5, r4, d2
; CHECK-NEXT:    strb r7, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[7]
; CHECK-NEXT:    strb.w r0, [r9]
; CHECK-NEXT:    vmov r0, r7, d3
; CHECK-NEXT:    strb r3, [r2]
; CHECK-NEXT:    vmov.u8 r2, q0[9]
; CHECK-NEXT:    strb.w r2, [r10]
; CHECK-NEXT:    vmov.u8 r2, q0[10]
; CHECK-NEXT:    strb r2, [r6]
; CHECK-NEXT:    vmov.u8 r2, q0[11]
; CHECK-NEXT:    strb r2, [r1]
; CHECK-NEXT:    vmov.u8 r1, q0[12]
; CHECK-NEXT:    strb r1, [r5]
; CHECK-NEXT:    vmov.u8 r1, q0[13]
; CHECK-NEXT:    strb r1, [r4]
; CHECK-NEXT:    vmov.u8 r1, q0[14]
; CHECK-NEXT:    strb r1, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[15]
; CHECK-NEXT:    strb r0, [r7]
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, pc}
entry:
  %offs = load <16 x i32>, <16 x i32>* %offptr, align 4
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @trunc_unsigned_unscaled_i64_i8(i8* %base, <16 x i8>* %offptr, <16 x i64> %input) {
; CHECK-LABEL: trunc_unsigned_unscaled_i64_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, lr}
; CHECK-NEXT:    push {r4, lr}
; CHECK-NEXT:    .vsave {d8, d9, d10, d11}
; CHECK-NEXT:    vpush {d8, d9, d10, d11}
; CHECK-NEXT:    vmov r4, s0
; CHECK-NEXT:    add r3, sp, #40
; CHECK-NEXT:    vmov.8 q5[0], r4
; CHECK-NEXT:    vmov r4, s2
; CHECK-NEXT:    vmov.8 q5[1], r4
; CHECK-NEXT:    vmov r4, s4
; CHECK-NEXT:    vmov.8 q5[2], r4
; CHECK-NEXT:    vmov r4, s6
; CHECK-NEXT:    vmov.8 q5[3], r4
; CHECK-NEXT:    vmov r4, s8
; CHECK-NEXT:    vmov.8 q5[4], r4
; CHECK-NEXT:    vmov r4, s10
; CHECK-NEXT:    vldrw.u32 q0, [r3]
; CHECK-NEXT:    vmov.8 q5[5], r4
; CHECK-NEXT:    vmov r4, s12
; CHECK-NEXT:    add.w lr, sp, #56
; CHECK-NEXT:    vmov.8 q5[6], r4
; CHECK-NEXT:    vmov r4, s14
; CHECK-NEXT:    vmov.8 q5[7], r4
; CHECK-NEXT:    vmov r3, s0
; CHECK-NEXT:    vmov.8 q5[8], r3
; CHECK-NEXT:    vmov r3, s2
; CHECK-NEXT:    vldrw.u32 q0, [lr]
; CHECK-NEXT:    vmov.8 q5[9], r3
; CHECK-NEXT:    add.w r12, sp, #72
; CHECK-NEXT:    add r2, sp, #88
; CHECK-NEXT:    vmov r3, s0
; CHECK-NEXT:    vldrw.u32 q4, [r2]
; CHECK-NEXT:    vmov.8 q5[10], r3
; CHECK-NEXT:    vmov r3, s2
; CHECK-NEXT:    vldrw.u32 q0, [r12]
; CHECK-NEXT:    vmov.8 q5[11], r3
; CHECK-NEXT:    vmov r2, s18
; CHECK-NEXT:    vmov r3, s0
; CHECK-NEXT:    vmov.8 q5[12], r3
; CHECK-NEXT:    vmov r3, s2
; CHECK-NEXT:    vmov.8 q5[13], r3
; CHECK-NEXT:    vmov r3, s16
; CHECK-NEXT:    vmov.8 q5[14], r3
; CHECK-NEXT:    vldrb.u8 q0, [r1]
; CHECK-NEXT:    vmov.8 q5[15], r2
; CHECK-NEXT:    vstrb.8 q5, [r0, q0]
; CHECK-NEXT:    vpop {d8, d9, d10, d11}
; CHECK-NEXT:    pop {r4, pc}
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %offs.zext = zext <16 x i8> %offs to <16 x i32>
  %byte_ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.zext
  %input.trunc = trunc <16 x i64> %input to <16 x i8>
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input.trunc, <16 x i8*> %byte_ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @trunc_unsigned_unscaled_i32_i8(i8* %base, <16 x i8>* %offptr, <16 x i32> %input) {
; CHECK-LABEL: trunc_unsigned_unscaled_i32_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .pad #16
; CHECK-NEXT:    sub sp, #16
; CHECK-NEXT:    mov r2, sp
; CHECK-NEXT:    vstrb.32 q3, [r2, #12]
; CHECK-NEXT:    vstrb.32 q2, [r2, #8]
; CHECK-NEXT:    vstrb.32 q1, [r2, #4]
; CHECK-NEXT:    vstrb.32 q0, [r2]
; CHECK-NEXT:    vldrb.u8 q0, [r1]
; CHECK-NEXT:    vldrw.u32 q1, [r2]
; CHECK-NEXT:    vstrb.8 q1, [r0, q0]
; CHECK-NEXT:    add sp, #16
; CHECK-NEXT:    bx lr
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %offs.zext = zext <16 x i8> %offs to <16 x i32>
  %byte_ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.zext
  %input.trunc = trunc <16 x i32> %input to <16 x i8>
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input.trunc, <16 x i8*> %byte_ptrs, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @trunc_unsigned_unscaled_i16_i8(i8* %base, <16 x i8>* %offptr, <16 x i16> %input) {
; CHECK-LABEL: trunc_unsigned_unscaled_i16_i8:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .pad #16
; CHECK-NEXT:    sub sp, #16
; CHECK-NEXT:    mov r2, sp
; CHECK-NEXT:    vstrb.16 q1, [r2, #8]
; CHECK-NEXT:    vstrb.16 q0, [r2]
; CHECK-NEXT:    vldrb.u8 q0, [r1]
; CHECK-NEXT:    vldrw.u32 q1, [r2]
; CHECK-NEXT:    vstrb.8 q1, [r0, q0]
; CHECK-NEXT:    add sp, #16
; CHECK-NEXT:    bx lr
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %offs.zext = zext <16 x i8> %offs to <16 x i32>
  %byte_ptrs = getelementptr inbounds i8, i8* %base, <16 x i32> %offs.zext
  %input.trunc = trunc <16 x i16> %input to <16 x i8>
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input.trunc, <16 x i8*> %byte_ptrs, i32 4, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @unscaled_v16i8_i8_2gep(i8* %base, <16 x i8>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_i8_2gep:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, lr}
; CHECK-NEXT:    vldrb.s32 q1, [r1]
; CHECK-NEXT:    movs r2, #5
; CHECK-NEXT:    vldrb.s32 q3, [r1, #8]
; CHECK-NEXT:    vmov.u8 r6, q0[0]
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov.u8 r4, q0[2]
; CHECK-NEXT:    vadd.i32 q1, q1, r2
; CHECK-NEXT:    vadd.i32 q3, q3, r0
; CHECK-NEXT:    vmov r3, r12, d2
; CHECK-NEXT:    vadd.i32 q3, q3, r2
; CHECK-NEXT:    vmov lr, r7, d3
; CHECK-NEXT:    vldrb.s32 q1, [r1, #4]
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vadd.i32 q2, q1, r2
; CHECK-NEXT:    vldrb.s32 q1, [r1, #12]
; CHECK-NEXT:    vmov r1, r5, d4
; CHECK-NEXT:    vadd.i32 q1, q1, r0
; CHECK-NEXT:    vmov r0, r8, d5
; CHECK-NEXT:    vadd.i32 q1, q1, r2
; CHECK-NEXT:    vmov.u8 r2, q0[4]
; CHECK-NEXT:    strb r6, [r3]
; CHECK-NEXT:    vmov.u8 r3, q0[1]
; CHECK-NEXT:    strb.w r3, [r12]
; CHECK-NEXT:    vmov r3, r12, d6
; CHECK-NEXT:    strb.w r4, [lr]
; CHECK-NEXT:    vmov.u8 r4, q0[3]
; CHECK-NEXT:    strb r4, [r7]
; CHECK-NEXT:    vmov r7, r4, d7
; CHECK-NEXT:    vmov.u8 r6, q0[8]
; CHECK-NEXT:    strb r2, [r1]
; CHECK-NEXT:    vmov.u8 r1, q0[5]
; CHECK-NEXT:    strb r1, [r5]
; CHECK-NEXT:    vmov.u8 r5, q0[6]
; CHECK-NEXT:    vmov r1, r2, d2
; CHECK-NEXT:    strb r5, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[7]
; CHECK-NEXT:    strb.w r0, [r8]
; CHECK-NEXT:    vmov r0, r5, d3
; CHECK-NEXT:    strb r6, [r3]
; CHECK-NEXT:    vmov.u8 r3, q0[9]
; CHECK-NEXT:    strb.w r3, [r12]
; CHECK-NEXT:    vmov.u8 r3, q0[10]
; CHECK-NEXT:    strb r3, [r7]
; CHECK-NEXT:    vmov.u8 r3, q0[11]
; CHECK-NEXT:    strb r3, [r4]
; CHECK-NEXT:    vmov.u8 r3, q0[12]
; CHECK-NEXT:    strb r3, [r1]
; CHECK-NEXT:    vmov.u8 r1, q0[13]
; CHECK-NEXT:    strb r1, [r2]
; CHECK-NEXT:    vmov.u8 r1, q0[14]
; CHECK-NEXT:    strb r1, [r0]
; CHECK-NEXT:    vmov.u8 r0, q0[15]
; CHECK-NEXT:    strb r0, [r5]
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, pc}
entry:
  %offs = load <16 x i8>, <16 x i8>* %offptr, align 1
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i8> %offs
  %ptrs2 = getelementptr inbounds i8, <16 x i8*> %ptrs, i8 5
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs2, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}

define arm_aapcs_vfpcc void @unscaled_v16i8_i8_2gep2(i8* %base, <16 x i8>* %offptr, <16 x i8> %input) {
; CHECK-LABEL: unscaled_v16i8_i8_2gep2:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    adr r1, .LCPI11_0
; CHECK-NEXT:    vldrw.u32 q1, [r1]
; CHECK-NEXT:    vstrb.8 q0, [r0, q1]
; CHECK-NEXT:    bx lr
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  @ %bb.1:
; CHECK-NEXT:  .LCPI11_0:
; CHECK-NEXT:    .byte 5 @ 0x5
; CHECK-NEXT:    .byte 8 @ 0x8
; CHECK-NEXT:    .byte 11 @ 0xb
; CHECK-NEXT:    .byte 14 @ 0xe
; CHECK-NEXT:    .byte 17 @ 0x11
; CHECK-NEXT:    .byte 20 @ 0x14
; CHECK-NEXT:    .byte 23 @ 0x17
; CHECK-NEXT:    .byte 26 @ 0x1a
; CHECK-NEXT:    .byte 29 @ 0x1d
; CHECK-NEXT:    .byte 32 @ 0x20
; CHECK-NEXT:    .byte 35 @ 0x23
; CHECK-NEXT:    .byte 38 @ 0x26
; CHECK-NEXT:    .byte 41 @ 0x29
; CHECK-NEXT:    .byte 44 @ 0x2c
; CHECK-NEXT:    .byte 47 @ 0x2f
; CHECK-NEXT:    .byte 50 @ 0x32
entry:
  %ptrs = getelementptr inbounds i8, i8* %base, <16 x i8> <i8 0, i8 3, i8 6, i8 9, i8 12, i8 15, i8 18, i8 21, i8 24, i8 27, i8 30, i8 33, i8 36, i8 39, i8 42, i8 45>
  %ptrs2 = getelementptr inbounds i8, <16 x i8*> %ptrs, i8 5
  call void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8> %input, <16 x i8*> %ptrs2, i32 1, <16 x i1> <i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true, i1 true>)
  ret void
}


declare void @llvm.masked.scatter.v2i8.v2p0i8(<2 x i8>, <2 x i8*>, i32, <2 x i1>)
declare void @llvm.masked.scatter.v8i8.v8p0i8(<8 x i8>, <8 x i8*>, i32, <8 x i1>)
declare void @llvm.masked.scatter.v16i8.v16p0i8(<16 x i8>, <16 x i8*>, i32, <16 x i1>)
