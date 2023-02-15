; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=thumbv8.1m.main-none-none-eabi -mattr=+mve -verify-machineinstrs %s -o - | FileCheck %s

define void @vmovl_s32(i32* noalias nocapture %d, i32* nocapture readonly %s, i32 %n) {
; CHECK-LABEL: vmovl_s32:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r7, lr}
; CHECK-NEXT:    push {r7, lr}
; CHECK-NEXT:    cmp r2, #1
; CHECK-NEXT:    it lt
; CHECK-NEXT:    poplt {r7, pc}
; CHECK-NEXT:  .LBB0_1: @ %vector.ph
; CHECK-NEXT:    dlstp.32 lr, r2
; CHECK-NEXT:  .LBB0_2: @ %vector.body
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    vldrw.u32 q0, [r1], #16
; CHECK-NEXT:    vmovlb.s16 q0, q0
; CHECK-NEXT:    vstrw.32 q0, [r0], #16
; CHECK-NEXT:    letp lr, .LBB0_2
; CHECK-NEXT:  @ %bb.3: @ %for.cond.cleanup
; CHECK-NEXT:    pop {r7, pc}
entry:
  %cmp7 = icmp sgt i32 %n, 0
  br i1 %cmp7, label %vector.ph, label %for.cond.cleanup

vector.ph:                                        ; preds = %entry
  %n.rnd.up = add i32 %n, 3
  %n.vec = and i32 %n.rnd.up, -4
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph
  %index = phi i32 [ 0, %vector.ph ], [ %index.next, %vector.body ]
  %active.lane.mask = call <4 x i1> @llvm.get.active.lane.mask.v4i1.i32(i32 %index, i32 %n)
  %0 = getelementptr inbounds i32, i32* %s, i32 %index
  %1 = bitcast i32* %0 to <4 x i32>*
  %wide.masked.load = call <4 x i32> @llvm.masked.load.v4i32.p0v4i32(<4 x i32>* %1, i32 4, <4 x i1> %active.lane.mask, <4 x i32> poison)
  %2 = shl <4 x i32> %wide.masked.load, <i32 16, i32 16, i32 16, i32 16>
  %3 = ashr exact <4 x i32> %2, <i32 16, i32 16, i32 16, i32 16>
  %4 = getelementptr inbounds i32, i32* %d, i32 %index
  %5 = bitcast i32* %4 to <4 x i32>*
  call void @llvm.masked.store.v4i32.p0v4i32(<4 x i32> %3, <4 x i32>* %5, i32 4, <4 x i1> %active.lane.mask)
  %index.next = add i32 %index, 4
  %6 = icmp eq i32 %index.next, %n.vec
  br i1 %6, label %for.cond.cleanup, label %vector.body

for.cond.cleanup:                                 ; preds = %vector.body, %entry
  ret void
}


define void @vmovl_u16(i16* noalias nocapture %d, i16* nocapture readonly %s, i32 %n) {
; CHECK-LABEL: vmovl_u16:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r7, lr}
; CHECK-NEXT:    push {r7, lr}
; CHECK-NEXT:    cmp r2, #1
; CHECK-NEXT:    it lt
; CHECK-NEXT:    poplt {r7, pc}
; CHECK-NEXT:  .LBB1_1: @ %vector.ph
; CHECK-NEXT:    dlstp.16 lr, r2
; CHECK-NEXT:  .LBB1_2: @ %vector.body
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    vldrh.u16 q0, [r1], #16
; CHECK-NEXT:    vmovlb.u8 q0, q0
; CHECK-NEXT:    vstrh.16 q0, [r0], #16
; CHECK-NEXT:    letp lr, .LBB1_2
; CHECK-NEXT:  @ %bb.3: @ %for.cond.cleanup
; CHECK-NEXT:    pop {r7, pc}
entry:
  %cmp7 = icmp sgt i32 %n, 0
  br i1 %cmp7, label %vector.ph, label %for.cond.cleanup

vector.ph:                                        ; preds = %entry
  %n.rnd.up = add i32 %n, 7
  %n.vec = and i32 %n.rnd.up, -8
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph
  %index = phi i32 [ 0, %vector.ph ], [ %index.next, %vector.body ]
  %active.lane.mask = call <8 x i1> @llvm.get.active.lane.mask.v8i1.i32(i32 %index, i32 %n)
  %0 = getelementptr inbounds i16, i16* %s, i32 %index
  %1 = bitcast i16* %0 to <8 x i16>*
  %wide.masked.load = call <8 x i16> @llvm.masked.load.v8i16.p0v8i16(<8 x i16>* %1, i32 2, <8 x i1> %active.lane.mask, <8 x i16> poison)
  %2 = and <8 x i16> %wide.masked.load, <i16 255, i16 255, i16 255, i16 255, i16 255, i16 255, i16 255, i16 255>
  %3 = getelementptr inbounds i16, i16* %d, i32 %index
  %4 = bitcast i16* %3 to <8 x i16>*
  call void @llvm.masked.store.v8i16.p0v8i16(<8 x i16> %2, <8 x i16>* %4, i32 2, <8 x i1> %active.lane.mask)
  %index.next = add i32 %index, 8
  %5 = icmp eq i32 %index.next, %n.vec
  br i1 %5, label %for.cond.cleanup, label %vector.body

for.cond.cleanup:                                 ; preds = %vector.body, %entry
  ret void
}

define void @vmovl_16to32(i16* %d, i16* %s, i32 %n) {
; CHECK-LABEL: vmovl_16to32:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r7, lr}
; CHECK-NEXT:    push {r7, lr}
; CHECK-NEXT:    cmp r2, #1
; CHECK-NEXT:    it lt
; CHECK-NEXT:    poplt {r7, pc}
; CHECK-NEXT:  .LBB2_1: @ %for.body.preheader
; CHECK-NEXT:    mov r3, r2
; CHECK-NEXT:    cmp r2, #8
; CHECK-NEXT:    it ge
; CHECK-NEXT:    movge r3, #8
; CHECK-NEXT:    subs r3, r2, r3
; CHECK-NEXT:    add.w r12, r3, #7
; CHECK-NEXT:    movs r3, #1
; CHECK-NEXT:    add.w r3, r3, r12, lsr #3
; CHECK-NEXT:    dls lr, r3
; CHECK-NEXT:  .LBB2_2: @ %for.body
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    vctp.16 r2
; CHECK-NEXT:    subs r2, #8
; CHECK-NEXT:    vpst
; CHECK-NEXT:    vldrht.u16 q0, [r1], #16
; CHECK-NEXT:    vmovlb.s16 q0, q0
; CHECK-NEXT:    vpst
; CHECK-NEXT:    vstrht.16 q0, [r0], #16
; CHECK-NEXT:    le lr, .LBB2_2
; CHECK-NEXT:  @ %bb.3: @ %for.cond.cleanup
; CHECK-NEXT:    pop {r7, pc}
entry:
  %cmp13 = icmp sgt i32 %n, 0
  br i1 %cmp13, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.body, %entry
  ret void

for.body:                                         ; preds = %entry, %for.body
  %d.addr.016 = phi i16* [ %add.ptr3, %for.body ], [ %d, %entry ]
  %s.addr.015 = phi i16* [ %add.ptr, %for.body ], [ %s, %entry ]
  %i.014 = phi i32 [ %sub, %for.body ], [ %n, %entry ]
  %0 = tail call <8 x i1> @llvm.arm.mve.vctp16(i32 %i.014)
  %1 = bitcast i16* %s.addr.015 to <8 x i16>*
  %2 = tail call <8 x i16> @llvm.masked.load.v8i16.p0v8i16(<8 x i16>* %1, i32 2, <8 x i1> %0, <8 x i16> <i16 0, i16 poison, i16 0, i16 poison, i16 0, i16 poison, i16 0, i16 poison>)
  %add.ptr = getelementptr inbounds i16, i16* %s.addr.015, i32 8
  %3 = shufflevector <8 x i16> %2, <8 x i16> poison, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %4 = sext <4 x i16> %3 to <4 x i32>
  %5 = bitcast <4 x i32> %4 to <8 x i16>
  %6 = bitcast i16* %d.addr.016 to <8 x i16>*
  tail call void @llvm.masked.store.v8i16.p0v8i16(<8 x i16> %5, <8 x i16>* %6, i32 2, <8 x i1> %0)
  %add.ptr3 = getelementptr inbounds i16, i16* %d.addr.016, i32 8
  %sub = add nsw i32 %i.014, -8
  %cmp = icmp sgt i32 %i.014, 8
  br i1 %cmp, label %for.body, label %for.cond.cleanup
}

define void @sunken_vmovl(i8* noalias %pTarget, i16 signext %iTargetStride, i8* noalias %pchAlpha, i16 signext %iAlphaStride, i16 %0, i8 zeroext %Colour) {
; CHECK-LABEL: sunken_vmovl:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r7, lr}
; CHECK-NEXT:    push {r7, lr}
; CHECK-NEXT:    ldrsh.w r1, [sp, #8]
; CHECK-NEXT:    vmov.i16 q0, #0x100
; CHECK-NEXT:    vldrb.u16 q1, [r2], #8
; CHECK-NEXT:    vldrb.u16 q2, [r0], #8
; CHECK-NEXT:    ldr r3, [sp, #12]
; CHECK-NEXT:    dlstp.16 lr, r1
; CHECK-NEXT:  .LBB3_1: @ %do.body
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    vmovlb.u8 q1, q1
; CHECK-NEXT:    vsub.i16 q3, q0, q1
; CHECK-NEXT:    vmovlb.u8 q2, q2
; CHECK-NEXT:    vmul.i16 q3, q2, q3
; CHECK-NEXT:    vldrb.u16 q2, [r0], #8
; CHECK-NEXT:    vmla.u16 q3, q1, r3
; CHECK-NEXT:    vldrb.u16 q1, [r2], #8
; CHECK-NEXT:    vshr.u16 q3, q3, #8
; CHECK-NEXT:    vstrb.16 q3, [r0, #-16]
; CHECK-NEXT:    letp lr, .LBB3_1
; CHECK-NEXT:  @ %bb.2: @ %do.end
; CHECK-NEXT:    pop {r7, pc}
entry:
  %conv3 = sext i16 %0 to i32
  %1 = zext i8 %Colour to i32
  %2 = bitcast i8* %pTarget to <8 x i8>*
  %3 = load <8 x i8>, <8 x i8>* %2, align 1
  %4 = bitcast i8* %pchAlpha to <8 x i8>*
  %5 = load <8 x i8>, <8 x i8>* %4, align 1
  br label %do.body

do.body:                                          ; preds = %do.body, %entry
  %pchAlpha.addr.0.pn = phi i8* [ %pchAlpha, %entry ], [ %pAlpha.0, %do.body ]
  %pTarget8.0 = phi i8* [ %pTarget, %entry ], [ %add.ptr5, %do.body ]
  %blkCnt.0 = phi i32 [ %conv3, %entry ], [ %sub, %do.body ]
  %vecTarget.0.in = phi <8 x i8> [ %3, %entry ], [ %10, %do.body ]
  %vecTransp.0.in = phi <8 x i8> [ %5, %entry ], [ %13, %do.body ]
  %vecTransp.0 = zext <8 x i8> %vecTransp.0.in to <8 x i16>
  %vecTarget.0 = zext <8 x i8> %vecTarget.0.in to <8 x i16>
  %pAlpha.0 = getelementptr inbounds i8, i8* %pchAlpha.addr.0.pn, i32 8
  %6 = tail call <8 x i1> @llvm.arm.mve.vctp16(i32 %blkCnt.0)
  %7 = tail call <8 x i16> @llvm.arm.mve.sub.predicated.v8i16.v8i1(<8 x i16> <i16 256, i16 256, i16 256, i16 256, i16 256, i16 256, i16 256, i16 256>, <8 x i16> %vecTransp.0, <8 x i1> %6, <8 x i16> undef)
  %8 = tail call <8 x i16> @llvm.arm.mve.mul.predicated.v8i16.v8i1(<8 x i16> %vecTarget.0, <8 x i16> %7, <8 x i1> %6, <8 x i16> undef)
  %add.ptr5 = getelementptr inbounds i8, i8* %pTarget8.0, i32 8
  %9 = bitcast i8* %add.ptr5 to <8 x i8>*
  %10 = tail call <8 x i8> @llvm.masked.load.v8i8.p0v8i8(<8 x i8>* nonnull %9, i32 1, <8 x i1> %6, <8 x i8> zeroinitializer)
  %11 = tail call <8 x i16> @llvm.arm.mve.vmla.n.predicated.v8i16.v8i1(<8 x i16> %8, <8 x i16> %vecTransp.0, i32 %1, <8 x i1> %6)
  %12 = bitcast i8* %pAlpha.0 to <8 x i8>*
  %13 = tail call <8 x i8> @llvm.masked.load.v8i8.p0v8i8(<8 x i8>* nonnull %12, i32 1, <8 x i1> %6, <8 x i8> zeroinitializer)
  %14 = tail call <8 x i16> @llvm.arm.mve.shr.imm.predicated.v8i16.v8i1(<8 x i16> %11, i32 8, i32 1, <8 x i1> %6, <8 x i16> %11)
  %15 = trunc <8 x i16> %14 to <8 x i8>
  %16 = bitcast i8* %pTarget8.0 to <8 x i8>*
  tail call void @llvm.masked.store.v8i8.p0v8i8(<8 x i8> %15, <8 x i8>* %16, i32 1, <8 x i1> %6)
  %sub = add nsw i32 %blkCnt.0, -8
  %cmp9 = icmp sgt i32 %blkCnt.0, 8
  br i1 %cmp9, label %do.body, label %do.end

do.end:                                           ; preds = %do.body
  ret void
}

declare <8 x i1> @llvm.get.active.lane.mask.v8i1.i32(i32, i32) #1
declare <8 x i16> @llvm.masked.load.v8i16.p0v8i16(<8 x i16>*, i32 immarg, <8 x i1>, <8 x i16>) #2
declare void @llvm.masked.store.v8i16.p0v8i16(<8 x i16>, <8 x i16>*, i32 immarg, <8 x i1>) #3
declare <4 x i1> @llvm.get.active.lane.mask.v4i1.i32(i32, i32) #1
declare <4 x i32> @llvm.masked.load.v4i32.p0v4i32(<4 x i32>*, i32 immarg, <4 x i1>, <4 x i32>) #2
declare void @llvm.masked.store.v4i32.p0v4i32(<4 x i32>, <4 x i32>*, i32 immarg, <4 x i1>) #3
declare <8 x i1> @llvm.arm.mve.vctp16(i32)
declare <8 x i16> @llvm.arm.mve.sub.predicated.v8i16.v8i1(<8 x i16>, <8 x i16>, <8 x i1>, <8 x i16>)
declare <8 x i16> @llvm.arm.mve.mul.predicated.v8i16.v8i1(<8 x i16>, <8 x i16>, <8 x i1>, <8 x i16>)
declare <8 x i8> @llvm.masked.load.v8i8.p0v8i8(<8 x i8>*, i32 immarg, <8 x i1>, <8 x i8>)
declare <8 x i16> @llvm.arm.mve.vmla.n.predicated.v8i16.v8i1(<8 x i16>, <8 x i16>, i32, <8 x i1>)
declare <8 x i16> @llvm.arm.mve.shr.imm.predicated.v8i16.v8i1(<8 x i16>, i32, i32, <8 x i1>, <8 x i16>)
declare void @llvm.masked.store.v8i8.p0v8i8(<8 x i8>, <8 x i8>*, i32 immarg, <8 x i1>)
