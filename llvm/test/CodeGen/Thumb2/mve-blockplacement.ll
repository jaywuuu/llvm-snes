; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=thumbv8.1m.main-none-none-eabi -verify-machineinstrs -mattr=+mve %s -o - | FileCheck %s

@var_36 = hidden local_unnamed_addr global i8 0, align 1
@arr_61 = hidden local_unnamed_addr global [1 x i32] zeroinitializer, align 4

define i32 @test(i8 zeroext %var_2, i16 signext %var_15, [18 x [22 x i8]]* %arr_60) {
; CHECK-LABEL: test:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, r11, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, r11, lr}
; CHECK-NEXT:    cmp r0, #0
; CHECK-NEXT:    beq.w .LBB0_10
; CHECK-NEXT:  @ %bb.1: @ %for.cond1.preheader
; CHECK-NEXT:    cmp r2, #0
; CHECK-NEXT:    beq.w .LBB0_11
; CHECK-NEXT:  @ %bb.2: @ %for.cond1.preheader1
; CHECK-NEXT:    movw r8, :lower16:var_36
; CHECK-NEXT:    movw r0, #27476
; CHECK-NEXT:    addw r10, r2, #397
; CHECK-NEXT:    mov.w r9, #11
; CHECK-NEXT:    movt r8, :upper16:var_36
; CHECK-NEXT:    sdiv r1, r0, r1
; CHECK-NEXT:    mov.w r11, #0
; CHECK-NEXT:  .LBB0_3: @ %for.cond6.preheader
; CHECK-NEXT:    @ =>This Loop Header: Depth=1
; CHECK-NEXT:    @ Child Loop BB0_4 Depth 2
; CHECK-NEXT:    @ Child Loop BB0_6 Depth 2
; CHECK-NEXT:    @ Child Loop BB0_8 Depth 2
; CHECK-NEXT:    movs r0, #22
; CHECK-NEXT:    dls lr, r9
; CHECK-NEXT:    mla r7, r11, r0, r10
; CHECK-NEXT:    movw r0, :lower16:arr_61
; CHECK-NEXT:    movt r0, :upper16:arr_61
; CHECK-NEXT:    adds r0, #4
; CHECK-NEXT:    mov r3, r2
; CHECK-NEXT:    mov r6, r0
; CHECK-NEXT:  .LBB0_4: @ %for.body10
; CHECK-NEXT:    @ Parent Loop BB0_3 Depth=1
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=2
; CHECK-NEXT:    str r3, [r6, #-4]
; CHECK-NEXT:    add.w r12, r3, #396
; CHECK-NEXT:    ldrb r5, [r7, #-1]
; CHECK-NEXT:    add.w r3, r3, #792
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    str.w r12, [r6]
; CHECK-NEXT:    cset r5, ne
; CHECK-NEXT:    adds r6, #8
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    ldrb r5, [r7]
; CHECK-NEXT:    adds r7, #2
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    cset r5, ne
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    le lr, .LBB0_4
; CHECK-NEXT:  @ %bb.5: @ %for.cond.cleanup9
; CHECK-NEXT:    @ in Loop: Header=BB0_3 Depth=1
; CHECK-NEXT:    add.w r3, r11, #1
; CHECK-NEXT:    movs r7, #22
; CHECK-NEXT:    dls lr, r9
; CHECK-NEXT:    mov r6, r0
; CHECK-NEXT:    uxtb r3, r3
; CHECK-NEXT:    smlabb r7, r3, r7, r10
; CHECK-NEXT:    mov r3, r2
; CHECK-NEXT:  .LBB0_6: @ %for.body10.1
; CHECK-NEXT:    @ Parent Loop BB0_3 Depth=1
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=2
; CHECK-NEXT:    str r3, [r6, #-4]
; CHECK-NEXT:    add.w r4, r3, #396
; CHECK-NEXT:    ldrb r5, [r7, #-1]
; CHECK-NEXT:    add.w r3, r3, #792
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    str r4, [r6]
; CHECK-NEXT:    cset r5, ne
; CHECK-NEXT:    adds r6, #8
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    ldrb r5, [r7]
; CHECK-NEXT:    adds r7, #2
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    cset r5, ne
; CHECK-NEXT:    strb.w r5, [r8]
; CHECK-NEXT:    le lr, .LBB0_6
; CHECK-NEXT:  @ %bb.7: @ %for.cond.cleanup9.1
; CHECK-NEXT:    @ in Loop: Header=BB0_3 Depth=1
; CHECK-NEXT:    add.w r3, r11, #2
; CHECK-NEXT:    movs r7, #22
; CHECK-NEXT:    dls lr, r9
; CHECK-NEXT:    uxtb r3, r3
; CHECK-NEXT:    smlabb r7, r3, r7, r10
; CHECK-NEXT:    mov r3, r2
; CHECK-NEXT:  .LBB0_8: @ %for.body10.2
; CHECK-NEXT:    @ Parent Loop BB0_3 Depth=1
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=2
; CHECK-NEXT:    str r3, [r0, #-4]
; CHECK-NEXT:    ldrb r6, [r7, #-1]
; CHECK-NEXT:    cmp r6, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    add.w r6, r3, #396
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    str r6, [r0]
; CHECK-NEXT:    cset r6, ne
; CHECK-NEXT:    strb.w r6, [r8]
; CHECK-NEXT:    add.w r3, r3, #792
; CHECK-NEXT:    ldrb r6, [r7]
; CHECK-NEXT:    adds r0, #8
; CHECK-NEXT:    adds r7, #2
; CHECK-NEXT:    cmp r6, #0
; CHECK-NEXT:    ite ne
; CHECK-NEXT:    sxthne r5, r1
; CHECK-NEXT:    moveq r5, #0
; CHECK-NEXT:    cmp r5, #0
; CHECK-NEXT:    cset r6, ne
; CHECK-NEXT:    strb.w r6, [r8]
; CHECK-NEXT:    le lr, .LBB0_8
; CHECK-NEXT:  @ %bb.9: @ %for.cond.cleanup9.2
; CHECK-NEXT:    @ in Loop: Header=BB0_3 Depth=1
; CHECK-NEXT:    add.w r0, r11, #3
; CHECK-NEXT:    uxtb.w r11, r0
; CHECK-NEXT:    cmp.w r11, #18
; CHECK-NEXT:    it hs
; CHECK-NEXT:    movhs.w r11, #0
; CHECK-NEXT:    b .LBB0_3
; CHECK-NEXT:  .LBB0_10: @ %for.cond.cleanup
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, r11, pc}
; CHECK-NEXT:  .LBB0_11: @ %for.cond1.us.preheader
; CHECK-NEXT:    movw r0, :lower16:arr_61
; CHECK-NEXT:    movs r1, #0
; CHECK-NEXT:    movt r0, :upper16:arr_61
; CHECK-NEXT:    str r1, [r0, #84]
; CHECK-NEXT:    .inst.n 0xdefe
entry:
  %tobool.not = icmp eq i8 %var_2, 0
  br i1 %tobool.not, label %for.cond.cleanup, label %for.cond1.preheader

for.cond1.preheader:                              ; preds = %entry
  %cmp11.not = icmp eq [18 x [22 x i8]]* %arr_60, null
  br i1 %cmp11.not, label %for.cond1.us.preheader, label %for.cond1

for.cond1.us.preheader:                           ; preds = %for.cond1.preheader
  store i32 0, i32* getelementptr ([1 x i32], [1 x i32]* @arr_61, i32 21, i32 0), align 4
  call void @llvm.trap()
  unreachable

for.cond.cleanup:                                 ; preds = %entry
  ret i32 undef

for.cond1:                                        ; preds = %for.cond.cleanup9.2, %for.cond1.preheader
  br label %for.cond6.preheader

for.cond6.preheader:                              ; preds = %for.cond.cleanup9.2, %for.cond1
  %conv45 = phi i32 [ 0, %for.cond1 ], [ %conv.2, %for.cond.cleanup9.2 ]
  br label %for.body10

for.cond.cleanup9:                                ; preds = %cond.end22.1
  %add27 = add nuw nsw i32 %conv45, 1
  %conv = and i32 %add27, 255
  br label %for.body10.1

for.body10:                                       ; preds = %cond.end22.1, %for.cond6.preheader
  %i_15.044 = phi i32 [ 0, %for.cond6.preheader ], [ %add.1, %cond.end22.1 ]
  %arraydecay = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %i_15.044, i32 0
  %0 = ptrtoint [22 x i8]* %arraydecay to i32
  %arrayidx13 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %i_15.044
  store i32 %0, i32* %arrayidx13, align 4
  %arrayidx16 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv45, i32 %i_15.044
  %1 = load i8, i8* %arrayidx16, align 1
  %tobool18.not = icmp eq i8 %1, 0
  br i1 %tobool18.not, label %cond.end22, label %cond.true19

cond.true19:                                      ; preds = %for.body10
  %div43 = sdiv i16 27476, %var_15
  %div.sext = sext i16 %div43 to i32
  br label %cond.end22

cond.end22:                                       ; preds = %for.body10, %cond.true19
  %cond23 = phi i32 [ %div.sext, %cond.true19 ], [ 0, %for.body10 ]
  %tobool24 = icmp ne i32 %cond23, 0
  %frombool = zext i1 %tobool24 to i8
  store i8 %frombool, i8* @var_36, align 1
  %add = or i32 %i_15.044, 1
  %arraydecay.1 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %add, i32 0
  %2 = ptrtoint [22 x i8]* %arraydecay.1 to i32
  %arrayidx13.1 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %add
  store i32 %2, i32* %arrayidx13.1, align 4
  %arrayidx16.1 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv45, i32 %add
  %3 = load i8, i8* %arrayidx16.1, align 1
  %tobool18.not.1 = icmp eq i8 %3, 0
  br i1 %tobool18.not.1, label %cond.end22.1, label %cond.true19.1

cond.true19.1:                                    ; preds = %cond.end22
  %div43.1 = sdiv i16 27476, %var_15
  %div.sext.1 = sext i16 %div43.1 to i32
  br label %cond.end22.1

cond.end22.1:                                     ; preds = %cond.true19.1, %cond.end22
  %cond23.1 = phi i32 [ %div.sext.1, %cond.true19.1 ], [ 0, %cond.end22 ]
  %tobool24.1 = icmp ne i32 %cond23.1, 0
  %frombool.1 = zext i1 %tobool24.1 to i8
  store i8 %frombool.1, i8* @var_36, align 1
  %add.1 = add nuw nsw i32 %i_15.044, 2
  %exitcond105.not.1 = icmp eq i32 %add.1, 22
  br i1 %exitcond105.not.1, label %for.cond.cleanup9, label %for.body10

for.body10.1:                                     ; preds = %cond.end22.1.1, %for.cond.cleanup9
  %i_15.044.1 = phi i32 [ 0, %for.cond.cleanup9 ], [ %add.1.1, %cond.end22.1.1 ]
  %arraydecay.1108 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %i_15.044.1, i32 0
  %4 = ptrtoint [22 x i8]* %arraydecay.1108 to i32
  %arrayidx13.1109 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %i_15.044.1
  store i32 %4, i32* %arrayidx13.1109, align 4
  %arrayidx16.1110 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv, i32 %i_15.044.1
  %5 = load i8, i8* %arrayidx16.1110, align 1
  %tobool18.not.1111 = icmp eq i8 %5, 0
  br i1 %tobool18.not.1111, label %cond.end22.1119, label %cond.true19.1114

cond.true19.1114:                                 ; preds = %for.body10.1
  %div43.1112 = sdiv i16 27476, %var_15
  %div.sext.1113 = sext i16 %div43.1112 to i32
  br label %cond.end22.1119

cond.end22.1119:                                  ; preds = %cond.true19.1114, %for.body10.1
  %cond23.1115 = phi i32 [ %div.sext.1113, %cond.true19.1114 ], [ 0, %for.body10.1 ]
  %tobool24.1116 = icmp ne i32 %cond23.1115, 0
  %frombool.1117 = zext i1 %tobool24.1116 to i8
  store i8 %frombool.1117, i8* @var_36, align 1
  %add.1118 = or i32 %i_15.044.1, 1
  %arraydecay.1.1 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %add.1118, i32 0
  %6 = ptrtoint [22 x i8]* %arraydecay.1.1 to i32
  %arrayidx13.1.1 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %add.1118
  store i32 %6, i32* %arrayidx13.1.1, align 4
  %arrayidx16.1.1 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv, i32 %add.1118
  %7 = load i8, i8* %arrayidx16.1.1, align 1
  %tobool18.not.1.1 = icmp eq i8 %7, 0
  br i1 %tobool18.not.1.1, label %cond.end22.1.1, label %cond.true19.1.1

cond.true19.1.1:                                  ; preds = %cond.end22.1119
  %div43.1.1 = sdiv i16 27476, %var_15
  %div.sext.1.1 = sext i16 %div43.1.1 to i32
  br label %cond.end22.1.1

cond.end22.1.1:                                   ; preds = %cond.true19.1.1, %cond.end22.1119
  %cond23.1.1 = phi i32 [ %div.sext.1.1, %cond.true19.1.1 ], [ 0, %cond.end22.1119 ]
  %tobool24.1.1 = icmp ne i32 %cond23.1.1, 0
  %frombool.1.1 = zext i1 %tobool24.1.1 to i8
  store i8 %frombool.1.1, i8* @var_36, align 1
  %add.1.1 = add nuw nsw i32 %i_15.044.1, 2
  %exitcond105.not.1.1 = icmp eq i32 %add.1.1, 22
  br i1 %exitcond105.not.1.1, label %for.cond.cleanup9.1, label %for.body10.1

for.cond.cleanup9.1:                              ; preds = %cond.end22.1.1
  %add27.1 = add nuw nsw i32 %conv45, 2
  %conv.1 = and i32 %add27.1, 255
  br label %for.body10.2

for.body10.2:                                     ; preds = %cond.end22.1.2, %for.cond.cleanup9.1
  %i_15.044.2 = phi i32 [ 0, %for.cond.cleanup9.1 ], [ %add.1.2, %cond.end22.1.2 ]
  %arraydecay.2 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %i_15.044.2, i32 0
  %8 = ptrtoint [22 x i8]* %arraydecay.2 to i32
  %arrayidx13.2 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %i_15.044.2
  store i32 %8, i32* %arrayidx13.2, align 4
  %arrayidx16.2 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv.1, i32 %i_15.044.2
  %9 = load i8, i8* %arrayidx16.2, align 1
  %tobool18.not.2 = icmp eq i8 %9, 0
  br i1 %tobool18.not.2, label %cond.end22.2, label %cond.true19.2

cond.true19.2:                                    ; preds = %for.body10.2
  %div43.2 = sdiv i16 27476, %var_15
  %div.sext.2 = sext i16 %div43.2 to i32
  br label %cond.end22.2

cond.end22.2:                                     ; preds = %cond.true19.2, %for.body10.2
  %cond23.2 = phi i32 [ %div.sext.2, %cond.true19.2 ], [ 0, %for.body10.2 ]
  %tobool24.2 = icmp ne i32 %cond23.2, 0
  %frombool.2 = zext i1 %tobool24.2 to i8
  store i8 %frombool.2, i8* @var_36, align 1
  %add.2 = or i32 %i_15.044.2, 1
  %arraydecay.1.2 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 %add.2, i32 0
  %10 = ptrtoint [22 x i8]* %arraydecay.1.2 to i32
  %arrayidx13.1.2 = getelementptr inbounds [1 x i32], [1 x i32]* @arr_61, i32 0, i32 %add.2
  store i32 %10, i32* %arrayidx13.1.2, align 4
  %arrayidx16.1.2 = getelementptr inbounds [18 x [22 x i8]], [18 x [22 x i8]]* %arr_60, i32 1, i32 %conv.1, i32 %add.2
  %11 = load i8, i8* %arrayidx16.1.2, align 1
  %tobool18.not.1.2 = icmp eq i8 %11, 0
  br i1 %tobool18.not.1.2, label %cond.end22.1.2, label %cond.true19.1.2

cond.true19.1.2:                                  ; preds = %cond.end22.2
  %div43.1.2 = sdiv i16 27476, %var_15
  %div.sext.1.2 = sext i16 %div43.1.2 to i32
  br label %cond.end22.1.2

cond.end22.1.2:                                   ; preds = %cond.true19.1.2, %cond.end22.2
  %cond23.1.2 = phi i32 [ %div.sext.1.2, %cond.true19.1.2 ], [ 0, %cond.end22.2 ]
  %tobool24.1.2 = icmp ne i32 %cond23.1.2, 0
  %frombool.1.2 = zext i1 %tobool24.1.2 to i8
  store i8 %frombool.1.2, i8* @var_36, align 1
  %add.1.2 = add nuw nsw i32 %i_15.044.2, 2
  %exitcond105.not.1.2 = icmp eq i32 %add.1.2, 22
  br i1 %exitcond105.not.1.2, label %for.cond.cleanup9.2, label %for.body10.2

for.cond.cleanup9.2:                              ; preds = %cond.end22.1.2
  %add27.2 = add nuw nsw i32 %conv45, 3
  %conv.2 = and i32 %add27.2, 255
  %cmp.2 = icmp ult i32 %conv.2, 18
  br i1 %cmp.2, label %for.cond6.preheader, label %for.cond1
}

declare void @llvm.trap() #1


@b = hidden local_unnamed_addr global i32 0, align 4
@a = hidden local_unnamed_addr global i32 0, align 4
@c = hidden local_unnamed_addr global [1 x i32] zeroinitializer, align 4

define i32 @d(i64 %e, i32 %f, i64 %g, i32 %h) {
; CHECK-LABEL: d:
; CHECK:       @ %bb.0: @ %entry
; CHECK-NEXT:    .save {r4, r5, r6, r7, r8, r9, r10, r11, lr}
; CHECK-NEXT:    push.w {r4, r5, r6, r7, r8, r9, r10, r11, lr}
; CHECK-NEXT:    .pad #4
; CHECK-NEXT:    sub sp, #4
; CHECK-NEXT:    .vsave {d8, d9, d10, d11, d12, d13}
; CHECK-NEXT:    vpush {d8, d9, d10, d11, d12, d13}
; CHECK-NEXT:    .pad #24
; CHECK-NEXT:    sub sp, #24
; CHECK-NEXT:    mov r12, r1
; CHECK-NEXT:    subs r1, r0, #1
; CHECK-NEXT:    sbcs r1, r12, #0
; CHECK-NEXT:    blt.w .LBB1_28
; CHECK-NEXT:  @ %bb.1: @ %for.cond2.preheader.lr.ph
; CHECK-NEXT:    movs r3, #1
; CHECK-NEXT:    cmp r2, #1
; CHECK-NEXT:    csel r7, r2, r3, lt
; CHECK-NEXT:    mov r10, r2
; CHECK-NEXT:    mov r1, r7
; CHECK-NEXT:    cmp r7, #3
; CHECK-NEXT:    it ls
; CHECK-NEXT:    movls r1, #3
; CHECK-NEXT:    movw r2, #43691
; CHECK-NEXT:    subs r1, r1, r7
; CHECK-NEXT:    movt r2, #43690
; CHECK-NEXT:    adds r1, #2
; CHECK-NEXT:    ldr r4, [sp, #120]
; CHECK-NEXT:    movw r11, :lower16:c
; CHECK-NEXT:    str r7, [sp, #12] @ 4-byte Spill
; CHECK-NEXT:    umull r1, r2, r1, r2
; CHECK-NEXT:    movt r11, :upper16:c
; CHECK-NEXT:    movs r1, #4
; CHECK-NEXT:    @ implicit-def: $r8
; CHECK-NEXT:    @ implicit-def: $r9
; CHECK-NEXT:    movs r5, #12
; CHECK-NEXT:    strd r12, r0, [sp, #4] @ 8-byte Folded Spill
; CHECK-NEXT:    add.w r6, r3, r2, lsr #1
; CHECK-NEXT:    add.w r1, r1, r2, lsr #1
; CHECK-NEXT:    movw r2, #65532
; CHECK-NEXT:    vdup.32 q6, r6
; CHECK-NEXT:    movt r2, #32767
; CHECK-NEXT:    ands r1, r2
; CHECK-NEXT:    str r1, [sp, #16] @ 4-byte Spill
; CHECK-NEXT:    subs r1, #4
; CHECK-NEXT:    add.w r1, r3, r1, lsr #2
; CHECK-NEXT:    str r1, [sp, #20] @ 4-byte Spill
; CHECK-NEXT:    adr r1, .LCPI1_0
; CHECK-NEXT:    vldrw.u32 q0, [r1]
; CHECK-NEXT:    adr r1, .LCPI1_1
; CHECK-NEXT:    vldrw.u32 q5, [r1]
; CHECK-NEXT:    vadd.i32 q4, q0, r7
; CHECK-NEXT:    @ implicit-def: $r7
; CHECK-NEXT:    b .LBB1_4
; CHECK-NEXT:  .LBB1_2: @ %for.body6.preheader
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    mov r0, r9
; CHECK-NEXT:    cmn.w r9, #4
; CHECK-NEXT:    it le
; CHECK-NEXT:    mvnle r0, #3
; CHECK-NEXT:    movw r2, #18725
; CHECK-NEXT:    adds r0, #6
; CHECK-NEXT:    movt r2, #9362
; CHECK-NEXT:    sub.w r1, r0, r9
; CHECK-NEXT:    movs r7, #0
; CHECK-NEXT:    umull r2, r3, r1, r2
; CHECK-NEXT:    subs r2, r1, r3
; CHECK-NEXT:    add.w r2, r3, r2, lsr #1
; CHECK-NEXT:    lsrs r3, r2, #2
; CHECK-NEXT:    lsls r3, r3, #3
; CHECK-NEXT:    sub.w r2, r3, r2, lsr #2
; CHECK-NEXT:    subs r1, r2, r1
; CHECK-NEXT:    add r0, r1
; CHECK-NEXT:    add.w r9, r0, #7
; CHECK-NEXT:    ldrd r12, r0, [sp, #4] @ 8-byte Folded Reload
; CHECK-NEXT:  .LBB1_3: @ %for.cond.cleanup5
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    add.w r8, r8, #2
; CHECK-NEXT:    subs.w r1, r8, r0
; CHECK-NEXT:    asr.w r2, r8, #31
; CHECK-NEXT:    sbcs.w r1, r2, r12
; CHECK-NEXT:    bge.w .LBB1_28
; CHECK-NEXT:  .LBB1_4: @ %for.cond2.preheader
; CHECK-NEXT:    @ =>This Loop Header: Depth=1
; CHECK-NEXT:    @ Child Loop BB1_17 Depth 2
; CHECK-NEXT:    @ Child Loop BB1_8 Depth 2
; CHECK-NEXT:    @ Child Loop BB1_10 Depth 3
; CHECK-NEXT:    @ Child Loop BB1_12 Depth 3
; CHECK-NEXT:    cmp.w r9, #2
; CHECK-NEXT:    bgt .LBB1_3
; CHECK-NEXT:  @ %bb.5: @ %for.body6.lr.ph
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    ldr r0, [sp, #12] @ 4-byte Reload
; CHECK-NEXT:    cmp r0, #5
; CHECK-NEXT:    bhi .LBB1_15
; CHECK-NEXT:  @ %bb.6: @ %for.body6.us.preheader
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    ldrd r2, r3, [sp, #112]
; CHECK-NEXT:    movs r0, #32
; CHECK-NEXT:    movs r1, #0
; CHECK-NEXT:    bl __aeabi_ldivmod
; CHECK-NEXT:    ldrd r12, r0, [sp, #4] @ 8-byte Folded Reload
; CHECK-NEXT:    vdup.32 q0, r2
; CHECK-NEXT:    mov r3, r9
; CHECK-NEXT:    b .LBB1_8
; CHECK-NEXT:  .LBB1_7: @ %for.cond.cleanup17.us
; CHECK-NEXT:    @ in Loop: Header=BB1_8 Depth=2
; CHECK-NEXT:    add.w r9, r3, #7
; CHECK-NEXT:    cmn.w r3, #4
; CHECK-NEXT:    mov.w r7, #0
; CHECK-NEXT:    mov r3, r9
; CHECK-NEXT:    bge .LBB1_3
; CHECK-NEXT:  .LBB1_8: @ %for.body6.us
; CHECK-NEXT:    @ Parent Loop BB1_4 Depth=1
; CHECK-NEXT:    @ => This Loop Header: Depth=2
; CHECK-NEXT:    @ Child Loop BB1_10 Depth 3
; CHECK-NEXT:    @ Child Loop BB1_12 Depth 3
; CHECK-NEXT:    movs r1, #0
; CHECK-NEXT:    cmp.w r10, #0
; CHECK-NEXT:    beq .LBB1_11
; CHECK-NEXT:  @ %bb.9: @ %for.body13.us51.preheader
; CHECK-NEXT:    @ in Loop: Header=BB1_8 Depth=2
; CHECK-NEXT:    movw r2, :lower16:a
; CHECK-NEXT:    vmov q1, q4
; CHECK-NEXT:    movt r2, :upper16:a
; CHECK-NEXT:    str r1, [r2]
; CHECK-NEXT:    movw r2, :lower16:b
; CHECK-NEXT:    movt r2, :upper16:b
; CHECK-NEXT:    str r1, [r2]
; CHECK-NEXT:    ldr r2, [sp, #20] @ 4-byte Reload
; CHECK-NEXT:    dlstp.32 lr, r6
; CHECK-NEXT:  .LBB1_10: @ %vector.body111
; CHECK-NEXT:    @ Parent Loop BB1_4 Depth=1
; CHECK-NEXT:    @ Parent Loop BB1_8 Depth=2
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=3
; CHECK-NEXT:    vshl.i32 q2, q1, #2
; CHECK-NEXT:    vadd.i32 q2, q2, r11
; CHECK-NEXT:    vadd.i32 q1, q1, r5
; CHECK-NEXT:    vstrw.32 q0, [q2]
; CHECK-NEXT:    letp lr, .LBB1_10
; CHECK-NEXT:    b .LBB1_13
; CHECK-NEXT:  .LBB1_11: @ %vector.body.preheader
; CHECK-NEXT:    @ in Loop: Header=BB1_8 Depth=2
; CHECK-NEXT:    ldr r2, [sp, #16] @ 4-byte Reload
; CHECK-NEXT:    vmov q1, q4
; CHECK-NEXT:  .LBB1_12: @ %vector.body
; CHECK-NEXT:    @ Parent Loop BB1_4 Depth=1
; CHECK-NEXT:    @ Parent Loop BB1_8 Depth=2
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=3
; CHECK-NEXT:    vqadd.u32 q2, q5, r1
; CHECK-NEXT:    subs r2, #4
; CHECK-NEXT:    vcmp.u32 hi, q6, q2
; CHECK-NEXT:    vshl.i32 q2, q1, #2
; CHECK-NEXT:    add.w r1, r1, #4
; CHECK-NEXT:    vadd.i32 q2, q2, r11
; CHECK-NEXT:    vadd.i32 q1, q1, r5
; CHECK-NEXT:    vpst
; CHECK-NEXT:    vstrwt.32 q0, [q2]
; CHECK-NEXT:    bne .LBB1_12
; CHECK-NEXT:  .LBB1_13: @ %for.cond9.for.cond15.preheader_crit_edge.us
; CHECK-NEXT:    @ in Loop: Header=BB1_8 Depth=2
; CHECK-NEXT:    cmp r4, #0
; CHECK-NEXT:    beq .LBB1_7
; CHECK-NEXT:  @ %bb.14: @ %for.cond9.for.cond15.preheader_crit_edge.us
; CHECK-NEXT:    @ in Loop: Header=BB1_8 Depth=2
; CHECK-NEXT:    eor r1, r7, #1
; CHECK-NEXT:    lsls r1, r1, #31
; CHECK-NEXT:    bne .LBB1_7
; CHECK-NEXT:    b .LBB1_26
; CHECK-NEXT:  .LBB1_15: @ %for.body6.lr.ph.split
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    cmp r4, #0
; CHECK-NEXT:    beq.w .LBB1_2
; CHECK-NEXT:  @ %bb.16: @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    ldrd r12, r0, [sp, #4] @ 8-byte Folded Reload
; CHECK-NEXT:    mov r2, r9
; CHECK-NEXT:  .LBB1_17: @ %for.body6.us60
; CHECK-NEXT:    @ Parent Loop BB1_4 Depth=1
; CHECK-NEXT:    @ => This Inner Loop Header: Depth=2
; CHECK-NEXT:    lsls r1, r7, #31
; CHECK-NEXT:    bne .LBB1_27
; CHECK-NEXT:  @ %bb.18: @ %for.cond.cleanup17.us63
; CHECK-NEXT:    @ in Loop: Header=BB1_17 Depth=2
; CHECK-NEXT:    cmn.w r2, #4
; CHECK-NEXT:    bge .LBB1_22
; CHECK-NEXT:  @ %bb.19: @ %for.cond.cleanup17.us63.1
; CHECK-NEXT:    @ in Loop: Header=BB1_17 Depth=2
; CHECK-NEXT:    cmn.w r2, #12
; CHECK-NEXT:    bgt .LBB1_23
; CHECK-NEXT:  @ %bb.20: @ %for.cond.cleanup17.us63.2
; CHECK-NEXT:    @ in Loop: Header=BB1_17 Depth=2
; CHECK-NEXT:    cmn.w r2, #19
; CHECK-NEXT:    bgt .LBB1_24
; CHECK-NEXT:  @ %bb.21: @ %for.cond.cleanup17.us63.3
; CHECK-NEXT:    @ in Loop: Header=BB1_17 Depth=2
; CHECK-NEXT:    add.w r9, r2, #28
; CHECK-NEXT:    cmn.w r2, #25
; CHECK-NEXT:    mov.w r7, #0
; CHECK-NEXT:    mov r2, r9
; CHECK-NEXT:    blt .LBB1_17
; CHECK-NEXT:    b .LBB1_3
; CHECK-NEXT:  .LBB1_22: @ %for.cond.cleanup5.loopexit134.split.loop.exit139
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    add.w r9, r2, #7
; CHECK-NEXT:    b .LBB1_25
; CHECK-NEXT:  .LBB1_23: @ %for.cond.cleanup5.loopexit134.split.loop.exit137
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    add.w r9, r2, #14
; CHECK-NEXT:    b .LBB1_25
; CHECK-NEXT:  .LBB1_24: @ %for.cond.cleanup5.loopexit134.split.loop.exit135
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    add.w r9, r2, #21
; CHECK-NEXT:  .LBB1_25: @ %for.cond.cleanup5
; CHECK-NEXT:    @ in Loop: Header=BB1_4 Depth=1
; CHECK-NEXT:    movs r7, #0
; CHECK-NEXT:    b .LBB1_3
; CHECK-NEXT:  .LBB1_26: @ %for.inc19.us
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    b .LBB1_26
; CHECK-NEXT:  .LBB1_27: @ %for.inc19.us66
; CHECK-NEXT:    @ =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    b .LBB1_27
; CHECK-NEXT:  .LBB1_28: @ %for.cond.cleanup
; CHECK-NEXT:    add sp, #24
; CHECK-NEXT:    vpop {d8, d9, d10, d11, d12, d13}
; CHECK-NEXT:    add sp, #4
; CHECK-NEXT:    pop.w {r4, r5, r6, r7, r8, r9, r10, r11, pc}
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  @ %bb.29:
; CHECK-NEXT:  .LCPI1_0:
; CHECK-NEXT:    .long 0 @ 0x0
; CHECK-NEXT:    .long 3 @ 0x3
; CHECK-NEXT:    .long 6 @ 0x6
; CHECK-NEXT:    .long 9 @ 0x9
; CHECK-NEXT:  .LCPI1_1:
; CHECK-NEXT:    .long 0 @ 0x0
; CHECK-NEXT:    .long 1 @ 0x1
; CHECK-NEXT:    .long 2 @ 0x2
; CHECK-NEXT:    .long 3 @ 0x3
entry:
  %cmp47 = icmp sgt i64 %e, 0
  br i1 %cmp47, label %for.cond2.preheader.lr.ph, label %for.cond.cleanup

for.cond2.preheader.lr.ph:                        ; preds = %entry
  %cmp7.inv = icmp slt i32 %f, 1
  %spec.select = select i1 %cmp7.inv, i32 %f, i32 1
  %cmp1041 = icmp ult i32 %spec.select, 6
  %tobool.not = icmp eq i32 %f, 0
  %tobool20.not97 = icmp eq i32 %h, 0
  %0 = icmp ugt i32 %spec.select, 3
  %umax = select i1 %0, i32 %spec.select, i32 3
  %1 = add i32 %umax, 2
  %2 = sub i32 %1, %spec.select
  %3 = udiv i32 %2, 3
  %4 = add nuw nsw i32 %3, 1
  %5 = icmp ugt i32 %spec.select, 3
  %umax112 = select i1 %5, i32 %spec.select, i32 3
  %6 = add i32 %umax112, 2
  %7 = sub i32 %6, %spec.select
  %8 = udiv i32 %7, 3
  %9 = add nuw nsw i32 %8, 1
  %n.rnd.up114 = add nuw nsw i32 %8, 4
  %n.vec116 = and i32 %n.rnd.up114, 2147483644
  %.splatinsert121 = insertelement <4 x i32> poison, i32 %spec.select, i32 0
  %.splat122 = shufflevector <4 x i32> %.splatinsert121, <4 x i32> poison, <4 x i32> zeroinitializer
  %induction123 = add <4 x i32> %.splat122, <i32 0, i32 3, i32 6, i32 9>
  %n.rnd.up = add nuw nsw i32 %3, 4
  %n.vec = and i32 %n.rnd.up, 2147483644
  %.splatinsert = insertelement <4 x i32> poison, i32 %spec.select, i32 0
  %.splat = shufflevector <4 x i32> %.splatinsert, <4 x i32> poison, <4 x i32> zeroinitializer
  %induction = add <4 x i32> %.splat, <i32 0, i32 3, i32 6, i32 9>
  br label %for.cond2.preheader

for.cond2.preheader:                              ; preds = %for.cond2.preheader.lr.ph, %for.cond.cleanup5
  %l.0.off050 = phi i1 [ undef, %for.cond2.preheader.lr.ph ], [ %l.1.off0.lcssa, %for.cond.cleanup5 ]
  %i.049 = phi i32 [ undef, %for.cond2.preheader.lr.ph ], [ %add26, %for.cond.cleanup5 ]
  %j.048 = phi i32 [ undef, %for.cond2.preheader.lr.ph ], [ %j.1.lcssa, %for.cond.cleanup5 ]
  %cmp343 = icmp slt i32 %j.048, 3
  br i1 %cmp343, label %for.body6.lr.ph, label %for.cond.cleanup5

for.body6.lr.ph:                                  ; preds = %for.cond2.preheader
  br i1 %cmp1041, label %for.body6.us.preheader, label %for.body6.lr.ph.split

for.body6.us.preheader:                           ; preds = %for.body6.lr.ph
  %rem.us = srem i64 32, %g
  %conv14.us = trunc i64 %rem.us to i32
  %broadcast.splatinsert131 = insertelement <4 x i32> poison, i32 %conv14.us, i32 0
  %broadcast.splat132 = shufflevector <4 x i32> %broadcast.splatinsert131, <4 x i32> poison, <4 x i32> zeroinitializer
  %broadcast.splatinsert107 = insertelement <4 x i32> poison, i32 %conv14.us, i32 0
  %broadcast.splat108 = shufflevector <4 x i32> %broadcast.splatinsert107, <4 x i32> poison, <4 x i32> zeroinitializer
  br label %for.body6.us

for.body6.us:                                     ; preds = %for.body6.us.preheader, %for.cond.cleanup17.us
  %l.1.off045.us = phi i1 [ false, %for.cond.cleanup17.us ], [ %l.0.off050, %for.body6.us.preheader ]
  %j.144.us = phi i32 [ %add23.us, %for.cond.cleanup17.us ], [ %j.048, %for.body6.us.preheader ]
  br i1 %tobool.not, label %vector.body, label %for.body13.us51.preheader

vector.body:                                      ; preds = %for.body6.us, %vector.body
  %index = phi i32 [ %index.next, %vector.body ], [ 0, %for.body6.us ]
  %vec.ind = phi <4 x i32> [ %vec.ind.next, %vector.body ], [ %induction, %for.body6.us ]
  %active.lane.mask = call <4 x i1> @llvm.get.active.lane.mask.v4i1.i32(i32 %index, i32 %4)
  %10 = getelementptr inbounds [1 x i32], [1 x i32]* @c, i32 0, <4 x i32> %vec.ind
  call void @llvm.masked.scatter.v4i32.v4p0i32(<4 x i32> %broadcast.splat108, <4 x i32*> %10, i32 4, <4 x i1> %active.lane.mask)
  %index.next = add i32 %index, 4
  %vec.ind.next = add <4 x i32> %vec.ind, <i32 12, i32 12, i32 12, i32 12>
  %11 = icmp eq i32 %index.next, %n.vec
  br i1 %11, label %for.cond9.for.cond15.preheader_crit_edge.us, label %vector.body

for.body13.us51.preheader:                        ; preds = %for.body6.us
  store i32 0, i32* @b, align 4
  store i32 0, i32* @a, align 4
  br label %vector.body111

vector.body111:                                   ; preds = %vector.body111, %for.body13.us51.preheader
  %index117 = phi i32 [ 0, %for.body13.us51.preheader ], [ %index.next118, %vector.body111 ]
  %vec.ind124 = phi <4 x i32> [ %induction123, %for.body13.us51.preheader ], [ %vec.ind.next125, %vector.body111 ]
  %active.lane.mask130 = call <4 x i1> @llvm.get.active.lane.mask.v4i1.i32(i32 %index117, i32 %9)
  %12 = getelementptr inbounds [1 x i32], [1 x i32]* @c, i32 0, <4 x i32> %vec.ind124
  call void @llvm.masked.scatter.v4i32.v4p0i32(<4 x i32> %broadcast.splat132, <4 x i32*> %12, i32 4, <4 x i1> %active.lane.mask130)
  %index.next118 = add i32 %index117, 4
  %vec.ind.next125 = add <4 x i32> %vec.ind124, <i32 12, i32 12, i32 12, i32 12>
  %13 = icmp eq i32 %index.next118, %n.vec116
  br i1 %13, label %for.cond9.for.cond15.preheader_crit_edge.us, label %vector.body111

for.cond.cleanup17.us:                            ; preds = %for.cond9.for.cond15.preheader_crit_edge.us
  %add23.us = add nsw i32 %j.144.us, 7
  %cmp3.us = icmp slt i32 %j.144.us, -4
  br i1 %cmp3.us, label %for.body6.us, label %for.cond.cleanup5

for.inc19.us:                                     ; preds = %for.cond9.for.cond15.preheader_crit_edge.us, %for.inc19.us
  br label %for.inc19.us

for.cond9.for.cond15.preheader_crit_edge.us:      ; preds = %vector.body111, %vector.body
  %l.1.off045.us.not = xor i1 %l.1.off045.us, true
  %brmerge = or i1 %tobool20.not97, %l.1.off045.us.not
  br i1 %brmerge, label %for.cond.cleanup17.us, label %for.inc19.us

for.body6.lr.ph.split:                            ; preds = %for.body6.lr.ph
  br i1 %tobool20.not97, label %for.body6.preheader, label %for.body6.us60

for.body6.preheader:                              ; preds = %for.body6.lr.ph.split
  %14 = icmp sgt i32 %j.048, -4
  %smax = select i1 %14, i32 %j.048, i32 -4
  %15 = add nsw i32 %smax, 6
  %16 = sub i32 %15, %j.048
  %17 = urem i32 %16, 7
  %18 = sub i32 %16, %17
  %19 = add nsw i32 %j.048, 7
  %20 = add i32 %19, %18
  br label %for.cond.cleanup5

for.body6.us60:                                   ; preds = %for.body6.lr.ph.split, %for.cond.cleanup17.us63.3
  %l.1.off045.us61 = phi i1 [ false, %for.cond.cleanup17.us63.3 ], [ %l.0.off050, %for.body6.lr.ph.split ]
  %j.144.us62 = phi i32 [ %add23.us64.3, %for.cond.cleanup17.us63.3 ], [ %j.048, %for.body6.lr.ph.split ]
  br i1 %l.1.off045.us61, label %for.inc19.us66, label %for.cond.cleanup17.us63

for.cond.cleanup17.us63:                          ; preds = %for.body6.us60
  %cmp3.us65 = icmp slt i32 %j.144.us62, -4
  br i1 %cmp3.us65, label %for.cond.cleanup17.us63.1, label %for.cond.cleanup5.loopexit134.split.loop.exit139

for.inc19.us66:                                   ; preds = %for.body6.us60, %for.inc19.us66
  br label %for.inc19.us66

for.cond.cleanup:                                 ; preds = %for.cond.cleanup5, %entry
  ret i32 undef

for.cond.cleanup5.loopexit134.split.loop.exit135: ; preds = %for.cond.cleanup17.us63.2
  %add23.us64.2.le = add nsw i32 %j.144.us62, 21
  br label %for.cond.cleanup5

for.cond.cleanup5.loopexit134.split.loop.exit137: ; preds = %for.cond.cleanup17.us63.1
  %add23.us64.1.le = add nsw i32 %j.144.us62, 14
  br label %for.cond.cleanup5

for.cond.cleanup5.loopexit134.split.loop.exit139: ; preds = %for.cond.cleanup17.us63
  %add23.us64.le = add nsw i32 %j.144.us62, 7
  br label %for.cond.cleanup5

for.cond.cleanup5:                                ; preds = %for.cond.cleanup5.loopexit134.split.loop.exit135, %for.cond.cleanup5.loopexit134.split.loop.exit137, %for.cond.cleanup5.loopexit134.split.loop.exit139, %for.cond.cleanup17.us63.3, %for.cond.cleanup17.us, %for.body6.preheader, %for.cond2.preheader
  %j.1.lcssa = phi i32 [ %j.048, %for.cond2.preheader ], [ %20, %for.body6.preheader ], [ %add23.us, %for.cond.cleanup17.us ], [ %add23.us64.2.le, %for.cond.cleanup5.loopexit134.split.loop.exit135 ], [ %add23.us64.1.le, %for.cond.cleanup5.loopexit134.split.loop.exit137 ], [ %add23.us64.le, %for.cond.cleanup5.loopexit134.split.loop.exit139 ], [ %add23.us64.3, %for.cond.cleanup17.us63.3 ]
  %l.1.off0.lcssa = phi i1 [ %l.0.off050, %for.cond2.preheader ], [ false, %for.body6.preheader ], [ false, %for.cond.cleanup17.us ], [ false, %for.cond.cleanup17.us63.3 ], [ false, %for.cond.cleanup5.loopexit134.split.loop.exit139 ], [ false, %for.cond.cleanup5.loopexit134.split.loop.exit137 ], [ false, %for.cond.cleanup5.loopexit134.split.loop.exit135 ]
  %add26 = add nsw i32 %i.049, 2
  %conv = sext i32 %add26 to i64
  %cmp = icmp slt i64 %conv, %e
  br i1 %cmp, label %for.cond2.preheader, label %for.cond.cleanup

for.cond.cleanup17.us63.1:                        ; preds = %for.cond.cleanup17.us63
  %cmp3.us65.1 = icmp slt i32 %j.144.us62, -11
  br i1 %cmp3.us65.1, label %for.cond.cleanup17.us63.2, label %for.cond.cleanup5.loopexit134.split.loop.exit137

for.cond.cleanup17.us63.2:                        ; preds = %for.cond.cleanup17.us63.1
  %cmp3.us65.2 = icmp slt i32 %j.144.us62, -18
  br i1 %cmp3.us65.2, label %for.cond.cleanup17.us63.3, label %for.cond.cleanup5.loopexit134.split.loop.exit135

for.cond.cleanup17.us63.3:                        ; preds = %for.cond.cleanup17.us63.2
  %add23.us64.3 = add nsw i32 %j.144.us62, 28
  %cmp3.us65.3 = icmp slt i32 %j.144.us62, -25
  br i1 %cmp3.us65.3, label %for.body6.us60, label %for.cond.cleanup5
}

declare <4 x i1> @llvm.get.active.lane.mask.v4i1.i32(i32, i32) #1
declare void @llvm.masked.scatter.v4i32.v4p0i32(<4 x i32>, <4 x i32*>, i32 immarg, <4 x i1>) #2
