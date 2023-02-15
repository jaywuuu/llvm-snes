; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -loop-interchange -cache-line-size=64 -verify-loop-lcssa -verify-dom-info -S %s | FileCheck %s

@b = external dso_local global [5 x i32], align 16

define void @test1() {
; CHECK-LABEL: @test1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br label [[FOR_BODY2_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INC41:%.*]] = phi i32 [ [[INC4:%.*]], [[FOR_INC3:%.*]] ], [ undef, [[FOR_BODY_PREHEADER:%.*]] ]
; CHECK-NEXT:    [[IDXPROM:%.*]] = sext i32 [[INC41]] to i64
; CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds [5 x i32], [5 x i32]* @b, i64 0, i64 [[IDXPROM]]
; CHECK-NEXT:    br label [[FOR_BODY2_SPLIT:%.*]]
; CHECK:       for.body2.preheader:
; CHECK-NEXT:    br label [[FOR_BODY2:%.*]]
; CHECK:       for.body2:
; CHECK-NEXT:    [[LSR_IV:%.*]] = phi i32 [ [[TMP1:%.*]], [[FOR_INC_SPLIT:%.*]] ], [ 1, [[FOR_BODY2_PREHEADER]] ]
; CHECK-NEXT:    br label [[FOR_BODY_PREHEADER]]
; CHECK:       for.body2.split:
; CHECK-NEXT:    br label [[FOR_INC:%.*]]
; CHECK:       for.inc:
; CHECK-NEXT:    [[TMP0:%.*]] = load i32, i32* [[ARRAYIDX]], align 4
; CHECK-NEXT:    store i32 undef, i32* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[CMP:%.*]] = icmp slt i32 [[LSR_IV]], 4
; CHECK-NEXT:    [[LSR_IV_NEXT:%.*]] = add nuw nsw i32 [[LSR_IV]], 1
; CHECK-NEXT:    br label [[FOR_COND1_FOR_END_CRIT_EDGE:%.*]]
; CHECK:       for.inc.split:
; CHECK-NEXT:    [[TMP1]] = add nuw nsw i32 [[LSR_IV]], 1
; CHECK-NEXT:    [[TMP2:%.*]] = icmp slt i32 [[LSR_IV]], 4
; CHECK-NEXT:    br i1 [[TMP2]], label [[FOR_BODY2]], label [[FOR_COND_FOR_END5_CRIT_EDGE:%.*]]
; CHECK:       for.cond1.for.end_crit_edge:
; CHECK-NEXT:    br label [[FOR_INC3]]
; CHECK:       for.inc3:
; CHECK-NEXT:    [[INC4]] = add nsw i32 [[INC41]], 1
; CHECK-NEXT:    br i1 false, label [[FOR_BODY]], label [[FOR_INC_SPLIT]]
; CHECK:       for.cond.for.end5_crit_edge:
; CHECK-NEXT:    ret void
;
entry:
  br label %for.body

for.body:                                         ; preds = %for.inc3, %entry
  %inc41 = phi i32 [ %inc4, %for.inc3 ], [ undef, %entry ]
  br label %for.body2

for.body2:                                        ; preds = %for.inc, %for.body
  %lsr.iv = phi i32 [ %lsr.iv.next, %for.inc ], [ 1, %for.body ]
  br label %for.inc

for.inc:                                          ; preds = %for.body2
  %idxprom = sext i32 %inc41 to i64
  %arrayidx = getelementptr inbounds [5 x i32], [5 x i32]* @b, i64 0, i64 %idxprom
  %0 = load i32, i32* %arrayidx, align 4
  store i32 undef, i32* %arrayidx, align 4
  %cmp = icmp slt i32 %lsr.iv, 4
  %lsr.iv.next = add nuw nsw i32 %lsr.iv, 1
  br i1 %cmp, label %for.body2, label %for.cond1.for.end_crit_edge

for.cond1.for.end_crit_edge:                      ; preds = %for.inc
  br label %for.inc3

for.inc3:                                         ; preds = %for.cond1.for.end_crit_edge
  %inc4 = add nsw i32 %inc41, 1
  br i1 undef, label %for.body, label %for.cond.for.end5_crit_edge

for.cond.for.end5_crit_edge:                      ; preds = %for.inc3
  ret void
}

define void @test2() {
; CHECK-LABEL: @test2(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br label [[FOR_BODY2_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INC41:%.*]] = phi i32 [ [[INC4:%.*]], [[FOR_INC3:%.*]] ], [ undef, [[FOR_BODY_PREHEADER:%.*]] ]
; CHECK-NEXT:    [[IDXPROM:%.*]] = sext i32 [[INC41]] to i64
; CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds [5 x i32], [5 x i32]* @b, i64 0, i64 [[IDXPROM]]
; CHECK-NEXT:    br label [[FOR_BODY2_SPLIT:%.*]]
; CHECK:       for.body2.preheader:
; CHECK-NEXT:    br label [[FOR_BODY2:%.*]]
; CHECK:       for.body2:
; CHECK-NEXT:    [[LSR_IV:%.*]] = phi i32 [ [[TMP1:%.*]], [[FOR_INC_SPLIT:%.*]] ], [ 1, [[FOR_BODY2_PREHEADER]] ]
; CHECK-NEXT:    br label [[FOR_BODY_PREHEADER]]
; CHECK:       for.body2.split:
; CHECK-NEXT:    br label [[FOR_INC:%.*]]
; CHECK:       for.inc:
; CHECK-NEXT:    [[TMP0:%.*]] = load i32, i32* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[CMP:%.*]] = icmp slt i32 [[LSR_IV]], 4
; CHECK-NEXT:    [[CMP_ZEXT:%.*]] = zext i1 [[CMP]] to i32
; CHECK-NEXT:    store i32 [[CMP_ZEXT]], i32* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[LSR_IV_NEXT:%.*]] = add nuw nsw i32 [[LSR_IV]], 1
; CHECK-NEXT:    br label [[FOR_COND1_FOR_END_CRIT_EDGE:%.*]]
; CHECK:       for.inc.split:
; CHECK-NEXT:    [[TMP1]] = add nuw nsw i32 [[LSR_IV]], 1
; CHECK-NEXT:    [[TMP2:%.*]] = icmp slt i32 [[LSR_IV]], 4
; CHECK-NEXT:    br i1 [[TMP2]], label [[FOR_BODY2]], label [[FOR_COND_FOR_END5_CRIT_EDGE:%.*]]
; CHECK:       for.cond1.for.end_crit_edge:
; CHECK-NEXT:    br label [[FOR_INC3]]
; CHECK:       for.inc3:
; CHECK-NEXT:    [[INC4]] = add nsw i32 [[INC41]], 1
; CHECK-NEXT:    br i1 false, label [[FOR_BODY]], label [[FOR_INC_SPLIT]]
; CHECK:       for.cond.for.end5_crit_edge:
; CHECK-NEXT:    ret void
;
entry:
  br label %for.body

for.body:                                         ; preds = %for.inc3, %entry
  %inc41 = phi i32 [ %inc4, %for.inc3 ], [ undef, %entry ]
  br label %for.body2

for.body2:                                        ; preds = %for.inc, %for.body
  %lsr.iv = phi i32 [ %lsr.iv.next, %for.inc ], [ 1, %for.body ]
  br label %for.inc

for.inc:                                          ; preds = %for.body2
  %idxprom = sext i32 %inc41 to i64
  %arrayidx = getelementptr inbounds [5 x i32], [5 x i32]* @b, i64 0, i64 %idxprom
  %0 = load i32, i32* %arrayidx, align 4
  %cmp = icmp slt i32 %lsr.iv, 4
  %cmp.zext = zext i1 %cmp to i32
  store i32 %cmp.zext, i32* %arrayidx, align 4
  %lsr.iv.next = add nuw nsw i32 %lsr.iv, 1
  br i1 %cmp, label %for.body2, label %for.cond1.for.end_crit_edge

for.cond1.for.end_crit_edge:                      ; preds = %for.inc
  br label %for.inc3

for.inc3:                                         ; preds = %for.cond1.for.end_crit_edge
  %inc4 = add nsw i32 %inc41, 1
  br i1 undef, label %for.body, label %for.cond.for.end5_crit_edge

for.cond.for.end5_crit_edge:                      ; preds = %for.inc3
  ret void
}
