; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -loop-reroll -S | FileCheck %s
target datalayout = "e-p:32:32:32-i1:8:32-i8:8:32-i16:16:32-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:32:64-v128:32:128-a0:0:32-n32-S32"
target triple = "thumbv7-none-linux"

;void foo(int *A, int *B, int m, int n) {
;  for (int i = m; i < n; i+=4) {
;    A[i+0] = B[i+0] * 4;
;    A[i+1] = B[i+1] * 4;
;    A[i+2] = B[i+2] * 4;
;    A[i+3] = B[i+3] * 4;
;  }
;}
define void @foo(i32* nocapture %A, i32* nocapture readonly %B, i32 %m, i32 %n) {
; CHECK-LABEL: @foo(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP34:%.*]] = icmp slt i32 [[M:%.*]], [[N:%.*]]
; CHECK-NEXT:    br i1 [[CMP34]], label [[FOR_BODY_PREHEADER:%.*]], label [[FOR_END:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[TMP0:%.*]] = add i32 [[M]], 4
; CHECK-NEXT:    [[SMAX:%.*]] = call i32 @llvm.smax.i32(i32 [[N]], i32 [[TMP0]])
; CHECK-NEXT:    [[TMP1:%.*]] = add i32 [[SMAX]], -1
; CHECK-NEXT:    [[TMP2:%.*]] = sub i32 [[TMP1]], [[M]]
; CHECK-NEXT:    [[TMP3:%.*]] = lshr i32 [[TMP2]], 2
; CHECK-NEXT:    [[TMP4:%.*]] = shl nuw i32 [[TMP3]], 2
; CHECK-NEXT:    [[TMP5:%.*]] = add nuw nsw i32 [[TMP4]], 3
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVAR:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVAR_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[TMP6:%.*]] = add i32 [[M]], [[INDVAR]]
; CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds i32, i32* [[B:%.*]], i32 [[TMP6]]
; CHECK-NEXT:    [[TMP7:%.*]] = load i32, i32* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[MUL:%.*]] = shl nsw i32 [[TMP7]], 2
; CHECK-NEXT:    [[ARRAYIDX2:%.*]] = getelementptr inbounds i32, i32* [[A:%.*]], i32 [[TMP6]]
; CHECK-NEXT:    store i32 [[MUL]], i32* [[ARRAYIDX2]], align 4
; CHECK-NEXT:    [[INDVAR_NEXT]] = add i32 [[INDVAR]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp eq i32 [[INDVAR]], [[TMP5]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_END_LOOPEXIT:%.*]], label [[FOR_BODY]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp34 = icmp slt i32 %m, %n
  br i1 %cmp34, label %for.body, label %for.end

for.body:                                         ; preds = %entry, %for.body
  %i.035 = phi i32 [ %add18, %for.body ], [ %m, %entry ]
  %arrayidx = getelementptr inbounds i32, i32* %B, i32 %i.035
  %0 = load i32, i32* %arrayidx, align 4
  %mul = shl nsw i32 %0, 2
  %arrayidx2 = getelementptr inbounds i32, i32* %A, i32 %i.035
  store i32 %mul, i32* %arrayidx2, align 4
  %add3 = add nsw i32 %i.035, 1
  %arrayidx4 = getelementptr inbounds i32, i32* %B, i32 %add3
  %1 = load i32, i32* %arrayidx4, align 4
  %mul5 = shl nsw i32 %1, 2
  %arrayidx7 = getelementptr inbounds i32, i32* %A, i32 %add3
  store i32 %mul5, i32* %arrayidx7, align 4
  %add8 = add nsw i32 %i.035, 2
  %arrayidx9 = getelementptr inbounds i32, i32* %B, i32 %add8
  %2 = load i32, i32* %arrayidx9, align 4
  %mul10 = shl nsw i32 %2, 2
  %arrayidx12 = getelementptr inbounds i32, i32* %A, i32 %add8
  store i32 %mul10, i32* %arrayidx12, align 4
  %add13 = add nsw i32 %i.035, 3
  %arrayidx14 = getelementptr inbounds i32, i32* %B, i32 %add13
  %3 = load i32, i32* %arrayidx14, align 4
  %mul15 = shl nsw i32 %3, 2
  %arrayidx17 = getelementptr inbounds i32, i32* %A, i32 %add13
  store i32 %mul15, i32* %arrayidx17, align 4
  %add18 = add nsw i32 %i.035, 4
  %cmp = icmp slt i32 %add18, %n
  br i1 %cmp, label %for.body, label %for.end

for.end:                                          ; preds = %for.body, %entry
  ret void
}

;void daxpy_ur(int n,float da,float *dx,float *dy)
;    {
;    int m = n % 4;
;    for (int i = m; i < n; i = i + 4)
;        {
;        dy[i]   = dy[i]   + da*dx[i];
;        dy[i+1] = dy[i+1] + da*dx[i+1];
;        dy[i+2] = dy[i+2] + da*dx[i+2];
;        dy[i+3] = dy[i+3] + da*dx[i+3];
;        }
;    }
define void @daxpy_ur(i32 %n, float %da, float* nocapture readonly %dx, float* nocapture %dy) {
; CHECK-LABEL: @daxpy_ur(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[REM:%.*]] = srem i32 [[N:%.*]], 4
; CHECK-NEXT:    [[CMP55:%.*]] = icmp slt i32 [[REM]], [[N]]
; CHECK-NEXT:    br i1 [[CMP55]], label [[FOR_BODY_PREHEADER:%.*]], label [[FOR_END:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[TMP0:%.*]] = add i32 [[N]], -1
; CHECK-NEXT:    [[TMP1:%.*]] = sub i32 [[TMP0]], [[REM]]
; CHECK-NEXT:    [[TMP2:%.*]] = lshr i32 [[TMP1]], 2
; CHECK-NEXT:    [[TMP3:%.*]] = shl nuw i32 [[TMP2]], 2
; CHECK-NEXT:    [[TMP4:%.*]] = add nuw nsw i32 [[TMP3]], 3
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVAR:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVAR_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[TMP5:%.*]] = add i32 [[REM]], [[INDVAR]]
; CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds float, float* [[DY:%.*]], i32 [[TMP5]]
; CHECK-NEXT:    [[TMP6:%.*]] = load float, float* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[ARRAYIDX1:%.*]] = getelementptr inbounds float, float* [[DX:%.*]], i32 [[TMP5]]
; CHECK-NEXT:    [[TMP7:%.*]] = load float, float* [[ARRAYIDX1]], align 4
; CHECK-NEXT:    [[MUL:%.*]] = fmul float [[TMP7]], [[DA:%.*]]
; CHECK-NEXT:    [[ADD:%.*]] = fadd float [[TMP6]], [[MUL]]
; CHECK-NEXT:    store float [[ADD]], float* [[ARRAYIDX]], align 4
; CHECK-NEXT:    [[INDVAR_NEXT]] = add i32 [[INDVAR]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp eq i32 [[INDVAR]], [[TMP4]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_END_LOOPEXIT:%.*]], label [[FOR_BODY]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %rem = srem i32 %n, 4
  %cmp55 = icmp slt i32 %rem, %n
  br i1 %cmp55, label %for.body, label %for.end

for.body:                                         ; preds = %entry, %for.body
  %i.056 = phi i32 [ %add27, %for.body ], [ %rem, %entry ]
  %arrayidx = getelementptr inbounds float, float* %dy, i32 %i.056
  %0 = load float, float* %arrayidx, align 4
  %arrayidx1 = getelementptr inbounds float, float* %dx, i32 %i.056
  %1 = load float, float* %arrayidx1, align 4
  %mul = fmul float %1, %da
  %add = fadd float %0, %mul
  store float %add, float* %arrayidx, align 4
  %add3 = add nsw i32 %i.056, 1
  %arrayidx4 = getelementptr inbounds float, float* %dy, i32 %add3
  %2 = load float, float* %arrayidx4, align 4
  %arrayidx6 = getelementptr inbounds float, float* %dx, i32 %add3
  %3 = load float, float* %arrayidx6, align 4
  %mul7 = fmul float %3, %da
  %add8 = fadd float %2, %mul7
  store float %add8, float* %arrayidx4, align 4
  %add11 = add nsw i32 %i.056, 2
  %arrayidx12 = getelementptr inbounds float, float* %dy, i32 %add11
  %4 = load float, float* %arrayidx12, align 4
  %arrayidx14 = getelementptr inbounds float, float* %dx, i32 %add11
  %5 = load float, float* %arrayidx14, align 4
  %mul15 = fmul float %5, %da
  %add16 = fadd float %4, %mul15
  store float %add16, float* %arrayidx12, align 4
  %add19 = add nsw i32 %i.056, 3
  %arrayidx20 = getelementptr inbounds float, float* %dy, i32 %add19
  %6 = load float, float* %arrayidx20, align 4
  %arrayidx22 = getelementptr inbounds float, float* %dx, i32 %add19
  %7 = load float, float* %arrayidx22, align 4
  %mul23 = fmul float %7, %da
  %add24 = fadd float %6, %mul23
  store float %add24, float* %arrayidx20, align 4
  %add27 = add nsw i32 %i.056, 4
  %cmp = icmp slt i32 %add27, %n
  br i1 %cmp, label %for.body, label %for.end

for.end:                                          ; preds = %for.body, %entry
  ret void
}

