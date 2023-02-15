; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -basic-aa -loop-idiom < %s -S | FileCheck %s
target datalayout = "e-p:40:64:64:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"

%struct.foo = type { i32, i32 }
%struct.foo1 = type { i32, i32, i32 }
%struct.foo2 = type { i32, i16, i16 }

;void bar1(foo_t *f, unsigned n) {
;  for (unsigned i = 0; i < n; ++i) {
;    f[i].a = 0;
;    f[i].b = 0;
;  }
;}
define void @bar1(%struct.foo* %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[F1:%.*]] = bitcast %struct.foo* [[F:%.*]] to i8*
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[TMP0:%.*]] = shl nuw i32 [[N]], 3
; CHECK-NEXT:    call void @llvm.memset.p0i8.i32(i8* align 4 [[F1]], i8 0, i32 [[TMP0]], i1 false)
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO:%.*]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nuw nsw i32 [[INDVARS_IV]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp ne i32 [[INDVARS_IV_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_BODY]], label [[FOR_END_LOOPEXIT:%.*]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %a = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %b = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 1
  store i32 0, i32* %b, align 4
  %indvars.iv.next = add nuw nsw i32 %indvars.iv, 1
  %exitcond = icmp ne i32 %indvars.iv.next, %n
  br i1 %exitcond, label %for.body, label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}

;void bar2(foo_t *f, unsigned n) {
;  for (unsigned i = 0; i < n; ++i) {
;    f[i].b = 0;
;    f[i].a = 0;
;  }
;}
define void @bar2(%struct.foo* %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar2(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[F1:%.*]] = bitcast %struct.foo* [[F:%.*]] to i8*
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[TMP0:%.*]] = shl nuw i32 [[N]], 3
; CHECK-NEXT:    call void @llvm.memset.p0i8.i32(i8* align 4 [[F1]], i8 0, i32 [[TMP0]], i1 false)
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO:%.*]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nuw nsw i32 [[INDVARS_IV]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp ne i32 [[INDVARS_IV_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_BODY]], label [[FOR_END_LOOPEXIT:%.*]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %b = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 1
  store i32 0, i32* %b, align 4
  %a = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %indvars.iv.next = add nuw nsw i32 %indvars.iv, 1
  %exitcond = icmp ne i32 %indvars.iv.next, %n
  br i1 %exitcond, label %for.body, label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}

;void bar3(foo_t *f, unsigned n) {
;  for (unsigned i = n; i > 0; --i) {
;    f[i].a = 0;
;    f[i].b = 0;
;  }
;}
define void @bar3(%struct.foo* nocapture %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar3(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[SCEVGEP:%.*]] = getelementptr [[STRUCT_FOO:%.*]], %struct.foo* [[F:%.*]], i32 1
; CHECK-NEXT:    [[SCEVGEP1:%.*]] = bitcast %struct.foo* [[SCEVGEP]] to i8*
; CHECK-NEXT:    [[TMP0:%.*]] = shl nuw i32 [[N]], 3
; CHECK-NEXT:    call void @llvm.memset.p0i8.i32(i8* align 4 [[SCEVGEP1]], i8 0, i32 [[TMP0]], i1 false)
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ [[N]], [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    [[DEC:%.*]] = add i32 [[INDVARS_IV]], -1
; CHECK-NEXT:    [[CMP:%.*]] = icmp eq i32 [[DEC]], 0
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nsw i32 [[INDVARS_IV]], -1
; CHECK-NEXT:    br i1 [[CMP]], label [[FOR_END_LOOPEXIT:%.*]], label [[FOR_BODY]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ %n, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %a = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %b = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 1
  store i32 0, i32* %b, align 4
  %dec = add i32 %indvars.iv, -1
  %cmp = icmp eq i32 %dec, 0
  %indvars.iv.next = add nsw i32 %indvars.iv, -1
  br i1 %cmp, label %for.end.loopexit, label %for.body

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}

;void bar4(foo_t *f, unsigned n) {
;  for (unsigned i = 0; i < n; ++i) {
;    f[i].a = 0;
;    f[i].b = 1;
;  }
;}
define void @bar4(%struct.foo* nocapture %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar4(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO:%.*]], %struct.foo* [[F:%.*]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    store i32 0, i32* [[A]], align 4
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO]], %struct.foo* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    store i32 1, i32* [[B]], align 4
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nuw nsw i32 [[INDVARS_IV]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp ne i32 [[INDVARS_IV_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_BODY]], label [[FOR_END_LOOPEXIT:%.*]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %a = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %b = getelementptr inbounds %struct.foo, %struct.foo* %f, i32 %indvars.iv, i32 1
  store i32 1, i32* %b, align 4
  %indvars.iv.next = add nuw nsw i32 %indvars.iv, 1
  %exitcond = icmp ne i32 %indvars.iv.next, %n
  br i1 %exitcond, label %for.body, label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}

;void bar5(foo1_t *f, unsigned n) {
;  for (unsigned i = 0; i < n; ++i) {
;    f[i].a = 0;
;    f[i].b = 0;
;  }
;}
define void @bar5(%struct.foo1* nocapture %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar5(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO1:%.*]], %struct.foo1* [[F:%.*]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    store i32 0, i32* [[A]], align 4
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO1]], %struct.foo1* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    store i32 0, i32* [[B]], align 4
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nuw nsw i32 [[INDVARS_IV]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp ne i32 [[INDVARS_IV_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_BODY]], label [[FOR_END_LOOPEXIT:%.*]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %a = getelementptr inbounds %struct.foo1, %struct.foo1* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %b = getelementptr inbounds %struct.foo1, %struct.foo1* %f, i32 %indvars.iv, i32 1
  store i32 0, i32* %b, align 4
  %indvars.iv.next = add nuw nsw i32 %indvars.iv, 1
  %exitcond = icmp ne i32 %indvars.iv.next, %n
  br i1 %exitcond, label %for.body, label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}

;void bar6(foo2_t *f, unsigned n) {
;  for (unsigned i = 0; i < n; ++i) {
;    f[i].a = 0;
;    f[i].b = 0;
;    f[i].c = 0;
;  }
;}
define void @bar6(%struct.foo2* nocapture %f, i32 %n) nounwind ssp {
; CHECK-LABEL: @bar6(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[F1:%.*]] = bitcast %struct.foo2* [[F:%.*]] to i8*
; CHECK-NEXT:    [[CMP1:%.*]] = icmp eq i32 [[N:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1]], label [[FOR_END:%.*]], label [[FOR_BODY_PREHEADER:%.*]]
; CHECK:       for.body.preheader:
; CHECK-NEXT:    [[TMP0:%.*]] = shl nuw i32 [[N]], 3
; CHECK-NEXT:    call void @llvm.memset.p0i8.i32(i8* align 4 [[F1]], i8 0, i32 [[TMP0]], i1 false)
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[INDVARS_IV:%.*]] = phi i32 [ 0, [[FOR_BODY_PREHEADER]] ], [ [[INDVARS_IV_NEXT:%.*]], [[FOR_BODY]] ]
; CHECK-NEXT:    [[A:%.*]] = getelementptr inbounds [[STRUCT_FOO2:%.*]], %struct.foo2* [[F]], i32 [[INDVARS_IV]], i32 0
; CHECK-NEXT:    [[B:%.*]] = getelementptr inbounds [[STRUCT_FOO2]], %struct.foo2* [[F]], i32 [[INDVARS_IV]], i32 1
; CHECK-NEXT:    [[C:%.*]] = getelementptr inbounds [[STRUCT_FOO2]], %struct.foo2* [[F]], i32 [[INDVARS_IV]], i32 2
; CHECK-NEXT:    [[INDVARS_IV_NEXT]] = add nuw nsw i32 [[INDVARS_IV]], 1
; CHECK-NEXT:    [[EXITCOND:%.*]] = icmp ne i32 [[INDVARS_IV_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[EXITCOND]], label [[FOR_BODY]], label [[FOR_END_LOOPEXIT:%.*]]
; CHECK:       for.end.loopexit:
; CHECK-NEXT:    br label [[FOR_END]]
; CHECK:       for.end:
; CHECK-NEXT:    ret void
;
entry:
  %cmp1 = icmp eq i32 %n, 0
  br i1 %cmp1, label %for.end, label %for.body.preheader

for.body.preheader:                               ; preds = %entry
  br label %for.body

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i32 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %a = getelementptr inbounds %struct.foo2, %struct.foo2* %f, i32 %indvars.iv, i32 0
  store i32 0, i32* %a, align 4
  %b = getelementptr inbounds %struct.foo2, %struct.foo2* %f, i32 %indvars.iv, i32 1
  store i16 0, i16* %b, align 4
  %c = getelementptr inbounds %struct.foo2, %struct.foo2* %f, i32 %indvars.iv, i32 2
  store i16 0, i16* %c, align 2
  %indvars.iv.next = add nuw nsw i32 %indvars.iv, 1
  %exitcond = icmp ne i32 %indvars.iv.next, %n
  br i1 %exitcond, label %for.body, label %for.end.loopexit

for.end.loopexit:                                 ; preds = %for.body
  br label %for.end

for.end:                                          ; preds = %for.end.loopexit, %entry
  ret void
}
