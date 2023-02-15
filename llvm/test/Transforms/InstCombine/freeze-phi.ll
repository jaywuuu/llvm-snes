; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -passes=instcombine -S | FileCheck %s

define i32 @const(i1 %cond) {
; CHECK-LABEL: @const(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[A:%.*]], label [[B:%.*]]
; CHECK:       A:
; CHECK-NEXT:    br label [[C:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[C]]
; CHECK:       C:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ 0, [[A]] ], [ 1, [[B]] ]
; CHECK-NEXT:    ret i32 [[Y]]
;
  br i1 %cond, label %A, label %B
A:
  br label %C
B:
  br label %C
C:
  %y = phi i32 [0, %A], [1, %B]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}

define <2 x i32> @vec(i1 %cond) {
; CHECK-LABEL: @vec(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[A:%.*]], label [[B:%.*]]
; CHECK:       A:
; CHECK-NEXT:    br label [[C:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[C]]
; CHECK:       C:
; CHECK-NEXT:    [[Y:%.*]] = phi <2 x i32> [ <i32 0, i32 1>, [[A]] ], [ <i32 2, i32 3>, [[B]] ]
; CHECK-NEXT:    ret <2 x i32> [[Y]]
;
  br i1 %cond, label %A, label %B
A:
  br label %C
B:
  br label %C
C:
  %y = phi <2 x i32> [<i32 0, i32 1>, %A], [<i32 2, i32 3>, %B]
  %y.fr = freeze <2 x i32> %y
  ret <2 x i32> %y.fr
}

define <2 x i32> @vec_undef(i1 %cond) {
; CHECK-LABEL: @vec_undef(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[A:%.*]], label [[B:%.*]]
; CHECK:       A:
; CHECK-NEXT:    br label [[C:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[C]]
; CHECK:       C:
; CHECK-NEXT:    [[Y:%.*]] = phi <2 x i32> [ <i32 0, i32 1>, [[A]] ], [ <i32 2, i32 0>, [[B]] ]
; CHECK-NEXT:    ret <2 x i32> [[Y]]
;
  br i1 %cond, label %A, label %B
A:
  br label %C
B:
  br label %C
C:
  %y = phi <2 x i32> [<i32 0, i32 1>, %A], [<i32 2, i32 undef>, %B]
  %y.fr = freeze <2 x i32> %y
  ret <2 x i32> %y.fr
}

define i32 @one(i1 %cond, i32 %x) {
; CHECK-LABEL: @one(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[A:%.*]], label [[B:%.*]]
; CHECK:       A:
; CHECK-NEXT:    br label [[C:%.*]]
; CHECK:       B:
; CHECK-NEXT:    [[PHI_FR:%.*]] = freeze i32 [[X:%.*]]
; CHECK-NEXT:    br label [[C]]
; CHECK:       C:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ 0, [[A]] ], [ [[PHI_FR]], [[B]] ]
; CHECK-NEXT:    ret i32 [[Y]]
;
  br i1 %cond, label %A, label %B
A:
  br label %C
B:
  br label %C
C:
  %y = phi i32 [0, %A], [%x, %B]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}

define i32 @two(i1 %cond, i32 %x, i32 %x2) {
; CHECK-LABEL: @two(
; CHECK-NEXT:    br i1 [[COND:%.*]], label [[A:%.*]], label [[B:%.*]]
; CHECK:       A:
; CHECK-NEXT:    br label [[C:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[C]]
; CHECK:       C:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ [[X:%.*]], [[A]] ], [ [[X2:%.*]], [[B]] ]
; CHECK-NEXT:    [[Y_FR:%.*]] = freeze i32 [[Y]]
; CHECK-NEXT:    ret i32 [[Y_FR]]
;
  br i1 %cond, label %A, label %B
A:
  br label %C
B:
  br label %C
C:
  %y = phi i32 [%x, %A], [%x2, %B]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}

define i32 @two_undef(i8 %cond, i32 %x) {
; CHECK-LABEL: @two_undef(
; CHECK-NEXT:    switch i8 [[COND:%.*]], label [[A:%.*]] [
; CHECK-NEXT:    i8 0, label [[B:%.*]]
; CHECK-NEXT:    i8 1, label [[C:%.*]]
; CHECK-NEXT:    ]
; CHECK:       A:
; CHECK-NEXT:    br label [[D:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[D]]
; CHECK:       C:
; CHECK-NEXT:    br label [[D]]
; CHECK:       D:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ undef, [[A]] ], [ [[X:%.*]], [[B]] ], [ 0, [[C]] ]
; CHECK-NEXT:    [[Y_FR:%.*]] = freeze i32 [[Y]]
; CHECK-NEXT:    ret i32 [[Y_FR]]
;
  switch i8 %cond, label %A [
  i8 0, label %B
  i8 1, label %C
  ]
A:
  br label %D
B:
  br label %D
C:
  br label %D
D:
  %y = phi i32 [undef, %A], [%x, %B], [0, %C]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}

define i32 @one_undef(i8 %cond) {
; CHECK-LABEL: @one_undef(
; CHECK-NEXT:    switch i8 [[COND:%.*]], label [[A:%.*]] [
; CHECK-NEXT:    i8 0, label [[B:%.*]]
; CHECK-NEXT:    i8 1, label [[C:%.*]]
; CHECK-NEXT:    ]
; CHECK:       A:
; CHECK-NEXT:    br label [[D:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[D]]
; CHECK:       C:
; CHECK-NEXT:    br label [[D]]
; CHECK:       D:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ 0, [[A]] ], [ 32, [[B]] ], [ 0, [[C]] ]
; CHECK-NEXT:    ret i32 [[Y]]
;
  switch i8 %cond, label %A [
  i8 0, label %B
  i8 1, label %C
  ]
A:
  br label %D
B:
  br label %D
C:
  br label %D
D:
  %y = phi i32 [undef, %A], [32, %B], [0, %C]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}

@glb = global i8 0

define i32 @one_constexpr(i8 %cond, i32 %x) {
; CHECK-LABEL: @one_constexpr(
; CHECK-NEXT:    switch i8 [[COND:%.*]], label [[A:%.*]] [
; CHECK-NEXT:    i8 0, label [[B:%.*]]
; CHECK-NEXT:    i8 1, label [[C:%.*]]
; CHECK-NEXT:    ]
; CHECK:       A:
; CHECK-NEXT:    [[PHI_FR:%.*]] = freeze i32 ptrtoint (i8* getelementptr inbounds (i8, i8* @glb, i64 2) to i32)
; CHECK-NEXT:    br label [[D:%.*]]
; CHECK:       B:
; CHECK-NEXT:    br label [[D]]
; CHECK:       C:
; CHECK-NEXT:    br label [[D]]
; CHECK:       D:
; CHECK-NEXT:    [[Y:%.*]] = phi i32 [ [[PHI_FR]], [[A]] ], [ 32, [[B]] ], [ 0, [[C]] ]
; CHECK-NEXT:    ret i32 [[Y]]
;
  switch i8 %cond, label %A [
  i8 0, label %B
  i8 1, label %C
  ]
A:
  br label %D
B:
  br label %D
C:
  br label %D
D:
  %y = phi i32 [ptrtoint (i8* getelementptr inbounds (i8, i8* @glb, i64 2) to i32), %A], [32, %B], [0, %C]
  %y.fr = freeze i32 %y
  ret i32 %y.fr
}
