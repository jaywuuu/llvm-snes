; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -passes=instcombine -S | FileCheck %s
target datalayout = "n64"

; Tests for removing zext/trunc from/to i8, i16 and i32, even if it is
; not a legal type.

define i8 @test1(i8 %x, i8 %y) {
; CHECK-LABEL: @test1(
; CHECK-NEXT:    [[C:%.*]] = add i8 [[X:%.*]], [[Y:%.*]]
; CHECK-NEXT:    ret i8 [[C]]
;
  %xz = zext i8 %x to i64
  %yz = zext i8 %y to i64
  %c = add i64 %xz, %yz
  %d = trunc i64 %c to i8
  ret i8 %d
}

define i16 @test2(i16 %x, i16 %y) {
; CHECK-LABEL: @test2(
; CHECK-NEXT:    [[C:%.*]] = add i16 [[X:%.*]], [[Y:%.*]]
; CHECK-NEXT:    ret i16 [[C]]
;
  %xz = zext i16 %x to i64
  %yz = zext i16 %y to i64
  %c = add i64 %xz, %yz
  %d = trunc i64 %c to i16
  ret i16 %d
}

define i32 @test3(i32 %x, i32 %y) {
; CHECK-LABEL: @test3(
; CHECK-NEXT:    [[C:%.*]] = add i32 [[X:%.*]], [[Y:%.*]]
; CHECK-NEXT:    ret i32 [[C]]
;
  %xz = zext i32 %x to i64
  %yz = zext i32 %y to i64
  %c = add i64 %xz, %yz
  %d = trunc i64 %c to i32
  ret i32 %d
}

define i9 @test4(i9 %x, i9 %y) {
; CHECK-LABEL: @test4(
; CHECK-NEXT:    [[XZ:%.*]] = zext i9 [[X:%.*]] to i64
; CHECK-NEXT:    [[YZ:%.*]] = zext i9 [[Y:%.*]] to i64
; CHECK-NEXT:    [[C:%.*]] = add nuw nsw i64 [[XZ]], [[YZ]]
; CHECK-NEXT:    [[D:%.*]] = trunc i64 [[C]] to i9
; CHECK-NEXT:    ret i9 [[D]]
;
  %xz = zext i9 %x to i64
  %yz = zext i9 %y to i64
  %c = add i64 %xz, %yz
  %d = trunc i64 %c to i9
  ret i9 %d
}
