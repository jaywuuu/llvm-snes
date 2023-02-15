; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -O2 -S < %s | FileCheck %s

; Given an "expect" on a compare, we should not combine
; that compare with other instructions in a way that the
; backend can't undo. Expect lowering becomes metadata,
; and passes like SimplifyCFG should respect that.

define void @PR49336(i32 %delta, i32 %tag_type, i8* %ip) {
; CHECK-LABEL: @PR49336(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP:%.*]] = icmp slt i32 [[DELTA:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP]], label [[IF_THEN:%.*]], label [[IF_END3:%.*]], !prof !0
; CHECK:       if.then:
; CHECK-NEXT:    [[CMP1_NOT:%.*]] = icmp eq i32 [[TAG_TYPE:%.*]], 0
; CHECK-NEXT:    br i1 [[CMP1_NOT]], label [[IF_END3]], label [[IF_THEN2:%.*]]
; CHECK:       if.then2:
; CHECK-NEXT:    store i8 42, i8* [[IP:%.*]], align 1
; CHECK-NEXT:    br label [[IF_END3]]
; CHECK:       if.end3:
; CHECK-NEXT:    ret void
;
entry:
  %delta.addr = alloca i32, align 4
  %tag_type.addr = alloca i32, align 4
  %ip.addr = alloca i8*, align 8
  store i32 %delta, i32* %delta.addr, align 4
  store i32 %tag_type, i32* %tag_type.addr, align 4
  store i8* %ip, i8** %ip.addr, align 8
  %0 = load i32, i32* %delta.addr, align 4
  %cmp = icmp slt i32 %0, 0
  %conv = zext i1 %cmp to i64
  %expval = call i64 @llvm.expect.i64(i64 %conv, i64 0)
  %tobool = icmp ne i64 %expval, 0
  br i1 %tobool, label %if.then, label %if.end3

if.then:
  %1 = load i32, i32* %tag_type.addr, align 4
  %cmp1 = icmp ne i32 %1, 0
  br i1 %cmp1, label %if.then2, label %if.end

if.then2:
  %2 = load i8*, i8** %ip.addr, align 8
  store i8 42, i8* %2, align 1
  br label %if.end

if.end:
  br label %if.end3

if.end3:
  ret void
}

declare i64 @llvm.expect.i64(i64, i64)