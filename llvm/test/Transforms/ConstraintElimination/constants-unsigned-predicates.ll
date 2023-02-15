; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -passes=constraint-elimination -S %s | FileCheck %s

define i1 @test_ult() {
; CHECK-LABEL: @test_ult(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[T_0:%.*]] = icmp ult i8 10, 11
; CHECK-NEXT:    [[F_0:%.*]] = icmp ult i8 10, 10
; CHECK-NEXT:    [[RES_1:%.*]] = xor i1 true, false
; CHECK-NEXT:    [[F_1:%.*]] = icmp ult i8 10, 9
; CHECK-NEXT:    [[RES_2:%.*]] = xor i1 [[RES_1]], false
; CHECK-NEXT:    [[T_1:%.*]] = icmp ult i8 10, -10
; CHECK-NEXT:    [[RES_3:%.*]] = xor i1 [[RES_2]], true
; CHECK-NEXT:    ret i1 [[RES_3]]
;
entry:
  %t.0 = icmp ult i8 10, 11
  %f.0 = icmp ult i8 10, 10
  %res.1 = xor i1 %t.0, %f.0
  %f.1 = icmp ult i8 10, 9
  %res.2 = xor i1 %res.1, %f.1
  %t.1 = icmp ult i8 10, -10
  %res.3 = xor i1 %res.2, %t.1
  ret i1 %res.3
}

; Test cases where lhs - rhs results in constant offset.
define i1 @test_ult_gep_1(ptr %base) {
; CHECK-LABEL: @test_ult_gep_1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[GEP_1:%.*]] = getelementptr inbounds i8, ptr [[BASE:%.*]], i8 1
; CHECK-NEXT:    [[T_0:%.*]] = icmp ult ptr [[BASE]], [[GEP_1]]
; CHECK-NEXT:    [[GEP_0:%.*]] = getelementptr inbounds i8, ptr [[BASE]], i8 0
; CHECK-NEXT:    [[F_0:%.*]] = icmp ult ptr [[BASE]], [[GEP_0]]
; CHECK-NEXT:    [[RES_1:%.*]] = xor i1 true, false
; CHECK-NEXT:    [[F_1:%.*]] = icmp ult ptr [[GEP_1]], [[BASE]]
; CHECK-NEXT:    [[RES_2:%.*]] = xor i1 [[RES_1]], false
; CHECK-NEXT:    ret i1 [[RES_2]]
;
entry:
  %gep.1 = getelementptr inbounds i8, ptr %base, i8 1
  %t.0 = icmp ult ptr %base, %gep.1
  %gep.0 = getelementptr inbounds i8, ptr %base, i8 0
  %f.0 = icmp ult ptr %base, %gep.0
  %res.1 = xor i1 %t.0, %f.0
  %f.1 = icmp ult ptr %gep.1, %base
  %res.2 = xor i1 %res.1, %f.1
  ret i1 %res.2
}

define i1 @test_ult_gep_2(ptr %base) {
; CHECK-LABEL: @test_ult_gep_2(
; CHECK-NEXT:    [[GEP_SUB_1:%.*]] = getelementptr inbounds i8, ptr [[BASE:%.*]], i8 -1
; CHECK-NEXT:    [[C_1:%.*]] = icmp ult ptr [[BASE]], [[GEP_SUB_1]]
; CHECK-NEXT:    ret i1 [[C_1]]
;
  %gep.sub.1 = getelementptr inbounds i8, ptr %base, i8 -1
  %c.1 = icmp ult ptr %base, %gep.sub.1
  ret i1 %c.1
}

define i1 @test_ult_gep_3(ptr %base) {
; CHECK-LABEL: @test_ult_gep_3(
; CHECK-NEXT:    [[GEP_1_NOINBOUNDS:%.*]] = getelementptr i8, ptr [[BASE:%.*]], i8 1
; CHECK-NEXT:    [[C_1:%.*]] = icmp ult ptr [[BASE]], [[GEP_1_NOINBOUNDS]]
; CHECK-NEXT:    ret i1 [[C_1]]
;
  %gep.1.noinbounds = getelementptr i8, ptr %base, i8 1
  %c.1 = icmp ult ptr %base, %gep.1.noinbounds
  ret i1 %c.1
}

define i1 @test_eq() {
; CHECK-LABEL: @test_eq(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[F_0:%.*]] = icmp eq i8 10, 11
; CHECK-NEXT:    [[T_0:%.*]] = icmp eq i8 10, 10
; CHECK-NEXT:    [[RES_1:%.*]] = xor i1 [[T_0]], [[F_0]]
; CHECK-NEXT:    [[F_1:%.*]] = icmp eq i8 10, 9
; CHECK-NEXT:    [[RES_2:%.*]] = xor i1 [[RES_1]], [[F_1]]
; CHECK-NEXT:    ret i1 [[RES_2]]
;
entry:
  %f.0 = icmp eq i8 10, 11
  %t.0 = icmp eq i8 10, 10
  %res.1 = xor i1 %t.0, %f.0
  %f.1 = icmp eq i8 10, 9
  %res.2 = xor i1 %res.1, %f.1
  ret i1 %res.2
}

define i1 @test_ne() {
; CHECK-LABEL: @test_ne(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[T_0:%.*]] = icmp ne i8 10, 11
; CHECK-NEXT:    [[F_0:%.*]] = icmp ne i8 10, 10
; CHECK-NEXT:    [[RES_1:%.*]] = xor i1 [[T_0]], [[F_0]]
; CHECK-NEXT:    [[T_1:%.*]] = icmp ne i8 10, 9
; CHECK-NEXT:    [[RES_2:%.*]] = xor i1 [[RES_1]], [[T_1]]
; CHECK-NEXT:    ret i1 [[RES_2]]
;
entry:
  %t.0 = icmp ne i8 10, 11
  %f.0 = icmp ne i8 10, 10
  %res.1 = xor i1 %t.0, %f.0
  %t.1 = icmp ne i8 10, 9
  %res.2 = xor i1 %res.1, %t.1
  ret i1 %res.2
}
