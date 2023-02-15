; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -S -passes=instcombine < %s | FileCheck %s

target datalayout = "e-p:64:64:64-p1:16:16:16-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128"

define i64 @test_inbounds([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl nsw i64 [[IDX:%.*]], 2
; CHECK-NEXT:    ret i64 [[P2_IDX]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub i64 %i2, %i1
  ret i64 %d
}

define i64 @test_partial_inbounds1([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_partial_inbounds1(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl i64 [[IDX:%.*]], 2
; CHECK-NEXT:    ret i64 [[P2_IDX]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub i64 %i2, %i1
  ret i64 %d
}

define i64 @test_partial_inbounds2([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_partial_inbounds2(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl nsw i64 [[IDX:%.*]], 2
; CHECK-NEXT:    ret i64 [[P2_IDX]]
;
  %p1 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds_nuw([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds_nuw(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl nuw nsw i64 [[IDX:%.*]], 2
; CHECK-NEXT:    ret i64 [[P2_IDX]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_nuw([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_nuw(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl i64 [[IDX:%.*]], 2
; CHECK-NEXT:    ret i64 [[P2_IDX]]
;
  %p1 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i32 @test_inbounds_nuw_trunc([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds_nuw_trunc(
; CHECK-NEXT:    [[IDX_TR:%.*]] = trunc i64 [[IDX:%.*]] to i32
; CHECK-NEXT:    [[TMP1:%.*]] = shl i32 [[IDX_TR]], 2
; CHECK-NEXT:    ret i32 [[TMP1]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %t1 = trunc i64 %i1 to i32
  %t2 = trunc i64 %i2 to i32
  %d = sub nuw i32 %t2, %t1
  ret i32 %d
}

define i64 @test_inbounds_nuw_swapped([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds_nuw_swapped(
; CHECK-NEXT:    [[P2_IDX_NEG:%.*]] = mul i64 [[IDX:%.*]], -4
; CHECK-NEXT:    ret i64 [[P2_IDX_NEG]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p2 to i64
  %i2 = ptrtoint i32* %p1 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds1_nuw_swapped([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds1_nuw_swapped(
; CHECK-NEXT:    [[P2_IDX_NEG:%.*]] = mul i64 [[IDX:%.*]], -4
; CHECK-NEXT:    ret i64 [[P2_IDX_NEG]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p2 to i64
  %i2 = ptrtoint i32* %p1 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds2_nuw_swapped([0 x i32]* %base, i64 %idx) {
; CHECK-LABEL: @test_inbounds2_nuw_swapped(
; CHECK-NEXT:    [[P2_IDX_NEG:%.*]] = mul i64 [[IDX:%.*]], -4
; CHECK-NEXT:    ret i64 [[P2_IDX_NEG]]
;
  %p1 = getelementptr [0 x i32], [0 x i32]* %base, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %i1 = ptrtoint i32* %p2 to i64
  %i2 = ptrtoint i32* %p1 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds_two_gep([0 x i32]* %base, i64 %idx, i64 %idx2) {
; CHECK-LABEL: @test_inbounds_two_gep(
; CHECK-NEXT:    [[TMP1:%.*]] = sub nsw i64 [[IDX2:%.*]], [[IDX:%.*]]
; CHECK-NEXT:    [[GEPDIFF:%.*]] = shl nsw i64 [[TMP1]], 2
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx2
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds_nsw_two_gep([0 x i32]* %base, i64 %idx, i64 %idx2) {
; CHECK-LABEL: @test_inbounds_nsw_two_gep(
; CHECK-NEXT:    [[TMP1:%.*]] = sub nsw i64 [[IDX2:%.*]], [[IDX:%.*]]
; CHECK-NEXT:    [[GEPDIFF:%.*]] = shl nsw i64 [[TMP1]], 2
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx2
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub nsw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds_nuw_two_gep([0 x i32]* %base, i64 %idx, i64 %idx2) {
; CHECK-LABEL: @test_inbounds_nuw_two_gep(
; CHECK-NEXT:    [[TMP1:%.*]] = sub nsw i64 [[IDX2:%.*]], [[IDX:%.*]]
; CHECK-NEXT:    [[GEPDIFF:%.*]] = shl nsw i64 [[TMP1]], 2
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %p1 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx
  %p2 = getelementptr inbounds [0 x i32], [0 x i32]* %base, i64 0, i64 %idx2
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

define i64 @test_inbounds_nuw_multi_index([0 x [2 x i32]]* %base, i64 %idx, i64 %idx2) {
; CHECK-LABEL: @test_inbounds_nuw_multi_index(
; CHECK-NEXT:    [[P2_IDX:%.*]] = shl nsw i64 [[IDX:%.*]], 3
; CHECK-NEXT:    [[P2_IDX1:%.*]] = shl nsw i64 [[IDX2:%.*]], 2
; CHECK-NEXT:    [[P2_OFFS:%.*]] = add nsw i64 [[P2_IDX]], [[P2_IDX1]]
; CHECK-NEXT:    ret i64 [[P2_OFFS]]
;
  %p1 = getelementptr inbounds [0 x [2 x i32]], [0 x [2 x i32]]* %base, i64 0, i64 0, i64 0
  %p2 = getelementptr inbounds [0 x [2 x i32]], [0 x [2 x i32]]* %base, i64 0, i64 %idx, i64 %idx2
  %i1 = ptrtoint i32* %p1 to i64
  %i2 = ptrtoint i32* %p2 to i64
  %d = sub nuw i64 %i2, %i1
  ret i64 %d
}

; rdar://7362831
define i32 @test23(i8* %P, i64 %A){
; CHECK-LABEL: @test23(
; CHECK-NEXT:    [[TMP1:%.*]] = trunc i64 [[A:%.*]] to i32
; CHECK-NEXT:    ret i32 [[TMP1]]
;
  %B = getelementptr inbounds i8, i8* %P, i64 %A
  %C = ptrtoint i8* %B to i64
  %D = trunc i64 %C to i32
  %E = ptrtoint i8* %P to i64
  %F = trunc i64 %E to i32
  %G = sub i32 %D, %F
  ret i32 %G
}

define i8 @test23_as1(i8 addrspace(1)* %P, i16 %A) {
; CHECK-LABEL: @test23_as1(
; CHECK-NEXT:    [[TMP1:%.*]] = trunc i16 [[A:%.*]] to i8
; CHECK-NEXT:    ret i8 [[TMP1]]
;
  %B = getelementptr inbounds i8, i8 addrspace(1)* %P, i16 %A
  %C = ptrtoint i8 addrspace(1)* %B to i16
  %D = trunc i16 %C to i8
  %E = ptrtoint i8 addrspace(1)* %P to i16
  %F = trunc i16 %E to i8
  %G = sub i8 %D, %F
  ret i8 %G
}

define i64 @test24(i8* %P, i64 %A){
; CHECK-LABEL: @test24(
; CHECK-NEXT:    ret i64 [[A:%.*]]
;
  %B = getelementptr inbounds i8, i8* %P, i64 %A
  %C = ptrtoint i8* %B to i64
  %E = ptrtoint i8* %P to i64
  %G = sub i64 %C, %E
  ret i64 %G
}

define i16 @test24_as1(i8 addrspace(1)* %P, i16 %A) {
; CHECK-LABEL: @test24_as1(
; CHECK-NEXT:    ret i16 [[A:%.*]]
;
  %B = getelementptr inbounds i8, i8 addrspace(1)* %P, i16 %A
  %C = ptrtoint i8 addrspace(1)* %B to i16
  %E = ptrtoint i8 addrspace(1)* %P to i16
  %G = sub i16 %C, %E
  ret i16 %G
}

define i64 @test24a(i8* %P, i64 %A){
; CHECK-LABEL: @test24a(
; CHECK-NEXT:    [[DIFF_NEG:%.*]] = sub i64 0, [[A:%.*]]
; CHECK-NEXT:    ret i64 [[DIFF_NEG]]
;
  %B = getelementptr inbounds i8, i8* %P, i64 %A
  %C = ptrtoint i8* %B to i64
  %E = ptrtoint i8* %P to i64
  %G = sub i64 %E, %C
  ret i64 %G
}

define i16 @test24a_as1(i8 addrspace(1)* %P, i16 %A) {
; CHECK-LABEL: @test24a_as1(
; CHECK-NEXT:    [[DIFF_NEG:%.*]] = sub i16 0, [[A:%.*]]
; CHECK-NEXT:    ret i16 [[DIFF_NEG]]
;
  %B = getelementptr inbounds i8, i8 addrspace(1)* %P, i16 %A
  %C = ptrtoint i8 addrspace(1)* %B to i16
  %E = ptrtoint i8 addrspace(1)* %P to i16
  %G = sub i16 %E, %C
  ret i16 %G
}

@Arr = external global [42 x i16]

define i64 @test24b(i8* %P, i64 %A){
; CHECK-LABEL: @test24b(
; CHECK-NEXT:    [[B_IDX:%.*]] = shl nsw i64 [[A:%.*]], 1
; CHECK-NEXT:    ret i64 [[B_IDX]]
;
  %B = getelementptr inbounds [42 x i16], [42 x i16]* @Arr, i64 0, i64 %A
  %C = ptrtoint i16* %B to i64
  %G = sub i64 %C, ptrtoint ([42 x i16]* @Arr to i64)
  ret i64 %G
}

define i64 @test25(i8* %P, i64 %A){
; CHECK-LABEL: @test25(
; CHECK-NEXT:    [[B_IDX:%.*]] = shl nsw i64 [[A:%.*]], 1
; CHECK-NEXT:    [[GEPDIFF:%.*]] = add nsw i64 [[B_IDX]], -84
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %B = getelementptr inbounds [42 x i16], [42 x i16]* @Arr, i64 0, i64 %A
  %C = ptrtoint i16* %B to i64
  %G = sub i64 %C, ptrtoint (i16* getelementptr ([42 x i16], [42 x i16]* @Arr, i64 1, i64 0) to i64)
  ret i64 %G
}

@Arr_as1 = external addrspace(1) global [42 x i16]

define i16 @test25_as1(i8 addrspace(1)* %P, i64 %A) {
; CHECK-LABEL: @test25_as1(
; CHECK-NEXT:    [[TMP1:%.*]] = trunc i64 [[A:%.*]] to i16
; CHECK-NEXT:    [[B_IDX:%.*]] = shl nsw i16 [[TMP1]], 1
; CHECK-NEXT:    [[GEPDIFF:%.*]] = add nsw i16 [[B_IDX]], -84
; CHECK-NEXT:    ret i16 [[GEPDIFF]]
;
  %B = getelementptr inbounds [42 x i16], [42 x i16] addrspace(1)* @Arr_as1, i64 0, i64 %A
  %C = ptrtoint i16 addrspace(1)* %B to i16
  %G = sub i16 %C, ptrtoint (i16 addrspace(1)* getelementptr ([42 x i16], [42 x i16] addrspace(1)* @Arr_as1, i64 1, i64 0) to i16)
  ret i16 %G
}

define i64 @test30(i8* %foo, i64 %i, i64 %j) {
; CHECK-LABEL: @test30(
; CHECK-NEXT:    [[GEP1_IDX:%.*]] = shl nsw i64 [[I:%.*]], 2
; CHECK-NEXT:    [[GEPDIFF:%.*]] = sub nsw i64 [[GEP1_IDX]], [[J:%.*]]
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %bit = bitcast i8* %foo to i32*
  %gep1 = getelementptr inbounds i32, i32* %bit, i64 %i
  %gep2 = getelementptr inbounds i8, i8* %foo, i64 %j
  %cast1 = ptrtoint i32* %gep1 to i64
  %cast2 = ptrtoint i8* %gep2 to i64
  %sub = sub i64 %cast1, %cast2
  ret i64 %sub
}

define i16 @test30_as1(i8 addrspace(1)* %foo, i16 %i, i16 %j) {
; CHECK-LABEL: @test30_as1(
; CHECK-NEXT:    [[GEP1_IDX:%.*]] = shl nsw i16 [[I:%.*]], 2
; CHECK-NEXT:    [[GEPDIFF:%.*]] = sub nsw i16 [[GEP1_IDX]], [[J:%.*]]
; CHECK-NEXT:    ret i16 [[GEPDIFF]]
;
  %bit = bitcast i8 addrspace(1)* %foo to i32 addrspace(1)*
  %gep1 = getelementptr inbounds i32, i32 addrspace(1)* %bit, i16 %i
  %gep2 = getelementptr inbounds i8, i8 addrspace(1)* %foo, i16 %j
  %cast1 = ptrtoint i32 addrspace(1)* %gep1 to i16
  %cast2 = ptrtoint i8 addrspace(1)* %gep2 to i16
  %sub = sub i16 %cast1, %cast2
  ret i16 %sub
}

; Inbounds translates to 'nsw' on sub

define i64 @gep_diff_both_inbounds(i8* %foo, i64 %i, i64 %j) {
; CHECK-LABEL: @gep_diff_both_inbounds(
; CHECK-NEXT:    [[GEPDIFF:%.*]] = sub nsw i64 [[I:%.*]], [[J:%.*]]
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %gep1 = getelementptr inbounds i8, i8* %foo, i64 %i
  %gep2 = getelementptr inbounds i8, i8* %foo, i64 %j
  %cast1 = ptrtoint i8* %gep1 to i64
  %cast2 = ptrtoint i8* %gep2 to i64
  %sub = sub i64 %cast1, %cast2
  ret i64 %sub
}

; Negative test for 'nsw' - both geps must be inbounds

define i64 @gep_diff_first_inbounds(i8* %foo, i64 %i, i64 %j) {
; CHECK-LABEL: @gep_diff_first_inbounds(
; CHECK-NEXT:    [[GEPDIFF:%.*]] = sub i64 [[I:%.*]], [[J:%.*]]
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %gep1 = getelementptr inbounds i8, i8* %foo, i64 %i
  %gep2 = getelementptr i8, i8* %foo, i64 %j
  %cast1 = ptrtoint i8* %gep1 to i64
  %cast2 = ptrtoint i8* %gep2 to i64
  %sub = sub i64 %cast1, %cast2
  ret i64 %sub
}

; Negative test for 'nsw' - both geps must be inbounds

define i64 @gep_diff_second_inbounds(i8* %foo, i64 %i, i64 %j) {
; CHECK-LABEL: @gep_diff_second_inbounds(
; CHECK-NEXT:    [[GEPDIFF:%.*]] = sub i64 [[I:%.*]], [[J:%.*]]
; CHECK-NEXT:    ret i64 [[GEPDIFF]]
;
  %gep1 = getelementptr i8, i8* %foo, i64 %i
  %gep2 = getelementptr inbounds i8, i8* %foo, i64 %j
  %cast1 = ptrtoint i8* %gep1 to i64
  %cast2 = ptrtoint i8* %gep2 to i64
  %sub = sub i64 %cast1, %cast2
  ret i64 %sub
}

define i64 @gep_diff_with_bitcast(i64* %p, i64 %idx) {
; CHECK-LABEL: @gep_diff_with_bitcast(
; CHECK-NEXT:    ret i64 [[IDX:%.*]]
;
  %i0 = bitcast i64* %p to [4 x i64]*
  %i1 = getelementptr inbounds [4 x i64], [4 x i64]* %i0, i64 %idx
  %i3 = ptrtoint [4 x i64]* %i1 to i64
  %i4 = ptrtoint i64* %p to i64
  %i5 = sub nuw i64 %i3, %i4
  %i6 = lshr i64 %i5, 5
  ret i64 %i6
}
