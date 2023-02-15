; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py
; RUN: opt < %s -mtriple=powerpc64-unknown-linux-gnu -mcpu=pwr8 -passes='print<cost-model>' -cost-kind=throughput 2>&1 -disable-output | FileCheck %s

define i32 @reduce_i1(i32 %arg) {
; CHECK-LABEL: 'reduce_i1'
; CHECK-NEXT:  Cost Model: Found an estimated cost of 3 for instruction: %V1 = call i1 @llvm.vector.reduce.and.v1i1(<1 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %V2 = call i1 @llvm.vector.reduce.and.v2i1(<2 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %V4 = call i1 @llvm.vector.reduce.and.v4i1(<4 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %V8 = call i1 @llvm.vector.reduce.and.v8i1(<8 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %V16 = call i1 @llvm.vector.reduce.and.v16i1(<16 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 97 for instruction: %V32 = call i1 @llvm.vector.reduce.and.v32i1(<32 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 193 for instruction: %V64 = call i1 @llvm.vector.reduce.and.v64i1(<64 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 386 for instruction: %V128 = call i1 @llvm.vector.reduce.and.v128i1(<128 x i1> undef)
; CHECK-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: ret i32 undef
;
  %V1   = call i1 @llvm.vector.reduce.and.v1i1(<1 x i1> undef)
  %V2   = call i1 @llvm.vector.reduce.and.v2i1(<2 x i1> undef)
  %V4   = call i1 @llvm.vector.reduce.and.v4i1(<4 x i1> undef)
  %V8   = call i1 @llvm.vector.reduce.and.v8i1(<8 x i1> undef)
  %V16  = call i1 @llvm.vector.reduce.and.v16i1(<16 x i1> undef)
  %V32  = call i1 @llvm.vector.reduce.and.v32i1(<32 x i1> undef)
  %V64  = call i1 @llvm.vector.reduce.and.v64i1(<64 x i1> undef)
  %V128 = call i1 @llvm.vector.reduce.and.v128i1(<128 x i1> undef)
  ret i32 undef
}

declare i1 @llvm.vector.reduce.and.v1i1(<1 x i1>)
declare i1 @llvm.vector.reduce.and.v2i1(<2 x i1>)
declare i1 @llvm.vector.reduce.and.v4i1(<4 x i1>)
declare i1 @llvm.vector.reduce.and.v8i1(<8 x i1>)
declare i1 @llvm.vector.reduce.and.v16i1(<16 x i1>)
declare i1 @llvm.vector.reduce.and.v32i1(<32 x i1>)
declare i1 @llvm.vector.reduce.and.v64i1(<64 x i1>)
declare i1 @llvm.vector.reduce.and.v128i1(<128 x i1>)
