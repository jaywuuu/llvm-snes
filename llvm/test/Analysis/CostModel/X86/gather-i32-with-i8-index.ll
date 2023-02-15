; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py UTC_ARGS: --filter "LV: Found an estimated cost of [0-9]+ for VF [0-9]+ For instruction:\s*%valB = load i32, i32\* %inB, align 4"
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+sse2 --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=SSE2
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+sse4.2 --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=SSE42
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+avx  --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=AVX1
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+avx2,-fast-gather --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=AVX2-SLOWGATHER
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+avx2,+fast-gather --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=AVX2-FASTGATHER
; RUN: opt -loop-vectorize -vectorizer-maximize-bandwidth -S -mattr=+avx512bw --debug-only=loop-vectorize < %s 2>&1 | FileCheck %s --check-prefixes=AVX512

; REQUIRES: asserts

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@A = global [1024 x i8] zeroinitializer, align 128
@B = global [1024 x i32] zeroinitializer, align 128
@C = global [1024 x i32] zeroinitializer, align 128

define void @test() {
; SSE2-LABEL: 'test'
; SSE2:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; SSE2:  LV: Found an estimated cost of 25 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; SSE2:  LV: Found an estimated cost of 51 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; SSE2:  LV: Found an estimated cost of 102 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; SSE2:  LV: Found an estimated cost of 204 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
;
; SSE42-LABEL: 'test'
; SSE42:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; SSE42:  LV: Found an estimated cost of 24 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; SSE42:  LV: Found an estimated cost of 48 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; SSE42:  LV: Found an estimated cost of 96 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; SSE42:  LV: Found an estimated cost of 192 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
;
; AVX1-LABEL: 'test'
; AVX1:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; AVX1:  LV: Found an estimated cost of 24 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; AVX1:  LV: Found an estimated cost of 48 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; AVX1:  LV: Found an estimated cost of 98 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; AVX1:  LV: Found an estimated cost of 196 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
; AVX1:  LV: Found an estimated cost of 392 for VF 32 For instruction: %valB = load i32, i32* %inB, align 4
;
; AVX2-SLOWGATHER-LABEL: 'test'
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 4 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 8 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 18 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 36 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-SLOWGATHER:  LV: Found an estimated cost of 72 for VF 32 For instruction: %valB = load i32, i32* %inB, align 4
;
; AVX2-FASTGATHER-LABEL: 'test'
; AVX2-FASTGATHER:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-FASTGATHER:  LV: Found an estimated cost of 4 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-FASTGATHER:  LV: Found an estimated cost of 6 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-FASTGATHER:  LV: Found an estimated cost of 12 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-FASTGATHER:  LV: Found an estimated cost of 24 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
; AVX2-FASTGATHER:  LV: Found an estimated cost of 48 for VF 32 For instruction: %valB = load i32, i32* %inB, align 4
;
; AVX512-LABEL: 'test'
; AVX512:  LV: Found an estimated cost of 1 for VF 1 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 6 for VF 2 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 13 for VF 4 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 10 for VF 8 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 18 for VF 16 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 36 for VF 32 For instruction: %valB = load i32, i32* %inB, align 4
; AVX512:  LV: Found an estimated cost of 72 for VF 64 For instruction: %valB = load i32, i32* %inB, align 4
;
entry:
  br label %for.body

for.body:
  %iv = phi i64 [ 0, %entry ], [ %iv.next, %for.body ]

  %inA = getelementptr inbounds [1024 x i8], [1024 x i8]* @A, i64 0, i64 %iv
  %valA = load i8, i8* %inA
  %valA.ext = sext i8 %valA to i64

  %inB = getelementptr inbounds [1024 x i32], [1024 x i32]* @B, i64 0, i64 %valA.ext
  %valB = load i32, i32* %inB

  %out = getelementptr inbounds [1024 x i32], [1024 x i32]* @C, i64 0, i64 %iv
  store i32 %valB, i32* %out

  %iv.next = add nuw nsw i64 %iv, 1
  %cmp = icmp ult i64 %iv.next, 1024
  br i1 %cmp, label %for.body, label %for.cond.cleanup

for.cond.cleanup:
  ret void
}
