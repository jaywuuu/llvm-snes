; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -S -slp-vectorizer -mtriple=x86_64-unknown-linux-gnu < %s | FileCheck %s

define void @b() {
; CHECK-LABEL: @b(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[TMP0:%.*]] = insertelement <4 x float> poison, float 0x7FF8000000000000, i32 0
; CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <4 x float> [[TMP0]], <4 x float> <float 0xFFF8000000000000, float 0xFFF8000000000000, float undef, float undef>, <4 x i32> <i32 0, i32 4, i32 5, i32 3>
; CHECK-NEXT:    [[TMP2:%.*]] = insertelement <4 x float> [[TMP1]], float 0x7FF8000000000000, i32 3
; CHECK-NEXT:    [[TMP3:%.*]] = call <4 x float> @llvm.fmuladd.v4f32(<4 x float> [[TMP2]], <4 x float> zeroinitializer, <4 x float> zeroinitializer)
; CHECK-NEXT:    [[TMP4:%.*]] = fmul <4 x float> [[TMP3]], <float undef, float undef, float undef, float 2.000000e+00>
; CHECK-NEXT:    [[TMP5:%.*]] = fdiv <4 x float> [[TMP4]], zeroinitializer
; CHECK-NEXT:    store <4 x float> [[TMP5]], ptr undef, align 4
; CHECK-NEXT:    ret void
;
entry:
  %mul = fmul float undef, 2.000000e+00
  %i = tail call float @llvm.fmuladd.f32(float %mul, float 0.000000e+00, float 0.000000e+00)
  %mul2 = fmul float undef, %i
  %add = fadd float undef, 1.000000e+00
  %neg = fneg float %add
  %i1 = tail call float @llvm.fmuladd.f32(float %neg, float 0.000000e+00, float 0.000000e+00)
  %mul4 = fmul float undef, %i1
  %neg7 = fneg float %mul
  %i2 = tail call float @llvm.fmuladd.f32(float %neg7, float 0.000000e+00, float 0.000000e+00)
  %mul8 = fmul float undef, %i2
  %i3 = tail call float @llvm.fmuladd.f32(float %add, float 0.000000e+00, float 0.000000e+00)
  %mul11 = fmul float %i3, 2.000000e+00
  %div = fdiv float %mul2, 0.000000e+00
  store float %div, ptr undef, align 4
  %div12 = fdiv float %mul4, 0.000000e+00
  %arrayidx13 = getelementptr inbounds float, ptr undef, i64 1
  store float %div12, ptr %arrayidx13, align 4
  %div14 = fdiv float %mul8, 0.000000e+00
  %arrayidx15 = getelementptr inbounds float, ptr undef, i64 2
  store float %div14, ptr %arrayidx15, align 4
  %div16 = fdiv float %mul11, 0.000000e+00
  %arrayidx17 = getelementptr inbounds float, ptr undef, i64 3
  store float %div16, ptr %arrayidx17, align 4
  ret void
}

declare float @llvm.fmuladd.f32(float, float, float)

define void @test(float %a) {
; CHECK-LABEL: @test(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[TMP0:%.*]] = insertelement <2 x float> poison, float [[A:%.*]], i32 0
; CHECK-NEXT:    [[TMP1:%.*]] = insertelement <2 x float> [[TMP0]], float [[A]], i32 1
; CHECK-NEXT:    br label [[LOOP:%.*]]
; CHECK:       loop:
; CHECK-NEXT:    [[TMP2:%.*]] = fadd <2 x float> zeroinitializer, [[TMP1]]
; CHECK-NEXT:    [[TMP3:%.*]] = shufflevector <2 x float> [[TMP2]], <2 x float> poison, <2 x i32> zeroinitializer
; CHECK-NEXT:    br label [[LOOP]]
;
entry:
  br label %loop

loop:
  %add.i157 = fadd float 0.000000e+00, %a
  %add23.i = fadd float 0.000000e+00, %a
  %insert = insertelement <2 x float> zeroinitializer, float %add.i157, i64 0
  %insert.i = insertelement <2 x float> %insert, float %add23.i, i64 1
  %agg = insertelement <2 x float> %insert.i, float %add.i157, i64 1
  br label %loop
}

define internal void @test1() {
; CHECK-LABEL: @test1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br label [[LOOP:%.*]]
; CHECK:       loop:
; CHECK-NEXT:    [[DOTSROA_025_4_VEC_INSERT_US_I:%.*]] = insertelement <2 x float> zeroinitializer, float 0.000000e+00, i64 0
; CHECK-NEXT:    br label [[LOOP]]
;
entry:
  br label %loop

loop:
  %0 = fadd float 0.000000e+00, 0.000000e+00
  %1 = fadd float 0.000000e+00, 0.000000e+00
  %2 = fadd float %0, 0.000000e+00
  %3 = fadd float %1, 0.000000e+00
  %.sroa.3.8.vec.insert.i.us.i = insertelement <2 x float> zeroinitializer, float %2, i64 0
  %.sroa.3.12.vec.insert.i.us.i = insertelement <2 x float> %.sroa.3.8.vec.insert.i.us.i, float %3, i64 1
  %.sroa.025.4.vec.insert.us.i = insertelement <2 x float> %.sroa.3.12.vec.insert.i.us.i, float %0, i64 0
  br label %loop
}
