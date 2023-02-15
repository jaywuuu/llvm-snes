; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -S -instcombine %s | FileCheck %s

; Treatment of operation with unused result.

; If operation does not raise exceptions, it may be removed even in strict mode.
define float @f_unused_precise() #0 {
; CHECK-LABEL: @f_unused_precise(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 1.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fadd.f32(float 1.0, float 1.0, metadata !"round.upward", metadata !"fpexcept.strict") #0
  ret float 1.0
}

; If operation raises exceptions, it cannot be removed in strict mode.
define float @f_unused_strict() #0 {
; CHECK-LABEL: @f_unused_strict(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[RESULT:%.*]] = call float @llvm.experimental.constrained.fdiv.f32(float 1.000000e+00, float 3.000000e+00, metadata !"round.tonearest", metadata !"fpexcept.strict") #[[ATTR0:[0-9]+]]
; CHECK-NEXT:    ret float 1.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.tonearest", metadata !"fpexcept.strict") #0
  ret float 1.0
}

; If operation raises exceptions, it can be removed in non-strict mode.
define float @f_unused_ignore() #0 {
; CHECK-LABEL: @f_unused_ignore(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 1.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.towardzero", metadata !"fpexcept.ignore") #0
  ret float 1.0
}

; If operation raises exceptions, it can be removed in non-strict mode even if rounding mode is dynamic.
define float @f_unused_dynamic_ignore() #0 {
; CHECK-LABEL: @f_unused_dynamic_ignore(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 1.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.dynamic", metadata !"fpexcept.ignore") #0
  ret float 1.0
}

; If operation raises exceptions, it can be removed in "maytrap" mode.
define float @f_unused_maytrap() #0 {
; CHECK-LABEL: @f_unused_maytrap(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 1.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.tonearest", metadata !"fpexcept.maytrap") #0
  ret float 1.0
}

; Constant evaluation.

; If operation does not raise exceptions, it may be folded even in strict mode.
define float @f_eval_precise() #0 {
; CHECK-LABEL: @f_eval_precise(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 2.000000e+00
;
entry:
  %result = call float @llvm.experimental.constrained.fadd.f32(float 1.0, float 1.0, metadata !"round.upward", metadata !"fpexcept.strict") #0
  ret float %result
}

; If operation raises exceptions, it cannot be folded in strict mode.
define float @f_eval_strict() #0 {
; CHECK-LABEL: @f_eval_strict(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[RESULT:%.*]] = call float @llvm.experimental.constrained.fdiv.f32(float 1.000000e+00, float 3.000000e+00, metadata !"round.upward", metadata !"fpexcept.strict") #[[ATTR0]]
; CHECK-NEXT:    ret float [[RESULT]]
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.upward", metadata !"fpexcept.strict") #0
  ret float %result
}

; If operation raises exceptions, it can be folded in non-strict mode.
define float @f_eval_ignore() #0 {
; CHECK-LABEL: @f_eval_ignore(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 0x3FD5555540000000
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.downward", metadata !"fpexcept.ignore") #0
  ret float %result
}

; if result is imprecise, it cannot be folded if rounding mode is dynamic.
define float @f_eval_dynamic_ignore() #0 {
; CHECK-LABEL: @f_eval_dynamic_ignore(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[RESULT:%.*]] = call float @llvm.experimental.constrained.fdiv.f32(float 1.000000e+00, float 3.000000e+00, metadata !"round.dynamic", metadata !"fpexcept.ignore") #[[ATTR0]]
; CHECK-NEXT:    ret float [[RESULT]]
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.dynamic", metadata !"fpexcept.ignore") #0
  ret float %result
}

; If result is imprecise and rounding mode is not dynamic, operation can be folded in "maytrap" mode.
define float @f_eval_maytrap() #0 {
; CHECK-LABEL: @f_eval_maytrap(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret float 0x3FD5555560000000
;
entry:
  %result = call float @llvm.experimental.constrained.fdiv.f32(float 1.0, float 3.0, metadata !"round.tonearest", metadata !"fpexcept.maytrap") #0
  ret float %result
}


declare float @llvm.experimental.constrained.fadd.f32(float, float, metadata, metadata)
declare float @llvm.experimental.constrained.fdiv.f32(float, float, metadata, metadata)

attributes #0 = { strictfp }
