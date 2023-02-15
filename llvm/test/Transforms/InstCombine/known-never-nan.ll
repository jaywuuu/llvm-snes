; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -S -passes=instcombine | FileCheck %s

; This file used to contain more tests that folded to true/false,
; but those are all tested identically in InstSimplify now.
; If any remaining tests are made to return true/false, that
; functionality/testing may be better housed in InstSimplify
; rather than InstCombine.

define i1 @fabs_sqrt_src_maybe_nan(double %arg0, double %arg1) {
; CHECK-LABEL: @fabs_sqrt_src_maybe_nan(
; CHECK-NEXT:    [[FABS:%.*]] = call double @llvm.fabs.f64(double [[ARG0:%.*]])
; CHECK-NEXT:    [[OP:%.*]] = call double @llvm.sqrt.f64(double [[FABS]])
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %fabs = call double @llvm.fabs.f64(double %arg0)
  %op = call double @llvm.sqrt.f64(double %fabs)
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @select_maybe_nan_lhs(i1 %cond, double %lhs, double %arg1) {
; CHECK-LABEL: @select_maybe_nan_lhs(
; CHECK-NEXT:    [[RHS:%.*]] = fadd nnan double [[ARG1:%.*]], 1.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = select i1 [[COND:%.*]], double [[LHS:%.*]], double [[RHS]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %rhs = fadd nnan double %arg1, 1.0
  %op = select i1 %cond, double %lhs, double %rhs
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @select_maybe_nan_rhs(i1 %cond, double %arg0, double %rhs) {
; CHECK-LABEL: @select_maybe_nan_rhs(
; CHECK-NEXT:    [[LHS:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = select i1 [[COND:%.*]], double [[LHS]], double [[RHS:%.*]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %lhs = fadd nnan double %arg0, 1.0
  %op = select i1 %cond, double %lhs, double %rhs
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_fadd(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fadd(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG0]], 2.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fadd double [[NNAN_ARG0]], [[NNAN_ARG1]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %nnan.arg1 = fadd nnan double %arg0, 2.0
  %op = fadd double %nnan.arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_fadd_maybe_nan_lhs(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fadd_maybe_nan_lhs(
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG1:%.*]], 1.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fadd double [[NNAN_ARG1]], [[ARG0:%.*]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg1 = fadd nnan double %arg1, 1.0
  %op = fadd double %arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_fadd_maybe_nan_rhs(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fadd_maybe_nan_rhs(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fadd double [[NNAN_ARG0]], [[ARG1:%.*]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %op = fadd double %nnan.arg0, %arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_fmul(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fmul(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG0]], 2.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fmul double [[NNAN_ARG0]], [[NNAN_ARG1]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %nnan.arg1 = fadd nnan double %arg0, 2.0
  %op = fmul double %nnan.arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_fsub(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fsub(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG0]], 2.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fsub double [[NNAN_ARG0]], [[NNAN_ARG1]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %nnan.arg1 = fadd nnan double %arg0, 2.0
  %op = fsub double %nnan.arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

declare double @func()

define i1 @nnan_fneg() {
; CHECK-LABEL: @nnan_fneg(
; CHECK-NEXT:    [[NNAN:%.*]] = call nnan double @func()
; CHECK-NEXT:    ret i1 true
;
  %nnan = call nnan double @func()
  %op = fsub double -0.0, %nnan
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_unary_fneg() {
; CHECK-LABEL: @nnan_unary_fneg(
; CHECK-NEXT:    [[NNAN:%.*]] = call nnan double @func()
; CHECK-NEXT:    ret i1 true
;
  %nnan = call nnan double @func()
  %op = fneg double %nnan
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @fpext_maybe_nan(float %arg0) {
; CHECK-LABEL: @fpext_maybe_nan(
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord float [[ARG0:%.*]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %op = fpext float %arg0 to double
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @fptrunc_maybe_nan(double %arg0) {
; CHECK-LABEL: @fptrunc_maybe_nan(
; CHECK-NEXT:    [[OP:%.*]] = fptrunc double [[ARG0:%.*]] to float
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord float [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %op = fptrunc double %arg0 to float
  %tmp = fcmp ord float %op, %op
  ret i1 %tmp
}

define i1 @nnan_fdiv(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_fdiv(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG0]], 2.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = fdiv double [[NNAN_ARG0]], [[NNAN_ARG1]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %nnan.arg1 = fadd nnan double %arg0, 2.0
  %op = fdiv double %nnan.arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

define i1 @nnan_frem(double %arg0, double %arg1) {
; CHECK-LABEL: @nnan_frem(
; CHECK-NEXT:    [[NNAN_ARG0:%.*]] = fadd nnan double [[ARG0:%.*]], 1.000000e+00
; CHECK-NEXT:    [[NNAN_ARG1:%.*]] = fadd nnan double [[ARG0]], 2.000000e+00
; CHECK-NEXT:    [[OP:%.*]] = frem double [[NNAN_ARG0]], [[NNAN_ARG1]]
; CHECK-NEXT:    [[TMP:%.*]] = fcmp ord double [[OP]], 0.000000e+00
; CHECK-NEXT:    ret i1 [[TMP]]
;
  %nnan.arg0 = fadd nnan double %arg0, 1.0
  %nnan.arg1 = fadd nnan double %arg0, 2.0
  %op = frem double %nnan.arg0, %nnan.arg1
  %tmp = fcmp ord double %op, %op
  ret i1 %tmp
}

declare double @llvm.sqrt.f64(double)
declare double @llvm.fabs.f64(double)
declare double @llvm.canonicalize.f64(double)
declare double @llvm.copysign.f64(double, double)
declare double @llvm.exp.f64(double)
declare double @llvm.exp2.f64(double)
declare double @llvm.floor.f64(double)
declare double @llvm.ceil.f64(double)
declare double @llvm.trunc.f64(double)
declare double @llvm.rint.f64(double)
declare double @llvm.nearbyint.f64(double)
declare double @llvm.round.f64(double)

