; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -O3 -S | FileCheck %s

; Compile time conversions of NaNs.

target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128"
target triple = "i686-apple-darwin8"

%struct..0anon = type { float }
%struct..1anon = type { double }

@fnan = constant [3 x i32] [ i32 2143831397, i32 2143831396, i32 2143831398 ]
@dnan = constant [3 x i64] [ i64 9223235251041752696, i64 9223235251041752697, i64 9223235250773317239 ], align 8
@fsnan = constant [3 x i32] [ i32 2139637093, i32 2139637092, i32 2139637094 ]
@dsnan = constant [3 x i64] [ i64 9220983451228067448, i64 9220983451228067449, i64 9220983450959631991 ], align 8
@.str = internal constant [10 x i8] c"%08x%08x\0A\00"
@.str1 = internal constant [6 x i8] c"%08x\0A\00"

@var = external global i32

; SNAN becomes QNAN on fptrunc:
; 2147228864 = 0x7ffc1cc0 : QNAN

define i32 @main() {
; CHECK-LABEL: @main(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -1610612736, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -2147483648, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -1073741824, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -1610612736, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -2147483648, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147027116, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 -1073741824, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    store volatile i32 2147228864, i32* @var, align 4
; CHECK-NEXT:    ret i32 undef
;
entry:
  %retval = alloca i32, align 4
  %i = alloca i32, align 4
  %uf = alloca %struct..0anon, align 4
  %ud = alloca %struct..1anon, align 8
  %"alloca point" = bitcast i32 0 to i32
  store i32 0, i32* %i, align 4
  br label %bb23

bb:		; preds = %bb23
  %t = load i32, i32* %i, align 4
  %t1 = getelementptr [3 x i32], [3 x i32]* @fnan, i32 0, i32 %t
  %t2 = load i32, i32* %t1, align 4
  %t3 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t34 = bitcast float* %t3 to i32*
  store i32 %t2, i32* %t34, align 4
  %t5 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t6 = load float, float* %t5, align 4
  %t67 = fpext float %t6 to double
  %t8 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  store double %t67, double* %t8, align 8
  %t9 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t910 = bitcast double* %t9 to i64*
  %t11 = load i64, i64* %t910, align 8
  %t1112 = trunc i64 %t11 to i32
  %t13 = and i32 %t1112, -1
  %t14 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t1415 = bitcast double* %t14 to i64*
  %t16 = load i64, i64* %t1415, align 8
  %.cast = zext i32 32 to i64
  %t17 = ashr i64 %t16, %.cast
  %t1718 = trunc i64 %t17 to i32
  %t19 = getelementptr [10 x i8], [10 x i8]* @.str, i32 0, i32 0
  store volatile i32 %t1718, i32* @var
  store volatile i32 %t13, i32* @var
  %t21 = load i32, i32* %i, align 4
  %t22 = add i32 %t21, 1
  store i32 %t22, i32* %i, align 4
  br label %bb23

bb23:		; preds = %bb, %entry
  %t24 = load i32, i32* %i, align 4
  %t25 = icmp sle i32 %t24, 2
  %t2526 = zext i1 %t25 to i8
  %toBool = icmp ne i8 %t2526, 0
  br i1 %toBool, label %bb, label %bb27

bb27:		; preds = %bb23
  store i32 0, i32* %i, align 4
  br label %bb46

bb28:		; preds = %bb46
  %t29 = load i32, i32* %i, align 4
  %t30 = getelementptr [3 x i64], [3 x i64]* @dnan, i32 0, i32 %t29
  %t31 = load i64, i64* %t30, align 8
  %t32 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t3233 = bitcast double* %t32 to i64*
  store i64 %t31, i64* %t3233, align 8
  %t35 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t36 = load double, double* %t35, align 8
  %t3637 = fptrunc double %t36 to float
  %t38 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  store float %t3637, float* %t38, align 4
  %t39 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t3940 = bitcast float* %t39 to i32*
  %t41 = load i32, i32* %t3940, align 4
  %t42 = getelementptr [6 x i8], [6 x i8]* @.str1, i32 0, i32 0
  store volatile i32 %t41, i32* @var
  %t44 = load i32, i32* %i, align 4
  %t45 = add i32 %t44, 1
  store i32 %t45, i32* %i, align 4
  br label %bb46

bb46:		; preds = %bb28, %bb27
  %t47 = load i32, i32* %i, align 4
  %t48 = icmp sle i32 %t47, 2
  %t4849 = zext i1 %t48 to i8
  %toBool50 = icmp ne i8 %t4849, 0
  br i1 %toBool50, label %bb28, label %bb51

bb51:		; preds = %bb46
  store i32 0, i32* %i, align 4
  br label %bb78

bb52:		; preds = %bb78
  %t53 = load i32, i32* %i, align 4
  %t54 = getelementptr [3 x i32], [3 x i32]* @fsnan, i32 0, i32 %t53
  %t55 = load i32, i32* %t54, align 4
  %t56 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t5657 = bitcast float* %t56 to i32*
  store i32 %t55, i32* %t5657, align 4
  %t58 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t59 = load float, float* %t58, align 4
  %t5960 = fpext float %t59 to double
  %t61 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  store double %t5960, double* %t61, align 8
  %t62 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t6263 = bitcast double* %t62 to i64*
  %t64 = load i64, i64* %t6263, align 8
  %t6465 = trunc i64 %t64 to i32
  %t66 = and i32 %t6465, -1
  %t68 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t6869 = bitcast double* %t68 to i64*
  %t70 = load i64, i64* %t6869, align 8
  %.cast71 = zext i32 32 to i64
  %t72 = ashr i64 %t70, %.cast71
  %t7273 = trunc i64 %t72 to i32
  %t74 = getelementptr [10 x i8], [10 x i8]* @.str, i32 0, i32 0
  store volatile i32 %t7273, i32* @var
  store volatile i32 %t66, i32* @var
  %t76 = load i32, i32* %i, align 4
  %t77 = add i32 %t76, 1
  store i32 %t77, i32* %i, align 4
  br label %bb78

bb78:		; preds = %bb52, %bb51
  %t79 = load i32, i32* %i, align 4
  %t80 = icmp sle i32 %t79, 2
  %t8081 = zext i1 %t80 to i8
  %toBool82 = icmp ne i8 %t8081, 0
  br i1 %toBool82, label %bb52, label %bb83

bb83:		; preds = %bb78
  store i32 0, i32* %i, align 4
  br label %bb101

bb84:		; preds = %bb101
  %t85 = load i32, i32* %i, align 4
  %t86 = getelementptr [3 x i64], [3 x i64]* @dsnan, i32 0, i32 %t85
  %t87 = load i64, i64* %t86, align 8
  %t88 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t8889 = bitcast double* %t88 to i64*
  store i64 %t87, i64* %t8889, align 8
  %t90 = getelementptr %struct..1anon, %struct..1anon* %ud, i32 0, i32 0
  %t91 = load double, double* %t90, align 8
  %t9192 = fptrunc double %t91 to float
  %t93 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  store float %t9192, float* %t93, align 4
  %t94 = getelementptr %struct..0anon, %struct..0anon* %uf, i32 0, i32 0
  %t9495 = bitcast float* %t94 to i32*
  %t96 = load i32, i32* %t9495, align 4
  %t97 = getelementptr [6 x i8], [6 x i8]* @.str1, i32 0, i32 0
  store volatile i32 %t96, i32* @var
  %t99 = load i32, i32* %i, align 4
  %t100 = add i32 %t99, 1
  store i32 %t100, i32* %i, align 4
  br label %bb101

bb101:		; preds = %bb84, %bb83
  %t102 = load i32, i32* %i, align 4
  %t103 = icmp sle i32 %t102, 2
  %t103104 = zext i1 %t103 to i8
  %toBool105 = icmp ne i8 %t103104, 0
  br i1 %toBool105, label %bb84, label %bb106

bb106:		; preds = %bb101
  br label %return

return:		; preds = %bb106
  %retval107 = load i32, i32* %retval
  ret i32 %retval107
}
