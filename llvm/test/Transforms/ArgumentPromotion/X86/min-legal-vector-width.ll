; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --function-signature --scrub-attributes
; RUN: opt -S -passes=argpromotion < %s | FileCheck %s
; Test that we only promote arguments when the caller/callee have compatible
; function attrubtes.

target triple = "x86_64-unknown-linux-gnu"

; This should promote
define internal fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer512(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #0 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal512_prefer512_call_avx512_legal512_prefer512
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal512_prefer512_call_avx512_legal512_prefer512(<8 x i64>* %arg) #0 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal512_prefer512_call_avx512_legal512_prefer512
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer512(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer512(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should promote
define internal fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #1 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal512_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal512_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %arg) #1 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal512_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer256(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should promote
define internal fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #1 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal512_prefer512_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal512_prefer512_call_avx512_legal512_prefer256(<8 x i64>* %arg) #0 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal512_prefer512_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer256(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal512_prefer512_call_avx512_legal512_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should promote
define internal fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer512(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #0 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal512_prefer256_call_avx512_legal512_prefer512
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal512_prefer256_call_avx512_legal512_prefer512(<8 x i64>* %arg) #1 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal512_prefer256_call_avx512_legal512_prefer512
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer512(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal512_prefer512(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should not promote
define internal fastcc void @callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #1 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64>* readonly [[ARG1:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = load <8 x i64>, <8 x i64>* [[ARG1]]
; CHECK-NEXT:    store <8 x i64> [[TMP]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal256_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %arg) #2 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal256_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    call fastcc void @callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(<8 x i64>* [[TMP2]], <8 x i64>* [[TMP]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should not promote
define internal fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #2 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64>* readonly [[ARG1:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = load <8 x i64>, <8 x i64>* [[ARG1]]
; CHECK-NEXT:    store <8 x i64> [[TMP]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx512_legal512_prefer256_call_avx512_legal256_prefer256(<8 x i64>* %arg) #1 {
; CHECK-LABEL: define {{[^@]+}}@avx512_legal512_prefer256_call_avx512_legal256_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(<8 x i64>* [[TMP2]], <8 x i64>* [[TMP]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should promote
define internal fastcc void @callee_avx2_legal256_prefer256_call_avx2_legal512_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #3 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx2_legal256_prefer256_call_avx2_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx2_legal256_prefer256_call_avx2_legal512_prefer256(<8 x i64>* %arg) #4 {
; CHECK-LABEL: define {{[^@]+}}@avx2_legal256_prefer256_call_avx2_legal512_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx2_legal256_prefer256_call_avx2_legal512_prefer256(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx2_legal256_prefer256_call_avx2_legal512_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; This should promote
define internal fastcc void @callee_avx2_legal512_prefer256_call_avx2_legal256_prefer256(<8 x i64>* %arg, <8 x i64>* readonly %arg1) #4 {
; CHECK-LABEL: define {{[^@]+}}@callee_avx2_legal512_prefer256_call_avx2_legal256_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]], <8 x i64> [[ARG1_VAL:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    store <8 x i64> [[ARG1_VAL]], <8 x i64>* [[ARG]]
; CHECK-NEXT:    ret void
;
bb:
  %tmp = load <8 x i64>, <8 x i64>* %arg1
  store <8 x i64> %tmp, <8 x i64>* %arg
  ret void
}

define void @avx2_legal512_prefer256_call_avx2_legal256_prefer256(<8 x i64>* %arg) #3 {
; CHECK-LABEL: define {{[^@]+}}@avx2_legal512_prefer256_call_avx2_legal256_prefer256
; CHECK-SAME: (<8 x i64>* [[ARG:%.*]])
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[TMP:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP2:%.*]] = alloca <8 x i64>, align 32
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast <8 x i64>* [[TMP]] to i8*
; CHECK-NEXT:    call void @llvm.memset.p0i8.i64(i8* align 32 [[TMP3]], i8 0, i64 32, i1 false)
; CHECK-NEXT:    [[TMP_VAL:%.*]] = load <8 x i64>, <8 x i64>* [[TMP]]
; CHECK-NEXT:    call fastcc void @callee_avx2_legal512_prefer256_call_avx2_legal256_prefer256(<8 x i64>* [[TMP2]], <8 x i64> [[TMP_VAL]])
; CHECK-NEXT:    [[TMP4:%.*]] = load <8 x i64>, <8 x i64>* [[TMP2]], align 32
; CHECK-NEXT:    store <8 x i64> [[TMP4]], <8 x i64>* [[ARG]], align 2
; CHECK-NEXT:    ret void
;
bb:
  %tmp = alloca <8 x i64>, align 32
  %tmp2 = alloca <8 x i64>, align 32
  %tmp3 = bitcast <8 x i64>* %tmp to i8*
  call void @llvm.memset.p0i8.i64(i8* align 32 %tmp3, i8 0, i64 32, i1 false)
  call fastcc void @callee_avx2_legal512_prefer256_call_avx2_legal256_prefer256(<8 x i64>* %tmp2, <8 x i64>* %tmp)
  %tmp4 = load <8 x i64>, <8 x i64>* %tmp2, align 32
  store <8 x i64> %tmp4, <8 x i64>* %arg, align 2
  ret void
}

; If the arguments are scalar, its ok to promote.
define internal i32 @scalar_callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(i32* %X, i32* %Y) #2 {
; CHECK-LABEL: define {{[^@]+}}@scalar_callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (i32 [[X_VAL:%.*]], i32 [[Y_VAL:%.*]])
; CHECK-NEXT:    [[C:%.*]] = add i32 [[X_VAL]], [[Y_VAL]]
; CHECK-NEXT:    ret i32 [[C]]
;
  %A = load i32, i32* %X
  %B = load i32, i32* %Y
  %C = add i32 %A, %B
  ret i32 %C
}

define i32 @scalar_avx512_legal256_prefer256_call_avx512_legal512_prefer256(i32* %B) #2 {
; CHECK-LABEL: define {{[^@]+}}@scalar_avx512_legal256_prefer256_call_avx512_legal512_prefer256
; CHECK-SAME: (i32* [[B:%.*]])
; CHECK-NEXT:    [[A:%.*]] = alloca i32
; CHECK-NEXT:    store i32 1, i32* [[A]]
; CHECK-NEXT:    [[A_VAL:%.*]] = load i32, i32* [[A]]
; CHECK-NEXT:    [[B_VAL:%.*]] = load i32, i32* [[B]]
; CHECK-NEXT:    [[C:%.*]] = call i32 @scalar_callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(i32 [[A_VAL]], i32 [[B_VAL]])
; CHECK-NEXT:    ret i32 [[C]]
;
  %A = alloca i32
  store i32 1, i32* %A
  %C = call i32 @scalar_callee_avx512_legal256_prefer256_call_avx512_legal512_prefer256(i32* %A, i32* %B)
  ret i32 %C
}

; If the arguments are scalar, its ok to promote.
define internal i32 @scalar_callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(i32* %X, i32* %Y) #2 {
; CHECK-LABEL: define {{[^@]+}}@scalar_callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256
; CHECK-SAME: (i32 [[X_VAL:%.*]], i32 [[Y_VAL:%.*]])
; CHECK-NEXT:    [[C:%.*]] = add i32 [[X_VAL]], [[Y_VAL]]
; CHECK-NEXT:    ret i32 [[C]]
;
  %A = load i32, i32* %X
  %B = load i32, i32* %Y
  %C = add i32 %A, %B
  ret i32 %C
}

define i32 @scalar_avx512_legal512_prefer256_call_avx512_legal256_prefer256(i32* %B) #2 {
; CHECK-LABEL: define {{[^@]+}}@scalar_avx512_legal512_prefer256_call_avx512_legal256_prefer256
; CHECK-SAME: (i32* [[B:%.*]])
; CHECK-NEXT:    [[A:%.*]] = alloca i32
; CHECK-NEXT:    store i32 1, i32* [[A]]
; CHECK-NEXT:    [[A_VAL:%.*]] = load i32, i32* [[A]]
; CHECK-NEXT:    [[B_VAL:%.*]] = load i32, i32* [[B]]
; CHECK-NEXT:    [[C:%.*]] = call i32 @scalar_callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(i32 [[A_VAL]], i32 [[B_VAL]])
; CHECK-NEXT:    ret i32 [[C]]
;
  %A = alloca i32
  store i32 1, i32* %A
  %C = call i32 @scalar_callee_avx512_legal512_prefer256_call_avx512_legal256_prefer256(i32* %A, i32* %B)
  ret i32 %C
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1) #5

attributes #0 = { inlinehint norecurse nounwind uwtable "target-features"="+avx512vl" "min-legal-vector-width"="512" "prefer-vector-width"="512" }
attributes #1 = { inlinehint norecurse nounwind uwtable "target-features"="+avx512vl" "min-legal-vector-width"="512" "prefer-vector-width"="256" }
attributes #2 = { inlinehint norecurse nounwind uwtable "target-features"="+avx512vl" "min-legal-vector-width"="256" "prefer-vector-width"="256" }
attributes #3 = { inlinehint norecurse nounwind uwtable "target-features"="+avx2" "min-legal-vector-width"="512" "prefer-vector-width"="256" }
attributes #4 = { inlinehint norecurse nounwind uwtable "target-features"="+avx2" "min-legal-vector-width"="256" "prefer-vector-width"="256" }
attributes #5 = { argmemonly nounwind }
