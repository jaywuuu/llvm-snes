; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -passes=vector-combine -S %s | FileCheck %s

target triple = "arm64-apple-darwin"

define void @load_extract_insert_store_const_idx(<225 x double>* %A) {
; CHECK-LABEL: @load_extract_insert_store_const_idx(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[TMP0:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A:%.*]], i32 0, i64 0
; CHECK-NEXT:    [[EXT_0:%.*]] = load double, double* [[TMP0]], align 8
; CHECK-NEXT:    [[MUL:%.*]] = fmul double 2.000000e+01, [[EXT_0]]
; CHECK-NEXT:    [[TMP1:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i32 0, i64 1
; CHECK-NEXT:    [[EXT_1:%.*]] = load double, double* [[TMP1]], align 8
; CHECK-NEXT:    [[SUB:%.*]] = fsub double [[EXT_1]], [[MUL]]
; CHECK-NEXT:    [[TMP2:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i64 0, i64 1
; CHECK-NEXT:    store double [[SUB]], double* [[TMP2]], align 8
; CHECK-NEXT:    ret void
;
entry:
  %lv = load <225 x double>, <225 x double>* %A, align 8
  %ext.0 = extractelement <225 x double> %lv, i64 0
  %mul = fmul double 20.0, %ext.0
  %ext.1 = extractelement <225 x double> %lv, i64 1
  %sub = fsub double %ext.1, %mul
  %ins = insertelement <225 x double> %lv, double %sub, i64 1
  store <225 x double> %ins, <225 x double>* %A, align 8
  ret void
}

define void @load_extract_insert_store_var_idx_assume_valid(i64 %idx.1, i64 %idx.2, <225 x double>* %A) {
; CHECK-LABEL: @load_extract_insert_store_var_idx_assume_valid(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP_1:%.*]] = icmp ult i64 [[IDX_1:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_1]])
; CHECK-NEXT:    [[CMP_2:%.*]] = icmp ult i64 [[IDX_2:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_2]])
; CHECK-NEXT:    [[TMP0:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A:%.*]], i32 0, i64 [[IDX_1]]
; CHECK-NEXT:    [[EXT_0:%.*]] = load double, double* [[TMP0]], align 8
; CHECK-NEXT:    [[MUL:%.*]] = fmul double 2.000000e+01, [[EXT_0]]
; CHECK-NEXT:    [[TMP1:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i32 0, i64 [[IDX_2]]
; CHECK-NEXT:    [[EXT_1:%.*]] = load double, double* [[TMP1]], align 8
; CHECK-NEXT:    [[SUB:%.*]] = fsub double [[EXT_1]], [[MUL]]
; CHECK-NEXT:    [[TMP2:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i64 0, i64 [[IDX_1]]
; CHECK-NEXT:    store double [[SUB]], double* [[TMP2]], align 8
; CHECK-NEXT:    ret void
;
entry:
  %cmp.1 = icmp ult i64 %idx.1, 225
  call void @llvm.assume(i1 %cmp.1)
  %cmp.2 = icmp ult i64 %idx.2, 225
  call void @llvm.assume(i1 %cmp.2)

  %lv = load <225 x double>, <225 x double>* %A, align 8
  %ext.0 = extractelement <225 x double> %lv, i64 %idx.1
  %mul = fmul double 20.0, %ext.0
  %ext.1 = extractelement <225 x double> %lv, i64 %idx.2
  %sub = fsub double %ext.1, %mul
  %ins = insertelement <225 x double> %lv, double %sub, i64 %idx.1
  store <225 x double> %ins, <225 x double>* %A, align 8
  ret void
}

declare i1 @cond()

define void @load_extract_insert_store_var_idx_assume_valid_in_dominating_block(i64 %idx.1, i64 %idx.2, <225 x double>* %A, i1 %c.1) {
; CHECK-LABEL: @load_extract_insert_store_var_idx_assume_valid_in_dominating_block(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[CMP_1:%.*]] = icmp ult i64 [[IDX_1:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_1]])
; CHECK-NEXT:    [[CMP_2:%.*]] = icmp ult i64 [[IDX_2:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_2]])
; CHECK-NEXT:    br i1 [[C_1:%.*]], label [[LOOP:%.*]], label [[EXIT:%.*]]
; CHECK:       loop:
; CHECK-NEXT:    [[TMP0:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A:%.*]], i32 0, i64 [[IDX_1]]
; CHECK-NEXT:    [[EXT_0:%.*]] = load double, double* [[TMP0]], align 8
; CHECK-NEXT:    [[MUL:%.*]] = fmul double 2.000000e+01, [[EXT_0]]
; CHECK-NEXT:    [[TMP1:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i32 0, i64 [[IDX_2]]
; CHECK-NEXT:    [[EXT_1:%.*]] = load double, double* [[TMP1]], align 8
; CHECK-NEXT:    [[SUB:%.*]] = fsub double [[EXT_1]], [[MUL]]
; CHECK-NEXT:    [[TMP2:%.*]] = getelementptr inbounds <225 x double>, <225 x double>* [[A]], i64 0, i64 [[IDX_1]]
; CHECK-NEXT:    store double [[SUB]], double* [[TMP2]], align 8
; CHECK-NEXT:    [[C_2:%.*]] = call i1 @cond()
; CHECK-NEXT:    br i1 [[C_2]], label [[LOOP]], label [[EXIT]]
; CHECK:       exit:
; CHECK-NEXT:    ret void
;
entry:
  %cmp.1 = icmp ult i64 %idx.1, 225
  call void @llvm.assume(i1 %cmp.1)
  %cmp.2 = icmp ult i64 %idx.2, 225
  call void @llvm.assume(i1 %cmp.2)
  br i1 %c.1, label %loop, label %exit

loop:
  %lv = load <225 x double>, <225 x double>* %A, align 8
  %ext.0 = extractelement <225 x double> %lv, i64 %idx.1
  %mul = fmul double 20.0, %ext.0
  %ext.1 = extractelement <225 x double> %lv, i64 %idx.2
  %sub = fsub double %ext.1, %mul
  %ins = insertelement <225 x double> %lv, double %sub, i64 %idx.1
  store <225 x double> %ins, <225 x double>* %A, align 8
  %c.2 = call i1 @cond()
  br i1 %c.2, label %loop, label %exit

exit:
  ret void
}

define void @load_extract_insert_store_var_idx_assume_valid_in_non_dominating_block(i64 %idx.1, i64 %idx.2, <225 x double>* %A, i1 %c.1, i1 %c.2) {
; CHECK-LABEL: @load_extract_insert_store_var_idx_assume_valid_in_non_dominating_block(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br i1 [[C_1:%.*]], label [[ASSUME_BLOCK:%.*]], label [[LOOP:%.*]]
; CHECK:       assume_block:
; CHECK-NEXT:    [[CMP_1:%.*]] = icmp ult i64 [[IDX_1:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_1]])
; CHECK-NEXT:    [[CMP_2:%.*]] = icmp ult i64 [[IDX_2:%.*]], 225
; CHECK-NEXT:    call void @llvm.assume(i1 [[CMP_2]])
; CHECK-NEXT:    br i1 [[C_2:%.*]], label [[LOOP]], label [[EXIT:%.*]]
; CHECK:       loop:
; CHECK-NEXT:    [[LV:%.*]] = load <225 x double>, <225 x double>* [[A:%.*]], align 8
; CHECK-NEXT:    [[EXT_0:%.*]] = extractelement <225 x double> [[LV]], i64 [[IDX_1]]
; CHECK-NEXT:    [[MUL:%.*]] = fmul double 2.000000e+01, [[EXT_0]]
; CHECK-NEXT:    [[EXT_1:%.*]] = extractelement <225 x double> [[LV]], i64 [[IDX_2]]
; CHECK-NEXT:    [[SUB:%.*]] = fsub double [[EXT_1]], [[MUL]]
; CHECK-NEXT:    [[INS:%.*]] = insertelement <225 x double> [[LV]], double [[SUB]], i64 [[IDX_1]]
; CHECK-NEXT:    store <225 x double> [[INS]], <225 x double>* [[A]], align 8
; CHECK-NEXT:    [[C_3:%.*]] = call i1 @cond()
; CHECK-NEXT:    br i1 [[C_3]], label [[LOOP]], label [[EXIT]]
; CHECK:       exit:
; CHECK-NEXT:    ret void
;
entry:
  br i1 %c.1, label %assume_block, label %loop

assume_block:
  %cmp.1 = icmp ult i64 %idx.1, 225
  call void @llvm.assume(i1 %cmp.1)
  %cmp.2 = icmp ult i64 %idx.2, 225
  call void @llvm.assume(i1 %cmp.2)
  br i1 %c.2, label %loop, label %exit

loop:
  %lv = load <225 x double>, <225 x double>* %A, align 8
  %ext.0 = extractelement <225 x double> %lv, i64 %idx.1
  %mul = fmul double 20.0, %ext.0
  %ext.1 = extractelement <225 x double> %lv, i64 %idx.2
  %sub = fsub double %ext.1, %mul
  %ins = insertelement <225 x double> %lv, double %sub, i64 %idx.1
  store <225 x double> %ins, <225 x double>* %A, align 8
  %c.3 = call i1 @cond()
  br i1 %c.3, label %loop, label %exit

exit:
  ret void
}

define void @load_extract_insert_store_var_idx_no_assume_valid(i64 %idx.1, i64 %idx.2, <225 x double>* %A) {
; CHECK-LABEL: @load_extract_insert_store_var_idx_no_assume_valid(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[LV:%.*]] = load <225 x double>, <225 x double>* [[A:%.*]], align 8
; CHECK-NEXT:    [[EXT_0:%.*]] = extractelement <225 x double> [[LV]], i64 [[IDX_1:%.*]]
; CHECK-NEXT:    [[MUL:%.*]] = fmul double 2.000000e+01, [[EXT_0]]
; CHECK-NEXT:    [[EXT_1:%.*]] = extractelement <225 x double> [[LV]], i64 [[IDX_2:%.*]]
; CHECK-NEXT:    [[SUB:%.*]] = fsub double [[EXT_1]], [[MUL]]
; CHECK-NEXT:    [[INS:%.*]] = insertelement <225 x double> [[LV]], double [[SUB]], i64 [[IDX_1]]
; CHECK-NEXT:    store <225 x double> [[INS]], <225 x double>* [[A]], align 8
; CHECK-NEXT:    ret void
;
entry:
  %lv = load <225 x double>, <225 x double>* %A, align 8
  %ext.0 = extractelement <225 x double> %lv, i64 %idx.1
  %mul = fmul double 20.0, %ext.0
  %ext.1 = extractelement <225 x double> %lv, i64 %idx.2
  %sub = fsub double %ext.1, %mul
  %ins = insertelement <225 x double> %lv, double %sub, i64 %idx.1
  store <225 x double> %ins, <225 x double>* %A, align 8
  ret void
}

declare void @llvm.assume(i1)
