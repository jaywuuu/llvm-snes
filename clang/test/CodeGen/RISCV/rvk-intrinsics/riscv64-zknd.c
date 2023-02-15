// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// RUN: %clang_cc1 -no-opaque-pointers -triple riscv64 -target-feature +zknd -emit-llvm %s -o - \
// RUN:     | FileCheck %s  -check-prefix=RV64ZKND


// RV64ZKND-LABEL: @aes64dsm(
// RV64ZKND-NEXT:  entry:
// RV64ZKND-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKND-NEXT:    [[RS2_ADDR:%.*]] = alloca i32, align 4
// RV64ZKND-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    store i32 [[RS2:%.*]], i32* [[RS2_ADDR]], align 4
// RV64ZKND-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKND-NEXT:    [[TMP1:%.*]] = load i32, i32* [[RS2_ADDR]], align 4
// RV64ZKND-NEXT:    [[CONV1:%.*]] = sext i32 [[TMP1]] to i64
// RV64ZKND-NEXT:    [[TMP2:%.*]] = call i64 @llvm.riscv.aes64dsm(i64 [[CONV]], i64 [[CONV1]])
// RV64ZKND-NEXT:    [[CONV2:%.*]] = trunc i64 [[TMP2]] to i32
// RV64ZKND-NEXT:    ret i32 [[CONV2]]
//
int aes64dsm(int rs1, int rs2) {
  return __builtin_riscv_aes64dsm_64(rs1, rs2);
}


// RV64ZKND-LABEL: @aes64ds(
// RV64ZKND-NEXT:  entry:
// RV64ZKND-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKND-NEXT:    [[RS2_ADDR:%.*]] = alloca i32, align 4
// RV64ZKND-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    store i32 [[RS2:%.*]], i32* [[RS2_ADDR]], align 4
// RV64ZKND-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKND-NEXT:    [[TMP1:%.*]] = load i32, i32* [[RS2_ADDR]], align 4
// RV64ZKND-NEXT:    [[CONV1:%.*]] = sext i32 [[TMP1]] to i64
// RV64ZKND-NEXT:    [[TMP2:%.*]] = call i64 @llvm.riscv.aes64ds(i64 [[CONV]], i64 [[CONV1]])
// RV64ZKND-NEXT:    [[CONV2:%.*]] = trunc i64 [[TMP2]] to i32
// RV64ZKND-NEXT:    ret i32 [[CONV2]]
//
int aes64ds(int rs1, int rs2) {
  return __builtin_riscv_aes64ds_64(rs1, rs2);
}


// RV64ZKND-LABEL: @aes64im(
// RV64ZKND-NEXT:  entry:
// RV64ZKND-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKND-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKND-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKND-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.aes64im(i64 [[CONV]])
// RV64ZKND-NEXT:    [[CONV1:%.*]] = trunc i64 [[TMP1]] to i32
// RV64ZKND-NEXT:    ret i32 [[CONV1]]
//
int aes64im(int rs1) {
  return __builtin_riscv_aes64im_64(rs1);
}
