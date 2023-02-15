// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// RUN: %clang_cc1 -no-opaque-pointers -triple riscv64 -target-feature +zknh -emit-llvm %s -o - \
// RUN:     | FileCheck %s  -check-prefix=RV64ZKNH


// RV64ZKNH-LABEL: @sha512sig0(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKNH-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha512sig0(i64 [[CONV]])
// RV64ZKNH-NEXT:    [[CONV1:%.*]] = trunc i64 [[TMP1]] to i32
// RV64ZKNH-NEXT:    ret i32 [[CONV1]]
//
int sha512sig0(int rs1) {
  return __builtin_riscv_sha512sig0_64(rs1);
}


// RV64ZKNH-LABEL: @sha512sig1(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKNH-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha512sig1(i64 [[CONV]])
// RV64ZKNH-NEXT:    [[CONV1:%.*]] = trunc i64 [[TMP1]] to i32
// RV64ZKNH-NEXT:    ret i32 [[CONV1]]
//
int sha512sig1(int rs1) {
  return __builtin_riscv_sha512sig1_64(rs1);
}


// RV64ZKNH-LABEL: @sha512sum0(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKNH-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha512sum0(i64 [[CONV]])
// RV64ZKNH-NEXT:    [[CONV1:%.*]] = trunc i64 [[TMP1]] to i32
// RV64ZKNH-NEXT:    ret i32 [[CONV1]]
//
int sha512sum0(int rs1) {
  return __builtin_riscv_sha512sum0_64(rs1);
}


// RV64ZKNH-LABEL: @sha512sum1(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i32, align 4
// RV64ZKNH-NEXT:    store i32 [[RS1:%.*]], i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i32, i32* [[RS1_ADDR]], align 4
// RV64ZKNH-NEXT:    [[CONV:%.*]] = sext i32 [[TMP0]] to i64
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha512sum1(i64 [[CONV]])
// RV64ZKNH-NEXT:    [[CONV1:%.*]] = trunc i64 [[TMP1]] to i32
// RV64ZKNH-NEXT:    ret i32 [[CONV1]]
//
int sha512sum1(int rs1) {
  return __builtin_riscv_sha512sum1_64(rs1);
}


// RV64ZKNH-LABEL: @sha256sig0(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i64, align 8
// RV64ZKNH-NEXT:    store i64 [[RS1:%.*]], i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i64, i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha256sig0.i64(i64 [[TMP0]])
// RV64ZKNH-NEXT:    ret i64 [[TMP1]]
//
long sha256sig0(long rs1) {
  return __builtin_riscv_sha256sig0(rs1);
}

// RV64ZKNH-LABEL: @sha256sig1(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i64, align 8
// RV64ZKNH-NEXT:    store i64 [[RS1:%.*]], i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i64, i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha256sig1.i64(i64 [[TMP0]])
// RV64ZKNH-NEXT:    ret i64 [[TMP1]]
//
long sha256sig1(long rs1) {
  return __builtin_riscv_sha256sig1(rs1);
}


// RV64ZKNH-LABEL: @sha256sum0(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i64, align 8
// RV64ZKNH-NEXT:    store i64 [[RS1:%.*]], i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i64, i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha256sum0.i64(i64 [[TMP0]])
// RV64ZKNH-NEXT:    ret i64 [[TMP1]]
//
long sha256sum0(long rs1) {
  return __builtin_riscv_sha256sum0(rs1);
}

// RV64ZKNH-LABEL: @sha256sum1(
// RV64ZKNH-NEXT:  entry:
// RV64ZKNH-NEXT:    [[RS1_ADDR:%.*]] = alloca i64, align 8
// RV64ZKNH-NEXT:    store i64 [[RS1:%.*]], i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP0:%.*]] = load i64, i64* [[RS1_ADDR]], align 8
// RV64ZKNH-NEXT:    [[TMP1:%.*]] = call i64 @llvm.riscv.sha256sum1.i64(i64 [[TMP0]])
// RV64ZKNH-NEXT:    ret i64 [[TMP1]]
//
long sha256sum1(long rs1) {
  return __builtin_riscv_sha256sum1(rs1);
}
