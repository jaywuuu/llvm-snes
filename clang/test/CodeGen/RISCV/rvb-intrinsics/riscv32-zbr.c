// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// RUN: %clang_cc1 -no-opaque-pointers -triple riscv32 -target-feature +experimental-zbr -emit-llvm %s -o - \
// RUN:     | FileCheck %s  -check-prefix=RV32ZBR

// RV32ZBR-LABEL: @crc32_b(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32.b.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32_b(long a) {
  return __builtin_riscv_crc32_b(a);
}

// RV32ZBR-LABEL: @crc32_h(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32.h.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32_h(long a) {
  return __builtin_riscv_crc32_h(a);
}

// RV32ZBR-LABEL: @crc32_w(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32.w.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32_w(long a) {
  return __builtin_riscv_crc32_w(a);
}

// RV32ZBR-LABEL: @crc32c_b(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32c.b.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32c_b(long a) {
  return __builtin_riscv_crc32c_b(a);
}

// RV32ZBR-LABEL: @crc32c_h(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32c.h.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32c_h(long a) {
  return __builtin_riscv_crc32c_h(a);
}

// RV32ZBR-LABEL: @crc32c_w(
// RV32ZBR-NEXT:  entry:
// RV32ZBR-NEXT:    [[A_ADDR:%.*]] = alloca i32, align 4
// RV32ZBR-NEXT:    store i32 [[A:%.*]], i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP0:%.*]] = load i32, i32* [[A_ADDR]], align 4
// RV32ZBR-NEXT:    [[TMP1:%.*]] = call i32 @llvm.riscv.crc32c.w.i32(i32 [[TMP0]])
// RV32ZBR-NEXT:    ret i32 [[TMP1]]
//
long crc32c_w(long a) {
  return __builtin_riscv_crc32c_w(a);
}
