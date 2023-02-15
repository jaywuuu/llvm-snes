// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// REQUIRES: systemz-registered-target
// RUN: %clang_cc1 -no-opaque-pointers %s -emit-llvm -ffp-exception-behavior=maytrap -o - -triple s390x-linux-gnu | FileCheck %s

#pragma float_control(except, on)

// CHECK-LABEL: @test_isnan_float(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[F_ADDR:%.*]] = alloca float, align 4
// CHECK-NEXT:    store float [[F:%.*]], float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = load float, float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f32(float [[TMP0]], i64 15) #[[ATTR2:[0-9]+]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isnan_float(float f) {
  return __builtin_isnan(f);
}

// CHECK-LABEL: @test_isnan_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[D_ADDR:%.*]] = alloca double, align 8
// CHECK-NEXT:    store double [[D:%.*]], double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP0:%.*]] = load double, double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f64(double [[TMP0]], i64 15) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isnan_double(double d) {
  return __builtin_isnan(d);
}

// CHECK-LABEL: @test_isnan_long_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[LD_ADDR:%.*]] = alloca fp128, align 8
// CHECK-NEXT:    [[LD:%.*]] = load fp128, fp128* [[TMP0:%.*]], align 8
// CHECK-NEXT:    store fp128 [[LD]], fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = load fp128, fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP2:%.*]] = call i32 @llvm.s390.tdc.f128(fp128 [[TMP1]], i64 15) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP2]]
//
int test_isnan_long_double(long double ld) {
  return __builtin_isnan(ld);
}

// CHECK-LABEL: @test_isinf_float(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[F_ADDR:%.*]] = alloca float, align 4
// CHECK-NEXT:    store float [[F:%.*]], float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = load float, float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f32(float [[TMP0]], i64 48) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isinf_float(float f) {
  return __builtin_isinf(f);
}

// CHECK-LABEL: @test_isinf_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[D_ADDR:%.*]] = alloca double, align 8
// CHECK-NEXT:    store double [[D:%.*]], double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP0:%.*]] = load double, double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f64(double [[TMP0]], i64 48) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isinf_double(double d) {
  return __builtin_isinf(d);
}

// CHECK-LABEL: @test_isinf_long_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[LD_ADDR:%.*]] = alloca fp128, align 8
// CHECK-NEXT:    [[LD:%.*]] = load fp128, fp128* [[TMP0:%.*]], align 8
// CHECK-NEXT:    store fp128 [[LD]], fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = load fp128, fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP2:%.*]] = call i32 @llvm.s390.tdc.f128(fp128 [[TMP1]], i64 48) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP2]]
//
int test_isinf_long_double(long double ld) {
  return __builtin_isinf(ld);
}

// CHECK-LABEL: @test_isfinite_float(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[F_ADDR:%.*]] = alloca float, align 4
// CHECK-NEXT:    store float [[F:%.*]], float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = load float, float* [[F_ADDR]], align 4
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f32(float [[TMP0]], i64 4032) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isfinite_float(float f) {
  return __builtin_isfinite(f);
}

// CHECK-LABEL: @test_isfinite_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[D_ADDR:%.*]] = alloca double, align 8
// CHECK-NEXT:    store double [[D:%.*]], double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP0:%.*]] = load double, double* [[D_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = call i32 @llvm.s390.tdc.f64(double [[TMP0]], i64 4032) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP1]]
//
int test_isfinite_double(double d) {
  return __builtin_isfinite(d);
}

// CHECK-LABEL: @test_isfinite_long_double(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[LD_ADDR:%.*]] = alloca fp128, align 8
// CHECK-NEXT:    [[LD:%.*]] = load fp128, fp128* [[TMP0:%.*]], align 8
// CHECK-NEXT:    store fp128 [[LD]], fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = load fp128, fp128* [[LD_ADDR]], align 8
// CHECK-NEXT:    [[TMP2:%.*]] = call i32 @llvm.s390.tdc.f128(fp128 [[TMP1]], i64 4032) #[[ATTR2]]
// CHECK-NEXT:    ret i32 [[TMP2]]
//
int test_isfinite_long_double(long double ld) {
  return __builtin_isfinite(ld);
}

