// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// REQUIRES: aarch64-registered-target
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - -x c++ %s | FileCheck %s -check-prefix=CPP-CHECK
// RUN: %clang_cc1 -DSVE_OVERLOADED_FORMS -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -DSVE_OVERLOADED_FORMS -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - -x c++ %s | FileCheck %s -check-prefix=CPP-CHECK
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -o /dev/null %s
#include <arm_sve.h>

#ifdef SVE_OVERLOADED_FORMS
// A simple used,unused... macro, long enough to represent any SVE builtin.
#define SVE_ACLE_FUNC(A1,A2_UNUSED,A3,A4_UNUSED) A1##A3
#else
#define SVE_ACLE_FUNC(A1,A2,A3,A4) A1##A2##A3##A4
#endif

// NOTE: For these tests clang converts the struct parameter into
// several parameters, one for each member of the original struct.
// CHECK-LABEL: @test_svget4_s8(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i8> @llvm.aarch64.sve.tuple.get.nxv16i8.nxv64i8(<vscale x 64 x i8> [[TUPLE:%.*]], i32 0)
// CHECK-NEXT:    ret <vscale x 16 x i8> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z14test_svget4_s810svint8x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i8> @llvm.aarch64.sve.tuple.get.nxv16i8.nxv64i8(<vscale x 64 x i8> [[TUPLE:%.*]], i32 0)
// CPP-CHECK-NEXT:    ret <vscale x 16 x i8> [[TMP0]]
//
svint8_t test_svget4_s8(svint8x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_s8,,)(tuple, 0);
}

// CHECK-LABEL: @test_svget4_s16(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i16> @llvm.aarch64.sve.tuple.get.nxv8i16.nxv32i16(<vscale x 32 x i16> [[TUPLE:%.*]], i32 2)
// CHECK-NEXT:    ret <vscale x 8 x i16> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_s1611svint16x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i16> @llvm.aarch64.sve.tuple.get.nxv8i16.nxv32i16(<vscale x 32 x i16> [[TUPLE:%.*]], i32 2)
// CPP-CHECK-NEXT:    ret <vscale x 8 x i16> [[TMP0]]
//
svint16_t test_svget4_s16(svint16x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_s16,,)(tuple, 2);
}

// CHECK-LABEL: @test_svget4_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i32> @llvm.aarch64.sve.tuple.get.nxv4i32.nxv16i32(<vscale x 16 x i32> [[TUPLE:%.*]], i32 2)
// CHECK-NEXT:    ret <vscale x 4 x i32> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_s3211svint32x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i32> @llvm.aarch64.sve.tuple.get.nxv4i32.nxv16i32(<vscale x 16 x i32> [[TUPLE:%.*]], i32 2)
// CPP-CHECK-NEXT:    ret <vscale x 4 x i32> [[TMP0]]
//
svint32_t test_svget4_s32(svint32x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_s32,,)(tuple, 2);
}

// CHECK-LABEL: @test_svget4_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i64> @llvm.aarch64.sve.tuple.get.nxv2i64.nxv8i64(<vscale x 8 x i64> [[TUPLE:%.*]], i32 3)
// CHECK-NEXT:    ret <vscale x 2 x i64> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_s6411svint64x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i64> @llvm.aarch64.sve.tuple.get.nxv2i64.nxv8i64(<vscale x 8 x i64> [[TUPLE:%.*]], i32 3)
// CPP-CHECK-NEXT:    ret <vscale x 2 x i64> [[TMP0]]
//
svint64_t test_svget4_s64(svint64x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_s64,,)(tuple, 3);
}

// CHECK-LABEL: @test_svget4_u8(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i8> @llvm.aarch64.sve.tuple.get.nxv16i8.nxv64i8(<vscale x 64 x i8> [[TUPLE:%.*]], i32 2)
// CHECK-NEXT:    ret <vscale x 16 x i8> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z14test_svget4_u811svuint8x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i8> @llvm.aarch64.sve.tuple.get.nxv16i8.nxv64i8(<vscale x 64 x i8> [[TUPLE:%.*]], i32 2)
// CPP-CHECK-NEXT:    ret <vscale x 16 x i8> [[TMP0]]
//
svuint8_t test_svget4_u8(svuint8x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_u8,,)(tuple, 2);
}

// CHECK-LABEL: @test_svget4_u16(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i16> @llvm.aarch64.sve.tuple.get.nxv8i16.nxv32i16(<vscale x 32 x i16> [[TUPLE:%.*]], i32 3)
// CHECK-NEXT:    ret <vscale x 8 x i16> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_u1612svuint16x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i16> @llvm.aarch64.sve.tuple.get.nxv8i16.nxv32i16(<vscale x 32 x i16> [[TUPLE:%.*]], i32 3)
// CPP-CHECK-NEXT:    ret <vscale x 8 x i16> [[TMP0]]
//
svuint16_t test_svget4_u16(svuint16x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_u16,,)(tuple, 3);
}

// CHECK-LABEL: @test_svget4_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i32> @llvm.aarch64.sve.tuple.get.nxv4i32.nxv16i32(<vscale x 16 x i32> [[TUPLE:%.*]], i32 0)
// CHECK-NEXT:    ret <vscale x 4 x i32> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_u3212svuint32x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i32> @llvm.aarch64.sve.tuple.get.nxv4i32.nxv16i32(<vscale x 16 x i32> [[TUPLE:%.*]], i32 0)
// CPP-CHECK-NEXT:    ret <vscale x 4 x i32> [[TMP0]]
//
svuint32_t test_svget4_u32(svuint32x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_u32,,)(tuple, 0);
}

// CHECK-LABEL: @test_svget4_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i64> @llvm.aarch64.sve.tuple.get.nxv2i64.nxv8i64(<vscale x 8 x i64> [[TUPLE:%.*]], i32 3)
// CHECK-NEXT:    ret <vscale x 2 x i64> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_u6412svuint64x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i64> @llvm.aarch64.sve.tuple.get.nxv2i64.nxv8i64(<vscale x 8 x i64> [[TUPLE:%.*]], i32 3)
// CPP-CHECK-NEXT:    ret <vscale x 2 x i64> [[TMP0]]
//
svuint64_t test_svget4_u64(svuint64x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_u64,,)(tuple, 3);
}

// CHECK-LABEL: @test_svget4_f16(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.tuple.get.nxv8f16.nxv32f16(<vscale x 32 x half> [[TUPLE:%.*]], i32 2)
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_f1613svfloat16x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.tuple.get.nxv8f16.nxv32f16(<vscale x 32 x half> [[TUPLE:%.*]], i32 2)
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP0]]
//
svfloat16_t test_svget4_f16(svfloat16x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_f16,,)(tuple, 2);
}

// CHECK-LABEL: @test_svget4_f32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.tuple.get.nxv4f32.nxv16f32(<vscale x 16 x float> [[TUPLE:%.*]], i32 0)
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_f3213svfloat32x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.tuple.get.nxv4f32.nxv16f32(<vscale x 16 x float> [[TUPLE:%.*]], i32 0)
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP0]]
//
svfloat32_t test_svget4_f32(svfloat32x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_f32,,)(tuple, 0);
}

// CHECK-LABEL: @test_svget4_f64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.tuple.get.nxv2f64.nxv8f64(<vscale x 8 x double> [[TUPLE:%.*]], i32 2)
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svget4_f6413svfloat64x4_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.tuple.get.nxv2f64.nxv8f64(<vscale x 8 x double> [[TUPLE:%.*]], i32 2)
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP0]]
//
svfloat64_t test_svget4_f64(svfloat64x4_t tuple)
{
  return SVE_ACLE_FUNC(svget4,_f64,,)(tuple, 2);
}
