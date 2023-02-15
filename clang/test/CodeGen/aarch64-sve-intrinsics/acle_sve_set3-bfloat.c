// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -target-feature +bf16 -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -target-feature +bf16 -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - -x c++ %s | FileCheck %s -check-prefix=CPP-CHECK
// RUN: %clang_cc1 -DSVE_OVERLOADED_FORMS -triple aarch64-none-linux-gnu -target-feature +sve -target-feature +bf16 -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -DSVE_OVERLOADED_FORMS -triple aarch64-none-linux-gnu -target-feature +sve -target-feature +bf16 -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - -x c++ %s | FileCheck %s -check-prefix=CPP-CHECK

// REQUIRES: aarch64-registered-target

#include <arm_sve.h>

#ifdef SVE_OVERLOADED_FORMS
// A simple used,unused... macro, long enough to represent any SVE builtin.
#define SVE_ACLE_FUNC(A1,A2_UNUSED,A3,A4_UNUSED) A1##A3
#else
#define SVE_ACLE_FUNC(A1,A2,A3,A4) A1##A2##A3##A4
#endif


// CHECK-LABEL: @test_svset3_bf16_0(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 0, <vscale x 8 x bfloat> [[X:%.*]])
// CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svset3_bf16_014svbfloat16x3_tu14__SVBFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 0, <vscale x 8 x bfloat> [[X:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
svbfloat16x3_t test_svset3_bf16_0(svbfloat16x3_t tuple, svbfloat16_t x)
{
  return SVE_ACLE_FUNC(svset3,_bf16,,)(tuple, 0, x);
}

// CHECK-LABEL: @test_svset3_bf16_1(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 1, <vscale x 8 x bfloat> [[X:%.*]])
// CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svset3_bf16_114svbfloat16x3_tu14__SVBFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 1, <vscale x 8 x bfloat> [[X:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
svbfloat16x3_t test_svset3_bf16_1(svbfloat16x3_t tuple, svbfloat16_t x)
{
  return SVE_ACLE_FUNC(svset3,_bf16,,)(tuple, 1, x);
}

// CHECK-LABEL: @test_svset3_bf16_2(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 2, <vscale x 8 x bfloat> [[X:%.*]])
// CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svset3_bf16_214svbfloat16x3_tu14__SVBFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 24 x bfloat> @llvm.aarch64.sve.tuple.set.nxv24bf16.nxv8bf16(<vscale x 24 x bfloat> [[TUPLE:%.*]], i32 2, <vscale x 8 x bfloat> [[X:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 24 x bfloat> [[TMP0]]
//
svbfloat16x3_t test_svset3_bf16_2(svbfloat16x3_t tuple, svbfloat16_t x)
{
  return SVE_ACLE_FUNC(svset3,_bf16,,)(tuple, 2, x);
}
