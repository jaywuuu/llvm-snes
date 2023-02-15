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

// CHECK-LABEL: @test_svnor_b_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i1> @llvm.aarch64.sve.nor.z.nxv16i1(<vscale x 16 x i1> [[PG:%.*]], <vscale x 16 x i1> [[OP1:%.*]], <vscale x 16 x i1> [[OP2:%.*]])
// CHECK-NEXT:    ret <vscale x 16 x i1> [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z14test_svnor_b_zu10__SVBool_tu10__SVBool_tu10__SVBool_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 16 x i1> @llvm.aarch64.sve.nor.z.nxv16i1(<vscale x 16 x i1> [[PG:%.*]], <vscale x 16 x i1> [[OP1:%.*]], <vscale x 16 x i1> [[OP2:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 16 x i1> [[TMP0]]
//
svbool_t test_svnor_b_z(svbool_t pg, svbool_t op1, svbool_t op2)
{
  return SVE_ACLE_FUNC(svnor,_b,_z,)(pg, op1, op2);
}
