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

// CHECK-LABEL: @test_svnmsb_f16_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[TMP1]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f16_zu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tu13__SVFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[TMP1]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
svfloat16_t test_svnmsb_f16_z(svbool_t pg, svfloat16_t op1, svfloat16_t op2, svfloat16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f16,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f32_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[TMP1]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f32_zu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tu13__SVFloat32_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[TMP1]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
svfloat32_t test_svnmsb_f32_z(svbool_t pg, svfloat32_t op1, svfloat32_t op2, svfloat32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f32,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f64_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[TMP1]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f64_zu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_tu13__SVFloat64_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = select <vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[TMP1]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
svfloat64_t test_svnmsb_f64_z(svbool_t pg, svfloat64_t op1, svfloat64_t op2, svfloat64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f64,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f16_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f16_mu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tu13__SVFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP1]]
//
svfloat16_t test_svnmsb_f16_m(svbool_t pg, svfloat16_t op1, svfloat16_t op2, svfloat16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f16,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f32_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f32_mu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tu13__SVFloat32_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP1]]
//
svfloat32_t test_svnmsb_f32_m(svbool_t pg, svfloat32_t op1, svfloat32_t op2, svfloat32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f32,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f64_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f64_mu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_tu13__SVFloat64_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP1]]
//
svfloat64_t test_svnmsb_f64_m(svbool_t pg, svfloat64_t op1, svfloat64_t op2, svfloat64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f64,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f16_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f16_xu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tu13__SVFloat16_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP1]]
//
svfloat16_t test_svnmsb_f16_x(svbool_t pg, svfloat16_t op1, svfloat16_t op2, svfloat16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f16,_x,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f32_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f32_xu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tu13__SVFloat32_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP1]]
//
svfloat32_t test_svnmsb_f32_x(svbool_t pg, svfloat32_t op1, svfloat32_t op2, svfloat32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f32,_x,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_f64_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z17test_svnmsb_f64_xu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_tu13__SVFloat64_t(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[OP3:%.*]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP1]]
//
svfloat64_t test_svnmsb_f64_x(svbool_t pg, svfloat64_t op1, svfloat64_t op2, svfloat64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_f64,_x,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f16_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> zeroinitializer
// CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[TMP2]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP3]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f16_zu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tDh(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[TMP2]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP3]]
//
svfloat16_t test_svnmsb_n_f16_z(svbool_t pg, svfloat16_t op1, svfloat16_t op2, float16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f16,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f32_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> zeroinitializer
// CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[TMP2]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP3]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f32_zu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tf(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[TMP2]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP3]]
//
svfloat32_t test_svnmsb_n_f32_z(svbool_t pg, svfloat32_t op1, svfloat32_t op2, float32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f32,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f64_z(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> zeroinitializer
// CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[TMP2]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP3]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f64_zu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_td(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = select <vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP3:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[TMP2]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP3]]
//
svfloat64_t test_svnmsb_n_f64_z(svbool_t pg, svfloat64_t op1, svfloat64_t op2, float64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f64,_z,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f16_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f16_mu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tDh(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
svfloat16_t test_svnmsb_n_f16_m(svbool_t pg, svfloat16_t op1, svfloat16_t op2, float16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f16,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f32_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f32_mu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tf(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
svfloat32_t test_svnmsb_n_f32_m(svbool_t pg, svfloat32_t op1, svfloat32_t op2, float32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f32,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f64_m(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f64_mu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_td(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
svfloat64_t test_svnmsb_n_f64_m(svbool_t pg, svfloat64_t op1, svfloat64_t op2, float64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f64,_m,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f16_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f16_xu10__SVBool_tu13__SVFloat16_tu13__SVFloat16_tDh(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 8 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv8i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 8 x half> poison, half [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 8 x half> [[DOTSPLATINSERT]], <vscale x 8 x half> poison, <vscale x 8 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 8 x half> @llvm.aarch64.sve.fnmsb.nxv8f16(<vscale x 8 x i1> [[TMP0]], <vscale x 8 x half> [[OP1:%.*]], <vscale x 8 x half> [[OP2:%.*]], <vscale x 8 x half> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 8 x half> [[TMP2]]
//
svfloat16_t test_svnmsb_n_f16_x(svbool_t pg, svfloat16_t op1, svfloat16_t op2, float16_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f16,_x,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f32_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f32_xu10__SVBool_tu13__SVFloat32_tu13__SVFloat32_tf(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 4 x float> poison, float [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 4 x float> [[DOTSPLATINSERT]], <vscale x 4 x float> poison, <vscale x 4 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 4 x float> @llvm.aarch64.sve.fnmsb.nxv4f32(<vscale x 4 x i1> [[TMP0]], <vscale x 4 x float> [[OP1:%.*]], <vscale x 4 x float> [[OP2:%.*]], <vscale x 4 x float> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 4 x float> [[TMP2]]
//
svfloat32_t test_svnmsb_n_f32_x(svbool_t pg, svfloat32_t op1, svfloat32_t op2, float32_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f32,_x,)(pg, op1, op2, op3);
}

// CHECK-LABEL: @test_svnmsb_n_f64_x(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
// CPP-CHECK-LABEL: @_Z19test_svnmsb_n_f64_xu10__SVBool_tu13__SVFloat64_tu13__SVFloat64_td(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CPP-CHECK-NEXT:    [[DOTSPLATINSERT:%.*]] = insertelement <vscale x 2 x double> poison, double [[OP3:%.*]], i64 0
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shufflevector <vscale x 2 x double> [[DOTSPLATINSERT]], <vscale x 2 x double> poison, <vscale x 2 x i32> zeroinitializer
// CPP-CHECK-NEXT:    [[TMP2:%.*]] = tail call <vscale x 2 x double> @llvm.aarch64.sve.fnmsb.nxv2f64(<vscale x 2 x i1> [[TMP0]], <vscale x 2 x double> [[OP1:%.*]], <vscale x 2 x double> [[OP2:%.*]], <vscale x 2 x double> [[TMP1]])
// CPP-CHECK-NEXT:    ret <vscale x 2 x double> [[TMP2]]
//
svfloat64_t test_svnmsb_n_f64_x(svbool_t pg, svfloat64_t op1, svfloat64_t op2, float64_t op3)
{
  return SVE_ACLE_FUNC(svnmsb,_n_f64,_x,)(pg, op1, op2, op3);
}
