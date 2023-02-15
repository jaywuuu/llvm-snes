// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// REQUIRES: aarch64-registered-target
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -emit-llvm -o - -x c++ %s | FileCheck %s -check-prefix=CPP-CHECK
// RUN: %clang_cc1 -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -Wall -o /dev/null %s
#include <arm_sve.h>

// CHECK-LABEL: @test_svcntd(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.vscale.i64()
// CHECK-NEXT:    [[TMP1:%.*]] = shl nuw nsw i64 [[TMP0]], 1
// CHECK-NEXT:    ret i64 [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z11test_svcntdv(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.vscale.i64()
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shl nuw nsw i64 [[TMP0]], 1
// CPP-CHECK-NEXT:    ret i64 [[TMP1]]
//
uint64_t test_svcntd()
{
  return svcntd();
}

// CHECK-LABEL: @test_svcntd_pat(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 0)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z15test_svcntd_patv(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 0)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat()
{
  return svcntd_pat(SV_POW2);
}

// CHECK-LABEL: @test_svcntd_pat_1(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    ret i64 1
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_1v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    ret i64 1
//
uint64_t test_svcntd_pat_1()
{
  return svcntd_pat(SV_VL1);
}

// CHECK-LABEL: @test_svcntd_pat_2(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    ret i64 2
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_2v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    ret i64 2
//
uint64_t test_svcntd_pat_2()
{
  return svcntd_pat(SV_VL2);
}

// CHECK-LABEL: @test_svcntd_pat_3(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 3)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_3v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 3)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_3()
{
  return svcntd_pat(SV_VL3);
}

// CHECK-LABEL: @test_svcntd_pat_4(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 4)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_4v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 4)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_4()
{
  return svcntd_pat(SV_VL4);
}

// CHECK-LABEL: @test_svcntd_pat_5(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 5)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_5v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 5)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_5()
{
  return svcntd_pat(SV_VL5);
}

// CHECK-LABEL: @test_svcntd_pat_6(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 6)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_6v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 6)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_6()
{
  return svcntd_pat(SV_VL6);
}

// CHECK-LABEL: @test_svcntd_pat_7(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 7)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_7v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 7)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_7()
{
  return svcntd_pat(SV_VL7);
}

// CHECK-LABEL: @test_svcntd_pat_8(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 8)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_8v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 8)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_8()
{
  return svcntd_pat(SV_VL8);
}

// CHECK-LABEL: @test_svcntd_pat_9(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 9)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z17test_svcntd_pat_9v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 9)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_9()
{
  return svcntd_pat(SV_VL16);
}

// CHECK-LABEL: @test_svcntd_pat_10(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 10)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_10v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 10)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_10()
{
  return svcntd_pat(SV_VL32);
}

// CHECK-LABEL: @test_svcntd_pat_11(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 11)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_11v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 11)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_11()
{
  return svcntd_pat(SV_VL64);
}

// CHECK-LABEL: @test_svcntd_pat_12(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 12)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_12v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 12)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_12()
{
  return svcntd_pat(SV_VL128);
}

// CHECK-LABEL: @test_svcntd_pat_13(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 13)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_13v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 13)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_13()
{
  return svcntd_pat(SV_VL256);
}

// CHECK-LABEL: @test_svcntd_pat_14(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 29)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_14v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 29)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_14()
{
  return svcntd_pat(SV_MUL4);
}

// CHECK-LABEL: @test_svcntd_pat_15(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 30)
// CHECK-NEXT:    ret i64 [[TMP0]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_15v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.aarch64.sve.cntd(i32 30)
// CPP-CHECK-NEXT:    ret i64 [[TMP0]]
//
uint64_t test_svcntd_pat_15()
{
  return svcntd_pat(SV_MUL3);
}

// CHECK-LABEL: @test_svcntd_pat_16(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.vscale.i64()
// CHECK-NEXT:    [[TMP1:%.*]] = shl nuw nsw i64 [[TMP0]], 1
// CHECK-NEXT:    ret i64 [[TMP1]]
//
// CPP-CHECK-LABEL: @_Z18test_svcntd_pat_16v(
// CPP-CHECK-NEXT:  entry:
// CPP-CHECK-NEXT:    [[TMP0:%.*]] = tail call i64 @llvm.vscale.i64()
// CPP-CHECK-NEXT:    [[TMP1:%.*]] = shl nuw nsw i64 [[TMP0]], 1
// CPP-CHECK-NEXT:    ret i64 [[TMP1]]
//
uint64_t test_svcntd_pat_16()
{
  return svcntd_pat(SV_ALL);
}
