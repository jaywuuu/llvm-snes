// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
// REQUIRES: aarch64-registered-target
// RUN: %clang_cc1 -no-opaque-pointers -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -o - -emit-llvm %s 2>&1 | FileCheck %s
// RUN: %clang_cc1 -no-opaque-pointers -DSVE_OVERLOADED_FORMS -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -o - -emit-llvm %s 2>&1 | FileCheck %s
// RUN: %clang_cc1 -no-opaque-pointers -triple aarch64-none-linux-gnu -target-feature +sve -fallow-half-arguments-and-returns -S -O1 -Werror -o /dev/null %s
#include <arm_sve.h>

#ifdef SVE_OVERLOADED_FORMS
// A simple used,unused... macro, long enough to represent any SVE builtin.
#define SVE_ACLE_FUNC(A1,A2_UNUSED,A3,A4_UNUSED) A1##A3
#else
#define SVE_ACLE_FUNC(A1,A2,A3,A4) A1##A2##A3##A4
#endif

// CHECK-LABEL: @test_svst1h_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP2:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 4 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv4i16.p0nxv4i16(<vscale x 4 x i16> [[TMP1]], <vscale x 4 x i16>* [[TMP2]], i32 1, <vscale x 4 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_s32(svbool_t pg, int16_t *base, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h,_s32,,)(pg, base, data);
}

// CHECK-LABEL: @test_svst1h_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP2:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 2 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv2i16.p0nxv2i16(<vscale x 2 x i16> [[TMP1]], <vscale x 2 x i16>* [[TMP2]], i32 1, <vscale x 2 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_s64(svbool_t pg, int16_t *base, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h,_s64,,)(pg, base, data);
}

// CHECK-LABEL: @test_svst1h_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP2:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 4 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv4i16.p0nxv4i16(<vscale x 4 x i16> [[TMP1]], <vscale x 4 x i16>* [[TMP2]], i32 1, <vscale x 4 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_u32(svbool_t pg, uint16_t *base, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h,_u32,,)(pg, base, data);
}

// CHECK-LABEL: @test_svst1h_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP2:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 2 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv2i16.p0nxv2i16(<vscale x 2 x i16> [[TMP1]], <vscale x 2 x i16>* [[TMP2]], i32 1, <vscale x 2 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_u64(svbool_t pg, uint16_t *base, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h,_u64,,)(pg, base, data);
}

// CHECK-LABEL: @test_svst1h_vnum_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 4 x i16>*
// CHECK-NEXT:    [[TMP2:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP3:%.*]] = getelementptr <vscale x 4 x i16>, <vscale x 4 x i16>* [[TMP1]], i64 [[VNUM:%.*]], i64 0
// CHECK-NEXT:    [[TMP4:%.*]] = bitcast i16* [[TMP3]] to <vscale x 4 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv4i16.p0nxv4i16(<vscale x 4 x i16> [[TMP2]], <vscale x 4 x i16>* [[TMP4]], i32 1, <vscale x 4 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_vnum_s32(svbool_t pg, int16_t *base, int64_t vnum, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_vnum,_s32,,)(pg, base, vnum, data);
}

// CHECK-LABEL: @test_svst1h_vnum_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 2 x i16>*
// CHECK-NEXT:    [[TMP2:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP3:%.*]] = getelementptr <vscale x 2 x i16>, <vscale x 2 x i16>* [[TMP1]], i64 [[VNUM:%.*]], i64 0
// CHECK-NEXT:    [[TMP4:%.*]] = bitcast i16* [[TMP3]] to <vscale x 2 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv2i16.p0nxv2i16(<vscale x 2 x i16> [[TMP2]], <vscale x 2 x i16>* [[TMP4]], i32 1, <vscale x 2 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_vnum_s64(svbool_t pg, int16_t *base, int64_t vnum, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_vnum,_s64,,)(pg, base, vnum, data);
}

// CHECK-LABEL: @test_svst1h_vnum_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 4 x i16>*
// CHECK-NEXT:    [[TMP2:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP3:%.*]] = getelementptr <vscale x 4 x i16>, <vscale x 4 x i16>* [[TMP1]], i64 [[VNUM:%.*]], i64 0
// CHECK-NEXT:    [[TMP4:%.*]] = bitcast i16* [[TMP3]] to <vscale x 4 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv4i16.p0nxv4i16(<vscale x 4 x i16> [[TMP2]], <vscale x 4 x i16>* [[TMP4]], i32 1, <vscale x 4 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_vnum_u32(svbool_t pg, uint16_t *base, int64_t vnum, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_vnum,_u32,,)(pg, base, vnum, data);
}

// CHECK-LABEL: @test_svst1h_vnum_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP1:%.*]] = bitcast i16* [[BASE:%.*]] to <vscale x 2 x i16>*
// CHECK-NEXT:    [[TMP2:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP3:%.*]] = getelementptr <vscale x 2 x i16>, <vscale x 2 x i16>* [[TMP1]], i64 [[VNUM:%.*]], i64 0
// CHECK-NEXT:    [[TMP4:%.*]] = bitcast i16* [[TMP3]] to <vscale x 2 x i16>*
// CHECK-NEXT:    call void @llvm.masked.store.nxv2i16.p0nxv2i16(<vscale x 2 x i16> [[TMP2]], <vscale x 2 x i16>* [[TMP4]], i32 1, <vscale x 2 x i1> [[TMP0]])
// CHECK-NEXT:    ret void
//
void test_svst1h_vnum_u64(svbool_t pg, uint16_t *base, int64_t vnum, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_vnum,_u64,,)(pg, base, vnum, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 0)
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_s32(svbool_t pg, svuint32_t bases, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,,_s32)(pg, bases, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 0)
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_s64(svbool_t pg, svuint64_t bases, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,,_s64)(pg, bases, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 0)
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_u32(svbool_t pg, svuint32_t bases, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,,_u32)(pg, bases, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 0)
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_u64(svbool_t pg, svuint64_t bases, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,,_u64)(pg, bases, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s32offset_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.sxtw.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s32offset_s32(svbool_t pg, int16_t *base, svint32_t offsets, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s32,offset,_s32)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s64offset_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s64offset_s64(svbool_t pg, int16_t *base, svint64_t offsets, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s64,offset,_s64)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s32offset_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.sxtw.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s32offset_u32(svbool_t pg, uint16_t *base, svint32_t offsets, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s32,offset,_u32)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s64offset_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s64offset_u64(svbool_t pg, uint16_t *base, svint64_t offsets, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s64,offset,_u64)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32offset_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.uxtw.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32offset_s32(svbool_t pg, int16_t *base, svuint32_t offsets, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u32,offset,_s32)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64offset_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64offset_s64(svbool_t pg, int16_t *base, svuint64_t offsets, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u64,offset,_s64)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32offset_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.uxtw.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32offset_u32(svbool_t pg, uint16_t *base, svuint32_t offsets, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u32,offset,_u32)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64offset_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[OFFSETS:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64offset_u64(svbool_t pg, uint16_t *base, svuint64_t offsets, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u64,offset,_u64)(pg, base, offsets, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_offset_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 [[OFFSET:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_offset_s32(svbool_t pg, svuint32_t bases, int64_t offset, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,_offset,_s32)(pg, bases, offset, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_offset_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 [[OFFSET:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_offset_s64(svbool_t pg, svuint64_t bases, int64_t offset, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,_offset,_s64)(pg, bases, offset, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_offset_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 [[OFFSET:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_offset_u32(svbool_t pg, svuint32_t bases, int64_t offset, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,_offset,_u32)(pg, bases, offset, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_offset_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 [[OFFSET:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_offset_u64(svbool_t pg, svuint64_t bases, int64_t offset, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,_offset,_u64)(pg, bases, offset, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s32index_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.sxtw.index.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s32index_s32(svbool_t pg, int16_t *base, svint32_t indices, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s32,index,_s32)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s64index_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.index.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s64index_s64(svbool_t pg, int16_t *base, svint64_t indices, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s64,index,_s64)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s32index_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.sxtw.index.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s32index_u32(svbool_t pg, uint16_t *base, svint32_t indices, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s32,index,_u32)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_s64index_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.index.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_s64index_u64(svbool_t pg, uint16_t *base, svint64_t indices, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,s64,index,_u64)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32index_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.uxtw.index.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32index_s32(svbool_t pg, int16_t *base, svuint32_t indices, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u32,index,_s32)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64index_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.index.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64index_s64(svbool_t pg, int16_t *base, svuint64_t indices, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u64,index,_s64)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32index_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.uxtw.index.nxv4i16(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 4 x i32> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32index_u32(svbool_t pg, uint16_t *base, svuint32_t indices, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u32,index,_u32)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64index_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.index.nxv2i16(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], i16* [[BASE:%.*]], <vscale x 2 x i64> [[INDICES:%.*]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64index_u64(svbool_t pg, uint16_t *base, svuint64_t indices, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter_,u64,index,_u64)(pg, base, indices, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_index_s32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP2:%.*]] = shl i64 [[INDEX:%.*]], 1
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 [[TMP2]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_index_s32(svbool_t pg, svuint32_t bases, int64_t index, svint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,_index,_s32)(pg, bases, index, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_index_s64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP2:%.*]] = shl i64 [[INDEX:%.*]], 1
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 [[TMP2]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_index_s64(svbool_t pg, svuint64_t bases, int64_t index, svint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,_index,_s64)(pg, bases, index, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u32base_index_u32(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 4 x i32> [[DATA:%.*]] to <vscale x 4 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 4 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv4i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP2:%.*]] = shl i64 [[INDEX:%.*]], 1
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv4i16.nxv4i32(<vscale x 4 x i16> [[TMP0]], <vscale x 4 x i1> [[TMP1]], <vscale x 4 x i32> [[BASES:%.*]], i64 [[TMP2]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u32base_index_u32(svbool_t pg, svuint32_t bases, int64_t index, svuint32_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u32base,_index,_u32)(pg, bases, index, data);
}

// CHECK-LABEL: @test_svst1h_scatter_u64base_index_u64(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[TMP0:%.*]] = trunc <vscale x 2 x i64> [[DATA:%.*]] to <vscale x 2 x i16>
// CHECK-NEXT:    [[TMP1:%.*]] = tail call <vscale x 2 x i1> @llvm.aarch64.sve.convert.from.svbool.nxv2i1(<vscale x 16 x i1> [[PG:%.*]])
// CHECK-NEXT:    [[TMP2:%.*]] = shl i64 [[INDEX:%.*]], 1
// CHECK-NEXT:    call void @llvm.aarch64.sve.st1.scatter.scalar.offset.nxv2i16.nxv2i64(<vscale x 2 x i16> [[TMP0]], <vscale x 2 x i1> [[TMP1]], <vscale x 2 x i64> [[BASES:%.*]], i64 [[TMP2]])
// CHECK-NEXT:    ret void
//
void test_svst1h_scatter_u64base_index_u64(svbool_t pg, svuint64_t bases, int64_t index, svuint64_t data)
{
  return SVE_ACLE_FUNC(svst1h_scatter,_u64base,_index,_u64)(pg, bases, index, data);
}
