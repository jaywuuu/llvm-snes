; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=aarch64 -mattr=+sve < %s | FileCheck %s

;
; OR reductions
;

define i1 @reduce_or_insert_subvec_into_zero(<vscale x 4 x i1> %in) {
; CHECK-LABEL: reduce_or_insert_subvec_into_zero:
; CHECK:       // %bb.0:
; CHECK-NEXT:    ptrue p1.s
; CHECK-NEXT:    ptest p1, p0.b
; CHECK-NEXT:    cset w0, ne
; CHECK-NEXT:    ret
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> zeroinitializer, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.or.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

define i1 @reduce_or_insert_subvec_into_poison(<vscale x 4 x i1> %in) {
; CHECK-LABEL: reduce_or_insert_subvec_into_poison:
; CHECK:       // %bb.0:
; CHECK-NEXT:    ptrue p1.s
; CHECK-NEXT:    ptest p1, p0.b
; CHECK-NEXT:    cset w0, ne
; CHECK-NEXT:    ret
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> poison, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.or.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

define i1 @reduce_or_insert_subvec_into_nonzero(<vscale x 4 x i1> %in, <vscale x 16 x i1> %vec) {
; CHECK-LABEL: reduce_or_insert_subvec_into_nonzero:
; CHECK:       // %bb.0:
; CHECK-NEXT:    punpklo p2.h, p1.b
; CHECK-NEXT:    punpkhi p1.h, p1.b
; CHECK-NEXT:    punpkhi p2.h, p2.b
; CHECK-NEXT:    uzp1 p0.h, p0.h, p2.h
; CHECK-NEXT:    uzp1 p0.b, p0.b, p1.b
; CHECK-NEXT:    ptest p0, p0.b
; CHECK-NEXT:    cset w0, ne
; CHECK-NEXT:    ret
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> %vec, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.or.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

;
; AND reductions
;

define i1 @reduce_and_insert_subvec_into_ones(<vscale x 4 x i1> %in) {
; CHECK-LABEL: reduce_and_insert_subvec_into_ones:
; CHECK:       // %bb.0:
; CHECK-NEXT:    ptrue p1.s
; CHECK-NEXT:    not p0.b, p1/z, p0.b
; CHECK-NEXT:    ptest p1, p0.b
; CHECK-NEXT:    cset w0, eq
; CHECK-NEXT:    ret
  %allones.ins = insertelement <vscale x 16 x i1> poison, i1 1, i32 0
  %allones = shufflevector <vscale x 16 x i1> %allones.ins,  <vscale x 16 x i1> poison,  <vscale x 16 x i32> zeroinitializer
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> %allones, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.and.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

define i1 @reduce_and_insert_subvec_into_poison(<vscale x 4 x i1> %in) {
; CHECK-LABEL: reduce_and_insert_subvec_into_poison:
; CHECK:       // %bb.0:
; CHECK-NEXT:    ptrue p1.s
; CHECK-NEXT:    not p0.b, p1/z, p0.b
; CHECK-NEXT:    ptest p1, p0.b
; CHECK-NEXT:    cset w0, eq
; CHECK-NEXT:    ret
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> poison, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.and.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

define i1 @reduce_and_insert_subvec_into_var(<vscale x 4 x i1> %in, <vscale x 16 x i1> %vec) {
; CHECK-LABEL: reduce_and_insert_subvec_into_var:
; CHECK:       // %bb.0:
; CHECK-NEXT:    punpklo p3.h, p1.b
; CHECK-NEXT:    punpkhi p1.h, p1.b
; CHECK-NEXT:    punpkhi p3.h, p3.b
; CHECK-NEXT:    ptrue p2.b
; CHECK-NEXT:    uzp1 p0.h, p0.h, p3.h
; CHECK-NEXT:    uzp1 p0.b, p0.b, p1.b
; CHECK-NEXT:    not p0.b, p2/z, p0.b
; CHECK-NEXT:    ptest p2, p0.b
; CHECK-NEXT:    cset w0, eq
; CHECK-NEXT:    ret
  %t = call <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1> %vec, <vscale x 4 x i1> %in, i64 0)
  %res = call i1 @llvm.vector.reduce.and.nxv16i1(<vscale x 16 x i1> %t)
  ret i1 %res
}

declare i1 @llvm.vector.reduce.and.nxv16i1(<vscale x 16 x i1>)
declare i1 @llvm.vector.reduce.or.nxv16i1(<vscale x 16 x i1>)
declare <vscale x 16 x i1> @llvm.vector.insert.nxv16i1.nxv4i1(<vscale x 16 x i1>, <vscale x 4 x i1>, i64)
