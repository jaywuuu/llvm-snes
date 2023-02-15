; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=aarch64-linux-gnu -mattr=+sme-i64 -verify-machineinstrs < %s | FileCheck %s

define void @addha_s(<vscale x 4 x i1> %pn, <vscale x 4 x i1> %pm, <vscale x 4 x i32> %zn) {
; CHECK-LABEL: addha_s:
; CHECK:       // %bb.0:
; CHECK-NEXT:    addha za0.s, p0/m, p1/m, z0.s
; CHECK-NEXT:    ret
  call void @llvm.aarch64.sme.addha.nxv4i32(i64 0, <vscale x 4 x i1> %pn, <vscale x 4 x i1> %pm, <vscale x 4 x i32> %zn)
  ret void
}

define void @addva_s(<vscale x 4 x i1> %pn, <vscale x 4 x i1> %pm, <vscale x 4 x i32> %zn) {
; CHECK-LABEL: addva_s:
; CHECK:       // %bb.0:
; CHECK-NEXT:    addva za3.s, p0/m, p1/m, z0.s
; CHECK-NEXT:    ret
  call void @llvm.aarch64.sme.addva.nxv4i32(i64 3, <vscale x 4 x i1> %pn, <vscale x 4 x i1> %pm, <vscale x 4 x i32> %zn)
  ret void
}

define void @addha_d(<vscale x 2 x i1> %pn, <vscale x 2 x i1> %pm, <vscale x 2 x i64> %zn) {
; CHECK-LABEL: addha_d:
; CHECK:       // %bb.0:
; CHECK-NEXT:    addha za0.d, p0/m, p1/m, z0.d
; CHECK-NEXT:    ret
  call void @llvm.aarch64.sme.addha.nxv2i64(i64 0, <vscale x 2 x i1> %pn, <vscale x 2 x i1> %pm, <vscale x 2 x i64> %zn)
  ret void
}

define void @addva_d(<vscale x 2 x i1> %pn, <vscale x 2 x i1> %pm, <vscale x 2 x i64> %zn) {
; CHECK-LABEL: addva_d:
; CHECK:       // %bb.0:
; CHECK-NEXT:    addva za7.d, p0/m, p1/m, z0.d
; CHECK-NEXT:    ret
  call void @llvm.aarch64.sme.addva.nxv2i64(i64 7, <vscale x 2 x i1> %pn, <vscale x 2 x i1> %pm, <vscale x 2 x i64> %zn)
  ret void
}

declare void @llvm.aarch64.sme.addha.nxv4i32(i64, <vscale x 4 x i1>, <vscale x 4 x i1>, <vscale x 4 x i32>)
declare void @llvm.aarch64.sme.addha.nxv2i64(i64, <vscale x 2 x i1>, <vscale x 2 x i1>, <vscale x 2 x i64>)
declare void @llvm.aarch64.sme.addva.nxv4i32(i64, <vscale x 4 x i1>, <vscale x 4 x i1>, <vscale x 4 x i32>)
declare void @llvm.aarch64.sme.addva.nxv2i64(i64, <vscale x 2 x i1>, <vscale x 2 x i1>, <vscale x 2 x i64>)
