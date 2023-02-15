; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=riscv32 -mattr=+v,+f -verify-machineinstrs \
; RUN:   < %s | FileCheck %s

declare <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
  <vscale x 1 x i64>,
  <vscale x 1 x i64>,
  i64,
  <vscale x 1 x i1>,
  i32,
  i32);

define <vscale x 1 x i64> @intrinsic_vslide1down_mask_tumu_vx_nxv1i64_nxv1i64_i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %2, <vscale x 1 x i1> %3, i32 %4) nounwind {
; CHECK-LABEL: intrinsic_vslide1down_mask_tumu_vx_nxv1i64_nxv1i64_i64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vsetvli a3, a2, e64, m1, ta, mu
; CHECK-NEXT:    slli a3, a3, 1
; CHECK-NEXT:    vsetvli zero, a3, e32, m1, ta, mu
; CHECK-NEXT:    vslide1down.vx v9, v9, a0
; CHECK-NEXT:    vslide1down.vx v9, v9, a1
; CHECK-NEXT:    vsetvli zero, a2, e64, m1, tu, mu
; CHECK-NEXT:    vmerge.vvm v8, v8, v9, v0
; CHECK-NEXT:    ret
entry:
  %a = call <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
    <vscale x 1 x i64> %0,
    <vscale x 1 x i64> %1,
    i64 %2,
    <vscale x 1 x i1> %3,
    i32 %4, i32 0)

  ret <vscale x 1 x i64> %a
}

define <vscale x 1 x i64> @intrinsic_vslide1down_mask_tamu_vx_nxv1i64_nxv1i64_i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %2, <vscale x 1 x i1> %3, i32 %4) nounwind {
; CHECK-LABEL: intrinsic_vslide1down_mask_tamu_vx_nxv1i64_nxv1i64_i64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vsetvli a3, a2, e64, m1, ta, mu
; CHECK-NEXT:    slli a3, a3, 1
; CHECK-NEXT:    vsetvli zero, a3, e32, m1, ta, mu
; CHECK-NEXT:    vslide1down.vx v9, v9, a0
; CHECK-NEXT:    vslide1down.vx v9, v9, a1
; CHECK-NEXT:    vsetvli zero, a2, e64, m1, ta, mu
; CHECK-NEXT:    vmerge.vvm v8, v8, v9, v0
; CHECK-NEXT:    ret
entry:
  %a = call <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
    <vscale x 1 x i64> %0,
    <vscale x 1 x i64> %1,
    i64 %2,
    <vscale x 1 x i1> %3,
    i32 %4, i32 1)

  ret <vscale x 1 x i64> %a
}


; Fallback vslide1 to mask undisturbed until InsertVSETVLI supports mask agnostic.
define <vscale x 1 x i64> @intrinsic_vslide1down_mask_tuma_vx_nxv1i64_nxv1i64_i64(<vscale x 1 x i64> %0, <vscale x 1 x i64> %1, i64 %2, <vscale x 1 x i1> %3, i32 %4) nounwind {
; CHECK-LABEL: intrinsic_vslide1down_mask_tuma_vx_nxv1i64_nxv1i64_i64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vsetvli a3, a2, e64, m1, ta, mu
; CHECK-NEXT:    slli a3, a3, 1
; CHECK-NEXT:    vsetvli zero, a3, e32, m1, ta, mu
; CHECK-NEXT:    vslide1down.vx v9, v9, a0
; CHECK-NEXT:    vslide1down.vx v9, v9, a1
; CHECK-NEXT:    vsetvli zero, a2, e64, m1, tu, mu
; CHECK-NEXT:    vmerge.vvm v8, v8, v9, v0
; CHECK-NEXT:    ret
entry:
  %a = call <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
    <vscale x 1 x i64> %0,
    <vscale x 1 x i64> %1,
    i64 %2,
    <vscale x 1 x i1> %3,
    i32 %4, i32 2)

  ret <vscale x 1 x i64> %a
}

; Fallback vslide1 to mask undisturbed until InsertVSETVLI supports mask agnostic.
define <vscale x 1 x i64> @intrinsic_vslide1down_mask_tama_vx_nxv1i64_nxv1i64_i64(<vscale x 1 x i64> %0, i64 %1, <vscale x 1 x i1> %2, i32 %3) nounwind {
; CHECK-LABEL: intrinsic_vslide1down_mask_tama_vx_nxv1i64_nxv1i64_i64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vsetvli a2, a2, e64, m1, ta, mu
; CHECK-NEXT:    slli a2, a2, 1
; CHECK-NEXT:    vsetvli zero, a2, e32, m1, ta, mu
; CHECK-NEXT:    vslide1down.vx v8, v8, a0
; CHECK-NEXT:    vslide1down.vx v8, v8, a1
; CHECK-NEXT:    ret
entry:
  %a = call <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
    <vscale x 1 x i64> undef,
    <vscale x 1 x i64> %0,
    i64 %1,
    <vscale x 1 x i1> %2,
    i32 %3, i32 3)

  ret <vscale x 1 x i64> %a
}

define <vscale x 1 x i64> @intrinsic_vslide1down_mask_tama_undef_mask_vx_nxv1i64_nxv1i64_i64(<vscale x 1 x i64> %0, i64 %1, i32 %2) nounwind {
; CHECK-LABEL: intrinsic_vslide1down_mask_tama_undef_mask_vx_nxv1i64_nxv1i64_i64:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vsetvli a2, a2, e64, m1, ta, mu
; CHECK-NEXT:    slli a2, a2, 1
; CHECK-NEXT:    vsetvli zero, a2, e32, m1, ta, mu
; CHECK-NEXT:    vslide1down.vx v8, v8, a0
; CHECK-NEXT:    vslide1down.vx v8, v8, a1
; CHECK-NEXT:    ret
entry:
  %a = call <vscale x 1 x i64> @llvm.riscv.vslide1down.mask.nxv1i64.i64(
    <vscale x 1 x i64> undef,
    <vscale x 1 x i64> %0,
    i64 %1,
    <vscale x 1 x i1> undef,
    i32 %2, i32 3)

  ret <vscale x 1 x i64> %a
}
