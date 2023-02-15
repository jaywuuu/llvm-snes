; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=aarch64-none-eabi -mattr=+fullfp16,+no-zcz-fp | FileCheck %s --check-prefixes=CHECK-FP16,CHECK-NOZCZ
; RUN: llc < %s -mtriple=aarch64-none-eabi -mattr=+fullfp16,+zcz | FileCheck %s --check-prefixes=CHECK-FP16,CHECK-ZCZ
; RUN: llc < %s -mtriple=aarch64-none-eabi -mattr=-fullfp16 | FileCheck %s --check-prefixes=CHECK-NOFP16

define half @Const0() {
; CHECK-NOZCZ-LABEL: Const0:
; CHECK-NOZCZ:       // %bb.0: // %entry
; CHECK-NOZCZ-NEXT:    fmov h0, wzr
; CHECK-NOZCZ-NEXT:    ret
;
; CHECK-ZCZ-LABEL: Const0:
; CHECK-ZCZ:       // %bb.0: // %entry
; CHECK-ZCZ-NEXT:    movi d0, #0000000000000000
; CHECK-ZCZ-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const0:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI0_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI0_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH0000
}

define half @Const1() {
; CHECK-FP16-LABEL: Const1:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    fmov h0, #1.00000000
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const1:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI1_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI1_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH3C00
}

define half @Const2() {
; CHECK-FP16-LABEL: Const2:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    fmov h0, #0.12500000
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const2:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI2_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI2_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH3000
}

define half @Const3() {
; CHECK-FP16-LABEL: Const3:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    fmov h0, #30.00000000
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const3:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI3_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI3_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH4F80
}

define half @Const4() {
; CHECK-FP16-LABEL: Const4:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    fmov h0, #31.00000000
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const4:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI4_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI4_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH4FC0
}

define half @Const5() {
; CHECK-FP16-LABEL: Const5:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    mov w8, #12272
; CHECK-FP16-NEXT:    fmov h0, w8
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const5:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI5_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI5_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH2FF0
}

define half @Const6() {
; CHECK-FP16-LABEL: Const6:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    mov w8, #20417
; CHECK-FP16-NEXT:    fmov h0, w8
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const6:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI6_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI6_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH4FC1
}

define half @Const7() {
; CHECK-FP16-LABEL: Const7:
; CHECK-FP16:       // %bb.0: // %entry
; CHECK-FP16-NEXT:    mov w8, #20480
; CHECK-FP16-NEXT:    fmov h0, w8
; CHECK-FP16-NEXT:    ret
;
; CHECK-NOFP16-LABEL: Const7:
; CHECK-NOFP16:       // %bb.0: // %entry
; CHECK-NOFP16-NEXT:    adrp x8, .LCPI7_0
; CHECK-NOFP16-NEXT:    ldr h0, [x8, :lo12:.LCPI7_0]
; CHECK-NOFP16-NEXT:    ret
entry:
  ret half 0xH5000
}

