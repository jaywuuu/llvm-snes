; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=arm64-eabi -mcpu=cyclone | FileCheck %s

define i128 @shl(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: shl:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsl x8, x1, x2
; CHECK-NEXT:    mvn w9, w2
; CHECK-NEXT:    lsr x10, x0, #1
; CHECK-NEXT:    lsr x9, x10, x9
; CHECK-NEXT:    orr x8, x8, x9
; CHECK-NEXT:    lsl x9, x0, x2
; CHECK-NEXT:    tst x2, #0x40
; CHECK-NEXT:    csel x1, x9, x8, ne
; CHECK-NEXT:    csel x0, xzr, x9, ne
; CHECK-NEXT:    ret
  %shl = shl i128 %r, %s
  ret i128 %shl
}

define i128 @shl_mask(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: shl_mask:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsl x8, x1, x2
; CHECK-NEXT:    lsr x9, x0, #1
; CHECK-NEXT:    and x10, x2, #0x3f
; CHECK-NEXT:    eor x10, x10, #0x3f
; CHECK-NEXT:    lsr x9, x9, x10
; CHECK-NEXT:    orr x1, x8, x9
; CHECK-NEXT:    lsl x0, x0, x2
; CHECK-NEXT:    ret
  %mask = and i128 %s, 63
  %shl = shl i128 %r, %mask
  ret i128 %shl
}

define i128 @ashr(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: ashr:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsr x8, x0, x2
; CHECK-NEXT:    mvn w9, w2
; CHECK-NEXT:    lsl x10, x1, #1
; CHECK-NEXT:    lsl x9, x10, x9
; CHECK-NEXT:    orr x8, x9, x8
; CHECK-NEXT:    asr x9, x1, x2
; CHECK-NEXT:    tst x2, #0x40
; CHECK-NEXT:    csel x0, x9, x8, ne
; CHECK-NEXT:    asr x8, x1, #63
; CHECK-NEXT:    csel x1, x8, x9, ne
; CHECK-NEXT:    ret
  %shr = ashr i128 %r, %s
  ret i128 %shr
}

define i128 @ashr_mask(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: ashr_mask:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsr x8, x0, x2
; CHECK-NEXT:    lsl x9, x1, #1
; CHECK-NEXT:    and x10, x2, #0x3f
; CHECK-NEXT:    eor x10, x10, #0x3f
; CHECK-NEXT:    lsl x9, x9, x10
; CHECK-NEXT:    orr x0, x8, x9
; CHECK-NEXT:    asr x1, x1, x2
; CHECK-NEXT:    ret
  %mask = and i128 %s, 63
  %shr = ashr i128 %r, %mask
  ret i128 %shr
}

define i128 @lshr(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: lshr:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsr x8, x0, x2
; CHECK-NEXT:    mvn w9, w2
; CHECK-NEXT:    lsl x10, x1, #1
; CHECK-NEXT:    lsl x9, x10, x9
; CHECK-NEXT:    orr x8, x9, x8
; CHECK-NEXT:    lsr x9, x1, x2
; CHECK-NEXT:    tst x2, #0x40
; CHECK-NEXT:    csel x0, x9, x8, ne
; CHECK-NEXT:    csel x1, xzr, x9, ne
; CHECK-NEXT:    ret
  %shr = lshr i128 %r, %s
  ret i128 %shr
}

define i128 @lshr_mask(i128 %r, i128 %s) nounwind readnone {
; CHECK-LABEL: lshr_mask:
; CHECK:       // %bb.0:
; CHECK-NEXT:    lsr x8, x0, x2
; CHECK-NEXT:    lsl x9, x1, #1
; CHECK-NEXT:    and x10, x2, #0x3f
; CHECK-NEXT:    eor x10, x10, #0x3f
; CHECK-NEXT:    lsl x9, x9, x10
; CHECK-NEXT:    orr x0, x8, x9
; CHECK-NEXT:    lsr x1, x1, x2
; CHECK-NEXT:    ret
  %mask = and i128 %s, 63
  %shr = lshr i128 %r, %mask
  ret i128 %shr
}
