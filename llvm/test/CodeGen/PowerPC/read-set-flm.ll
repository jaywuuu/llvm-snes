; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple powerpc64le-unknown-linux | FileCheck %s
; RUN: llc < %s -mtriple powerpc64le-unknown-linux -debug-only=machine-scheduler \
; RUN:   2>&1 | FileCheck %s --check-prefix=LOG
; REQUIRES: DEBUG

define double @in_nostrict(double %a, double %b, double %c, double %d) {
; CHECK-LABEL: in_nostrict:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    xsdivdp 1, 1, 2
; CHECK-NEXT:    xsadddp 1, 1, 3
; CHECK-NEXT:    xsadddp 0, 1, 0
; CHECK-NEXT:    mtfsf 255, 4
; CHECK-NEXT:    xsdivdp 1, 3, 4
; CHECK-NEXT:    xsadddp 1, 1, 2
; CHECK-NEXT:    xsadddp 1, 0, 1
; CHECK-NEXT:    blr
;
; LOG: *** MI Scheduling ***
; LOG-NEXT: in_nostrict:%bb.0 entry
; LOG: ExitSU: MTFSF 255, %{{[0-9]+}}:f8rc, 0, 0
; LOG: *** MI Scheduling ***
; LOG-NEXT: in_nostrict:%bb.0 entry
; LOG: ExitSU: %{{[0-9]+}}:f8rc = MFFS implicit $rm
;
; LOG: *** MI Scheduling ***
; LOG-NEXT: in_nostrict:%bb.0 entry
; LOG: ExitSU: MTFSF 255, renamable $f{{[0-9]+}}, 0, 0
entry:
  %0 = tail call double @llvm.ppc.readflm()
  %1 = fdiv double %a, %b
  %2 = fadd double %1, %c
  %3 = fadd double %2, %0
  call double @llvm.ppc.setflm(double %d)
  %5 = fdiv double %c, %d
  %6 = fadd double %5, %b
  %7 = fadd double %3, %6
  ret double %7
}

define double @in_strict(double %a, double %b, double %c, double %d) #0 {
; CHECK-LABEL: in_strict:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    xsdivdp 1, 1, 2
; CHECK-NEXT:    xsadddp 1, 1, 3
; CHECK-NEXT:    xsadddp 0, 1, 0
; CHECK-NEXT:    mtfsf 255, 4
; CHECK-NEXT:    xsdivdp 1, 3, 4
; CHECK-NEXT:    xsadddp 1, 1, 2
; CHECK-NEXT:    xsadddp 1, 0, 1
; CHECK-NEXT:    blr
;
; LOG: ***** MI Scheduling *****
; LOG-NEXT: in_strict:%bb.0 entry
; LOG: ExitSU: MTFSF 255, %{{[0-9]+}}:f8rc, 0, 0
; LOG: ***** MI Scheduling *****
; LOG-NEXT: in_strict:%bb.0 entry
; LOG: ExitSU: %{{[0-9]+}}:f8rc = MFFS implicit $rm
;
; LOG: ***** MI Scheduling *****
; LOG-NEXT: in_strict:%bb.0 entry
; LOG: ExitSU: MTFSF 255, renamable $f{{[0-9]+}}, 0, 0
entry:
  %0 = tail call double @llvm.ppc.readflm()
  %1 = call double @llvm.experimental.constrained.fdiv.f64(double %a, double %b, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  %2 = call double @llvm.experimental.constrained.fadd.f64(double %1, double %c, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  %3 = call double @llvm.experimental.constrained.fadd.f64(double %2, double %0, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  call double @llvm.ppc.setflm(double %d)
  %5 = call double @llvm.experimental.constrained.fdiv.f64(double %c, double %d, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  %6 = call double @llvm.experimental.constrained.fadd.f64(double %5, double %b, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  %7 = call double @llvm.experimental.constrained.fadd.f64(double %3, double %6, metadata !"round.dynamic", metadata !"fpexcept.strict") #0
  ret double %7
}

define void @cse_nomerge(double* %f1, double* %f2, double %f3) #0 {
; CHECK-LABEL: cse_nomerge:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    mflr 0
; CHECK-NEXT:    .cfi_def_cfa_offset 64
; CHECK-NEXT:    .cfi_offset lr, 16
; CHECK-NEXT:    .cfi_offset r30, -24
; CHECK-NEXT:    .cfi_offset f31, -8
; CHECK-NEXT:    std 30, -24(1) # 8-byte Folded Spill
; CHECK-NEXT:    stfd 31, -8(1) # 8-byte Folded Spill
; CHECK-NEXT:    std 0, 16(1)
; CHECK-NEXT:    stdu 1, -64(1)
; CHECK-NEXT:    fmr 31, 1
; CHECK-NEXT:    mr 30, 4
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    stfd 0, 0(3)
; CHECK-NEXT:    bl effect_func
; CHECK-NEXT:    nop
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    stfd 0, 0(30)
; CHECK-NEXT:    mtfsf 255, 31
; CHECK-NEXT:    addi 1, 1, 64
; CHECK-NEXT:    ld 0, 16(1)
; CHECK-NEXT:    lfd 31, -8(1) # 8-byte Folded Reload
; CHECK-NEXT:    ld 30, -24(1) # 8-byte Folded Reload
; CHECK-NEXT:    mtlr 0
; CHECK-NEXT:    blr
entry:
  %0 = call double @llvm.ppc.readflm()
  store double %0, double* %f1, align 8
  call void @effect_func()
  %1 = call double @llvm.ppc.readflm()
  store double %1, double* %f2, align 8
  %2 = call contract double @llvm.ppc.setflm(double %f3)
  ret void
}

define void @cse_nomerge_readonly(double* %f1, double* %f2, double %f3) #0 {
; CHECK-LABEL: cse_nomerge_readonly:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    mflr 0
; CHECK-NEXT:    .cfi_def_cfa_offset 64
; CHECK-NEXT:    .cfi_offset lr, 16
; CHECK-NEXT:    .cfi_offset r30, -24
; CHECK-NEXT:    .cfi_offset f31, -8
; CHECK-NEXT:    std 30, -24(1) # 8-byte Folded Spill
; CHECK-NEXT:    stfd 31, -8(1) # 8-byte Folded Spill
; CHECK-NEXT:    std 0, 16(1)
; CHECK-NEXT:    stdu 1, -64(1)
; CHECK-NEXT:    fmr 31, 1
; CHECK-NEXT:    mr 30, 4
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    stfd 0, 0(3)
; CHECK-NEXT:    bl readonly_func
; CHECK-NEXT:    nop
; CHECK-NEXT:    mffs 0
; CHECK-NEXT:    stfd 0, 0(30)
; CHECK-NEXT:    mtfsf 255, 31
; CHECK-NEXT:    addi 1, 1, 64
; CHECK-NEXT:    ld 0, 16(1)
; CHECK-NEXT:    lfd 31, -8(1) # 8-byte Folded Reload
; CHECK-NEXT:    ld 30, -24(1) # 8-byte Folded Reload
; CHECK-NEXT:    mtlr 0
; CHECK-NEXT:    blr
entry:
  %0 = call double @llvm.ppc.readflm()
  store double %0, double* %f1, align 8
  call void @readonly_func()
  %1 = call double @llvm.ppc.readflm()
  store double %1, double* %f2, align 8
  %2 = call contract double @llvm.ppc.setflm(double %f3)
  ret void
}

declare void @effect_func()
declare void @readonly_func() #1
declare double @llvm.ppc.readflm()
declare double @llvm.ppc.setflm(double)
declare double @llvm.experimental.constrained.fadd.f64(double, double, metadata, metadata)
declare double @llvm.experimental.constrained.fdiv.f64(double, double, metadata, metadata)

attributes #0 = { strictfp }
attributes #1 = { readonly }
