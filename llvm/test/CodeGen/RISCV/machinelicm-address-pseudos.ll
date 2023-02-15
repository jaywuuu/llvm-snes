; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -mtriple=riscv32 -relocation-model=pic -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefixes=RV32I %s
; RUN: llc -mtriple=riscv64 -relocation-model=pic -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefixes=RV64I %s

; Verifies that MachineLICM can hoist address generation pseudos out of loops.

@l = protected global i32 0, align 4

define void @test_lla(i32 signext %n) {
; RV32I-LABEL: test_lla:
; RV32I:       # %bb.0: # %entry
; RV32I-NEXT:    li a1, 0
; RV32I-NEXT:  .LBB0_3: # %entry
; RV32I-NEXT:    # Label of block must be emitted
; RV32I-NEXT:    auipc a2, %pcrel_hi(l)
; RV32I-NEXT:    addi a2, a2, %pcrel_lo(.LBB0_3)
; RV32I-NEXT:  .LBB0_1: # %loop
; RV32I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV32I-NEXT:    lw a3, 0(a2)
; RV32I-NEXT:    addi a1, a1, 1
; RV32I-NEXT:    blt a1, a0, .LBB0_1
; RV32I-NEXT:  # %bb.2: # %ret
; RV32I-NEXT:    ret
;
; RV64I-LABEL: test_lla:
; RV64I:       # %bb.0: # %entry
; RV64I-NEXT:    li a1, 0
; RV64I-NEXT:  .LBB0_3: # %entry
; RV64I-NEXT:    # Label of block must be emitted
; RV64I-NEXT:    auipc a2, %pcrel_hi(l)
; RV64I-NEXT:    addi a2, a2, %pcrel_lo(.LBB0_3)
; RV64I-NEXT:  .LBB0_1: # %loop
; RV64I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV64I-NEXT:    lw a3, 0(a2)
; RV64I-NEXT:    addiw a1, a1, 1
; RV64I-NEXT:    blt a1, a0, .LBB0_1
; RV64I-NEXT:  # %bb.2: # %ret
; RV64I-NEXT:    ret
entry:
  br label %loop

loop:
  %i = phi i32 [ %inc, %loop ], [ 0, %entry ]
  %0 = load volatile i32, i32* @l, align 4
  %inc = add nuw nsw i32 %i, 1
  %cmp = icmp slt i32 %inc, %n
  br i1 %cmp, label %loop, label %ret

ret:
  ret void
}

@g = global i32 0, align 4

define void @test_la(i32 signext %n) {
; RV32I-LABEL: test_la:
; RV32I:       # %bb.0: # %entry
; RV32I-NEXT:  .LBB1_3: # %entry
; RV32I-NEXT:    # Label of block must be emitted
; RV32I-NEXT:    auipc a1, %got_pcrel_hi(g)
; RV32I-NEXT:    lw a1, %pcrel_lo(.LBB1_3)(a1)
; RV32I-NEXT:    li a2, 0
; RV32I-NEXT:  .LBB1_1: # %loop
; RV32I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV32I-NEXT:    lw a3, 0(a1)
; RV32I-NEXT:    addi a2, a2, 1
; RV32I-NEXT:    blt a2, a0, .LBB1_1
; RV32I-NEXT:  # %bb.2: # %ret
; RV32I-NEXT:    ret
;
; RV64I-LABEL: test_la:
; RV64I:       # %bb.0: # %entry
; RV64I-NEXT:  .LBB1_3: # %entry
; RV64I-NEXT:    # Label of block must be emitted
; RV64I-NEXT:    auipc a1, %got_pcrel_hi(g)
; RV64I-NEXT:    ld a1, %pcrel_lo(.LBB1_3)(a1)
; RV64I-NEXT:    li a2, 0
; RV64I-NEXT:  .LBB1_1: # %loop
; RV64I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV64I-NEXT:    lw a3, 0(a1)
; RV64I-NEXT:    addiw a2, a2, 1
; RV64I-NEXT:    blt a2, a0, .LBB1_1
; RV64I-NEXT:  # %bb.2: # %ret
; RV64I-NEXT:    ret
entry:
  br label %loop

loop:
  %i = phi i32 [ %inc, %loop ], [ 0, %entry ]
  %0 = load volatile i32, i32* @g, align 4
  %inc = add nuw nsw i32 %i, 1
  %cmp = icmp slt i32 %inc, %n
  br i1 %cmp, label %loop, label %ret

ret:
  ret void
}

@ie = external thread_local(initialexec) global i32

define void @test_la_tls_ie(i32 signext %n) {
; RV32I-LABEL: test_la_tls_ie:
; RV32I:       # %bb.0: # %entry
; RV32I-NEXT:  .LBB2_3: # %entry
; RV32I-NEXT:    # Label of block must be emitted
; RV32I-NEXT:    auipc a2, %tls_ie_pcrel_hi(ie)
; RV32I-NEXT:    lw a2, %pcrel_lo(.LBB2_3)(a2)
; RV32I-NEXT:    li a1, 0
; RV32I-NEXT:    add a2, a2, tp
; RV32I-NEXT:  .LBB2_1: # %loop
; RV32I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV32I-NEXT:    lw a3, 0(a2)
; RV32I-NEXT:    addi a1, a1, 1
; RV32I-NEXT:    blt a1, a0, .LBB2_1
; RV32I-NEXT:  # %bb.2: # %ret
; RV32I-NEXT:    ret
;
; RV64I-LABEL: test_la_tls_ie:
; RV64I:       # %bb.0: # %entry
; RV64I-NEXT:  .LBB2_3: # %entry
; RV64I-NEXT:    # Label of block must be emitted
; RV64I-NEXT:    auipc a2, %tls_ie_pcrel_hi(ie)
; RV64I-NEXT:    ld a2, %pcrel_lo(.LBB2_3)(a2)
; RV64I-NEXT:    li a1, 0
; RV64I-NEXT:    add a2, a2, tp
; RV64I-NEXT:  .LBB2_1: # %loop
; RV64I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV64I-NEXT:    lw a3, 0(a2)
; RV64I-NEXT:    addiw a1, a1, 1
; RV64I-NEXT:    blt a1, a0, .LBB2_1
; RV64I-NEXT:  # %bb.2: # %ret
; RV64I-NEXT:    ret
entry:
  br label %loop

loop:
  %i = phi i32 [ %inc, %loop ], [ 0, %entry ]
  %0 = load volatile i32, i32* @ie, align 4
  %inc = add nuw nsw i32 %i, 1
  %cmp = icmp slt i32 %inc, %n
  br i1 %cmp, label %loop, label %ret

ret:
  ret void
}

@gd = external thread_local global i32

define void @test_la_tls_gd(i32 signext %n) nounwind {
; RV32I-LABEL: test_la_tls_gd:
; RV32I:       # %bb.0: # %entry
; RV32I-NEXT:    addi sp, sp, -16
; RV32I-NEXT:    sw ra, 12(sp) # 4-byte Folded Spill
; RV32I-NEXT:    sw s0, 8(sp) # 4-byte Folded Spill
; RV32I-NEXT:    sw s1, 4(sp) # 4-byte Folded Spill
; RV32I-NEXT:    sw s2, 0(sp) # 4-byte Folded Spill
; RV32I-NEXT:    mv s0, a0
; RV32I-NEXT:    li s2, 0
; RV32I-NEXT:  .LBB3_3: # %entry
; RV32I-NEXT:    # Label of block must be emitted
; RV32I-NEXT:    auipc s1, %tls_gd_pcrel_hi(gd)
; RV32I-NEXT:    addi s1, s1, %pcrel_lo(.LBB3_3)
; RV32I-NEXT:  .LBB3_1: # %loop
; RV32I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV32I-NEXT:    mv a0, s1
; RV32I-NEXT:    call __tls_get_addr@plt
; RV32I-NEXT:    lw a0, 0(a0)
; RV32I-NEXT:    addi s2, s2, 1
; RV32I-NEXT:    blt s2, s0, .LBB3_1
; RV32I-NEXT:  # %bb.2: # %ret
; RV32I-NEXT:    lw ra, 12(sp) # 4-byte Folded Reload
; RV32I-NEXT:    lw s0, 8(sp) # 4-byte Folded Reload
; RV32I-NEXT:    lw s1, 4(sp) # 4-byte Folded Reload
; RV32I-NEXT:    lw s2, 0(sp) # 4-byte Folded Reload
; RV32I-NEXT:    addi sp, sp, 16
; RV32I-NEXT:    ret
;
; RV64I-LABEL: test_la_tls_gd:
; RV64I:       # %bb.0: # %entry
; RV64I-NEXT:    addi sp, sp, -32
; RV64I-NEXT:    sd ra, 24(sp) # 8-byte Folded Spill
; RV64I-NEXT:    sd s0, 16(sp) # 8-byte Folded Spill
; RV64I-NEXT:    sd s1, 8(sp) # 8-byte Folded Spill
; RV64I-NEXT:    sd s2, 0(sp) # 8-byte Folded Spill
; RV64I-NEXT:    mv s0, a0
; RV64I-NEXT:    li s2, 0
; RV64I-NEXT:  .LBB3_3: # %entry
; RV64I-NEXT:    # Label of block must be emitted
; RV64I-NEXT:    auipc s1, %tls_gd_pcrel_hi(gd)
; RV64I-NEXT:    addi s1, s1, %pcrel_lo(.LBB3_3)
; RV64I-NEXT:  .LBB3_1: # %loop
; RV64I-NEXT:    # =>This Inner Loop Header: Depth=1
; RV64I-NEXT:    mv a0, s1
; RV64I-NEXT:    call __tls_get_addr@plt
; RV64I-NEXT:    lw a0, 0(a0)
; RV64I-NEXT:    addiw s2, s2, 1
; RV64I-NEXT:    blt s2, s0, .LBB3_1
; RV64I-NEXT:  # %bb.2: # %ret
; RV64I-NEXT:    ld ra, 24(sp) # 8-byte Folded Reload
; RV64I-NEXT:    ld s0, 16(sp) # 8-byte Folded Reload
; RV64I-NEXT:    ld s1, 8(sp) # 8-byte Folded Reload
; RV64I-NEXT:    ld s2, 0(sp) # 8-byte Folded Reload
; RV64I-NEXT:    addi sp, sp, 32
; RV64I-NEXT:    ret
entry:
  br label %loop

loop:
  %i = phi i32 [ %inc, %loop ], [ 0, %entry ]
  %0 = load volatile i32, i32* @gd, align 4
  %inc = add nuw nsw i32 %i, 1
  %cmp = icmp slt i32 %inc, %n
  br i1 %cmp, label %loop, label %ret

ret:
  ret void
}
