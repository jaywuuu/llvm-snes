; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -march=amdgcn -mcpu=gfx1010 | FileCheck %s --check-prefix=GCN
; RUN: llc < %s -march=amdgcn -mcpu=gfx1100 | FileCheck %s --check-prefix=GFX11

define void @vgpr_descriptor_waterfall_loop_idom_update(<4 x i32>* %arg) #0 {
; GCN-LABEL: vgpr_descriptor_waterfall_loop_idom_update:
; GCN:       ; %bb.0: ; %entry
; GCN-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GCN-NEXT:    s_waitcnt_vscnt null, 0x0
; GCN-NEXT:    v_add_co_u32 v6, vcc_lo, v0, 8
; GCN-NEXT:    v_add_co_ci_u32_e32 v7, vcc_lo, 0, v1, vcc_lo
; GCN-NEXT:  .LBB0_1: ; %bb0
; GCN-NEXT:    ; =>This Loop Header: Depth=1
; GCN-NEXT:    ; Child Loop BB0_2 Depth 2
; GCN-NEXT:    s_clause 0x1
; GCN-NEXT:    flat_load_dwordx2 v[4:5], v[6:7]
; GCN-NEXT:    flat_load_dwordx2 v[2:3], v[0:1]
; GCN-NEXT:    s_mov_b32 s5, exec_lo
; GCN-NEXT:  .LBB0_2: ; Parent Loop BB0_1 Depth=1
; GCN-NEXT:    ; => This Inner Loop Header: Depth=2
; GCN-NEXT:    s_waitcnt vmcnt(0) lgkmcnt(0)
; GCN-NEXT:    v_readfirstlane_b32 s8, v2
; GCN-NEXT:    v_readfirstlane_b32 s9, v3
; GCN-NEXT:    v_readfirstlane_b32 s10, v4
; GCN-NEXT:    v_readfirstlane_b32 s11, v5
; GCN-NEXT:    v_cmp_eq_u64_e32 vcc_lo, s[8:9], v[2:3]
; GCN-NEXT:    v_cmp_eq_u64_e64 s4, s[10:11], v[4:5]
; GCN-NEXT:    s_and_b32 s4, vcc_lo, s4
; GCN-NEXT:    s_and_saveexec_b32 s4, s4
; GCN-NEXT:    buffer_store_dword v0, v0, s[8:11], 0 offen
; GCN-NEXT:    ; implicit-def: $vgpr2_vgpr3_vgpr4_vgpr5
; GCN-NEXT:    s_waitcnt_depctr 0xffe3
; GCN-NEXT:    s_xor_b32 exec_lo, exec_lo, s4
; GCN-NEXT:    s_cbranch_execnz .LBB0_2
; GCN-NEXT:  ; %bb.3: ; in Loop: Header=BB0_1 Depth=1
; GCN-NEXT:    s_mov_b32 exec_lo, s5
; GCN-NEXT:    s_branch .LBB0_1
;
; GFX11-LABEL: vgpr_descriptor_waterfall_loop_idom_update:
; GFX11:       ; %bb.0: ; %entry
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    .p2align 6
; GFX11-NEXT:  .LBB0_1: ; %bb0
; GFX11-NEXT:    ; =>This Loop Header: Depth=1
; GFX11-NEXT:    ; Child Loop BB0_2 Depth 2
; GFX11-NEXT:    flat_load_b128 v[2:5], v[0:1]
; GFX11-NEXT:    s_mov_b32 s1, exec_lo
; GFX11-NEXT:  .LBB0_2: ; Parent Loop BB0_1 Depth=1
; GFX11-NEXT:    ; => This Inner Loop Header: Depth=2
; GFX11-NEXT:    s_waitcnt vmcnt(0) lgkmcnt(0)
; GFX11-NEXT:    v_readfirstlane_b32 s4, v2
; GFX11-NEXT:    v_readfirstlane_b32 s5, v3
; GFX11-NEXT:    v_readfirstlane_b32 s6, v4
; GFX11-NEXT:    v_readfirstlane_b32 s7, v5
; GFX11-NEXT:    s_delay_alu instid0(VALU_DEP_3) | instskip(NEXT) | instid1(VALU_DEP_2)
; GFX11-NEXT:    v_cmp_eq_u64_e32 vcc_lo, s[4:5], v[2:3]
; GFX11-NEXT:    v_cmp_eq_u64_e64 s0, s[6:7], v[4:5]
; GFX11-NEXT:    s_delay_alu instid0(VALU_DEP_1) | instskip(NEXT) | instid1(SALU_CYCLE_1)
; GFX11-NEXT:    s_and_b32 s0, vcc_lo, s0
; GFX11-NEXT:    s_and_saveexec_b32 s0, s0
; GFX11-NEXT:    buffer_store_b32 v0, v0, s[4:7], 0 offen
; GFX11-NEXT:    ; implicit-def: $vgpr2_vgpr3_vgpr4_vgpr5
; GFX11-NEXT:    s_xor_b32 exec_lo, exec_lo, s0
; GFX11-NEXT:    s_cbranch_execnz .LBB0_2
; GFX11-NEXT:  ; %bb.3: ; in Loop: Header=BB0_1 Depth=1
; GFX11-NEXT:    s_mov_b32 exec_lo, s1
; GFX11-NEXT:    s_branch .LBB0_1
entry:
  br label %bb0

bb0:
  %desc = load <4 x i32>, <4 x i32>* %arg, align 8
  tail call void @llvm.amdgcn.raw.buffer.store.f32(float undef, <4 x i32> %desc, i32 undef, i32 0, i32 0)
  br label %bb0
}

declare void @llvm.amdgcn.raw.buffer.store.f32(float, <4 x i32>, i32, i32, i32 immarg) #0

attributes #0 = { nounwind writeonly }
