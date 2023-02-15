; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -global-isel -mtriple=amdgcn-amd-amdpal -mcpu=hawaii -verify-machineinstrs < %s | FileCheck -check-prefixes=GCN,GFX7 %s
; RUN: llc -global-isel -mtriple=amdgcn-amd-amdpal -mcpu=fiji -verify-machineinstrs < %s | FileCheck -check-prefixes=GCN,GFX8 %s
; RUN: llc -global-isel -mtriple=amdgcn-amd-amdpal -mcpu=gfx900 -verify-machineinstrs < %s | FileCheck -check-prefixes=GCN,GFX9 %s
; RUN: llc -global-isel -mtriple=amdgcn-amd-amdpal -mcpu=gfx1010 -verify-machineinstrs < %s | FileCheck -check-prefixes=GFX10PLUS,GFX10 %s
; RUN: llc -global-isel -mtriple=amdgcn-amd-amdpal -mcpu=gfx1100 -amdgpu-enable-delay-alu=0 -verify-machineinstrs < %s | FileCheck -check-prefixes=GFX10PLUS,GFX11 %s

; Test optimization to reduce shifts to narrower sizes.

define amdgpu_ps i64 @s_shl_i64_zext_i32(i32 inreg %x) {
; GCN-LABEL: s_shl_i64_zext_i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_andn2_b32 s0, s0, -2.0
; GCN-NEXT:    s_lshl_b32 s0, s0, 2
; GCN-NEXT:    s_mov_b32 s1, 0
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10-LABEL: s_shl_i64_zext_i32:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_andn2_b32 s0, s0, -2.0
; GFX10-NEXT:    s_mov_b32 s1, 0
; GFX10-NEXT:    s_lshl_b32 s0, s0, 2
; GFX10-NEXT:    ; return to shader part epilog
;
; GFX11-LABEL: s_shl_i64_zext_i32:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_and_not1_b32 s0, s0, -2.0
; GFX11-NEXT:    s_mov_b32 s1, 0
; GFX11-NEXT:    s_lshl_b32 s0, s0, 2
; GFX11-NEXT:    ; return to shader part epilog
  %and = and i32 %x, 1073741823
  %ext = zext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define i64 @v_shl_i64_zext_i32(i32 %x) {
; GCN-LABEL: v_shl_i64_zext_i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GCN-NEXT:    v_and_b32_e32 v0, 0x3fffffff, v0
; GCN-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GCN-NEXT:    v_mov_b32_e32 v1, 0
; GCN-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10-LABEL: v_shl_i64_zext_i32:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10-NEXT:    v_and_b32_e32 v0, 0x3fffffff, v0
; GFX10-NEXT:    v_mov_b32_e32 v1, 0
; GFX10-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX10-NEXT:    s_setpc_b64 s[30:31]
;
; GFX11-LABEL: v_shl_i64_zext_i32:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    v_dual_mov_b32 v1, 0 :: v_dual_and_b32 v0, 0x3fffffff, v0
; GFX11-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX11-NEXT:    s_setpc_b64 s[30:31]
  %and = and i32 %x, 1073741823
  %ext = zext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define amdgpu_ps i64 @s_shl_i64_sext_i32(i32 inreg %x) {
; GCN-LABEL: s_shl_i64_sext_i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_and_b32 s0, s0, 0x1fffffff
; GCN-NEXT:    s_lshl_b32 s0, s0, 2
; GCN-NEXT:    s_mov_b32 s1, 0
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_i64_sext_i32:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_and_b32 s0, s0, 0x1fffffff
; GFX10PLUS-NEXT:    s_mov_b32 s1, 0
; GFX10PLUS-NEXT:    s_lshl_b32 s0, s0, 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and i32 %x, 536870911
  %ext = sext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define i64 @v_shl_i64_sext_i32(i32 %x) {
; GCN-LABEL: v_shl_i64_sext_i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GCN-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GCN-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GCN-NEXT:    v_mov_b32_e32 v1, 0
; GCN-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10-LABEL: v_shl_i64_sext_i32:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GFX10-NEXT:    v_mov_b32_e32 v1, 0
; GFX10-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX10-NEXT:    s_setpc_b64 s[30:31]
;
; GFX11-LABEL: v_shl_i64_sext_i32:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    v_dual_mov_b32 v1, 0 :: v_dual_and_b32 v0, 0x1fffffff, v0
; GFX11-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX11-NEXT:    s_setpc_b64 s[30:31]
  %and = and i32 %x, 536870911
  %ext = sext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define amdgpu_ps i64 @s_shl_i64_zext_i32_overflow(i32 inreg %x) {
; GCN-LABEL: s_shl_i64_zext_i32_overflow:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_bitset0_b32 s0, 31
; GCN-NEXT:    s_bfe_u64 s[0:1], s[0:1], 0x200000
; GCN-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_i64_zext_i32_overflow:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_bitset0_b32 s0, 31
; GFX10PLUS-NEXT:    s_bfe_u64 s[0:1], s[0:1], 0x200000
; GFX10PLUS-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and i32 %x, 2147483647
  %ext = zext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define i64 @v_shl_i64_zext_i32_overflow(i32 %x) {
; GFX7-LABEL: v_shl_i64_zext_i32_overflow:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX7-NEXT:    v_mov_b32_e32 v1, 0
; GFX7-NEXT:    v_lshl_b64 v[0:1], v[0:1], 2
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_i64_zext_i32_overflow:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX8-NEXT:    v_mov_b32_e32 v1, 0
; GFX8-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_i64_zext_i32_overflow:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX9-NEXT:    v_mov_b32_e32 v1, 0
; GFX9-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10-LABEL: v_shl_i64_zext_i32_overflow:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10-NEXT:    v_mov_b32_e32 v1, 0
; GFX10-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX10-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX10-NEXT:    s_setpc_b64 s[30:31]
;
; GFX11-LABEL: v_shl_i64_zext_i32_overflow:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    v_dual_mov_b32 v1, 0 :: v_dual_and_b32 v0, 0x7fffffff, v0
; GFX11-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX11-NEXT:    s_setpc_b64 s[30:31]
  %and = and i32 %x, 2147483647
  %ext = zext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define amdgpu_ps i64 @s_shl_i64_sext_i32_overflow(i32 inreg %x) {
; GCN-LABEL: s_shl_i64_sext_i32_overflow:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_bitset0_b32 s0, 31
; GCN-NEXT:    s_bfe_i64 s[0:1], s[0:1], 0x200000
; GCN-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_i64_sext_i32_overflow:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_bitset0_b32 s0, 31
; GFX10PLUS-NEXT:    s_bfe_i64 s[0:1], s[0:1], 0x200000
; GFX10PLUS-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and i32 %x, 2147483647
  %ext = sext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define i64 @v_shl_i64_sext_i32_overflow(i32 %x) {
; GFX7-LABEL: v_shl_i64_sext_i32_overflow:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX7-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX7-NEXT:    v_lshl_b64 v[0:1], v[0:1], 2
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_i64_sext_i32_overflow:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX8-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX8-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_i64_sext_i32_overflow:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX9-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX9-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10PLUS-LABEL: v_shl_i64_sext_i32_overflow:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10PLUS-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10PLUS-NEXT:    v_and_b32_e32 v0, 0x7fffffff, v0
; GFX10PLUS-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX10PLUS-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX10PLUS-NEXT:    s_setpc_b64 s[30:31]
  %and = and i32 %x, 2147483647
  %ext = sext i32 %and to i64
  %shl = shl i64 %ext, 2
  ret i64 %shl
}

define amdgpu_kernel void @mulu24_shl64(i32 addrspace(1)* nocapture %arg) {
; GFX7-LABEL: mulu24_shl64:
; GFX7:       ; %bb.0: ; %bb
; GFX7-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x0
; GFX7-NEXT:    v_and_b32_e32 v0, 6, v0
; GFX7-NEXT:    v_mul_u32_u24_e32 v0, 7, v0
; GFX7-NEXT:    v_mov_b32_e32 v1, 0
; GFX7-NEXT:    v_lshl_b64 v[2:3], v[0:1], 2
; GFX7-NEXT:    s_mov_b32 s2, 0
; GFX7-NEXT:    s_mov_b32 s3, 0xf000
; GFX7-NEXT:    s_waitcnt lgkmcnt(0)
; GFX7-NEXT:    buffer_store_dword v1, v[2:3], s[0:3], 0 addr64
; GFX7-NEXT:    s_endpgm
;
; GFX8-LABEL: mulu24_shl64:
; GFX8:       ; %bb.0: ; %bb
; GFX8-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x0
; GFX8-NEXT:    v_and_b32_e32 v0, 6, v0
; GFX8-NEXT:    v_mul_u32_u24_e32 v0, 7, v0
; GFX8-NEXT:    v_mov_b32_e32 v1, 0
; GFX8-NEXT:    v_lshlrev_b64 v[2:3], 2, v[0:1]
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_mov_b32_e32 v5, s1
; GFX8-NEXT:    v_mov_b32_e32 v4, s0
; GFX8-NEXT:    v_add_u32_e32 v2, vcc, v4, v2
; GFX8-NEXT:    v_addc_u32_e32 v3, vcc, v5, v3, vcc
; GFX8-NEXT:    flat_store_dword v[2:3], v1
; GFX8-NEXT:    s_endpgm
;
; GFX9-LABEL: mulu24_shl64:
; GFX9:       ; %bb.0: ; %bb
; GFX9-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x0
; GFX9-NEXT:    v_and_b32_e32 v0, 6, v0
; GFX9-NEXT:    v_mul_u32_u24_e32 v0, 7, v0
; GFX9-NEXT:    v_mov_b32_e32 v1, 0
; GFX9-NEXT:    v_lshlrev_b64 v[2:3], 2, v[0:1]
; GFX9-NEXT:    s_waitcnt lgkmcnt(0)
; GFX9-NEXT:    v_mov_b32_e32 v5, s1
; GFX9-NEXT:    v_mov_b32_e32 v4, s0
; GFX9-NEXT:    v_add_co_u32_e32 v2, vcc, v4, v2
; GFX9-NEXT:    v_addc_co_u32_e32 v3, vcc, v5, v3, vcc
; GFX9-NEXT:    global_store_dword v[2:3], v1, off
; GFX9-NEXT:    s_endpgm
;
; GFX10-LABEL: mulu24_shl64:
; GFX10:       ; %bb.0: ; %bb
; GFX10-NEXT:    s_load_dwordx2 s[0:1], s[0:1], 0x0
; GFX10-NEXT:    v_and_b32_e32 v0, 6, v0
; GFX10-NEXT:    v_mov_b32_e32 v1, 0
; GFX10-NEXT:    v_mul_u32_u24_e32 v0, 7, v0
; GFX10-NEXT:    v_lshlrev_b64 v[2:3], 2, v[0:1]
; GFX10-NEXT:    s_waitcnt lgkmcnt(0)
; GFX10-NEXT:    v_mov_b32_e32 v5, s1
; GFX10-NEXT:    v_mov_b32_e32 v4, s0
; GFX10-NEXT:    v_add_co_u32 v2, vcc_lo, v4, v2
; GFX10-NEXT:    v_add_co_ci_u32_e32 v3, vcc_lo, v5, v3, vcc_lo
; GFX10-NEXT:    global_store_dword v[2:3], v1, off
; GFX10-NEXT:    s_endpgm
;
; GFX11-LABEL: mulu24_shl64:
; GFX11:       ; %bb.0: ; %bb
; GFX11-NEXT:    s_load_b64 s[0:1], s[0:1], 0x0
; GFX11-NEXT:    v_dual_mov_b32 v1, 0 :: v_dual_and_b32 v0, 6, v0
; GFX11-NEXT:    v_mul_u32_u24_e32 v0, 7, v0
; GFX11-NEXT:    v_lshlrev_b64 v[2:3], 2, v[0:1]
; GFX11-NEXT:    s_waitcnt lgkmcnt(0)
; GFX11-NEXT:    v_dual_mov_b32 v5, s1 :: v_dual_mov_b32 v4, s0
; GFX11-NEXT:    v_add_co_u32 v2, vcc_lo, v4, v2
; GFX11-NEXT:    v_add_co_ci_u32_e32 v3, vcc_lo, v5, v3, vcc_lo
; GFX11-NEXT:    global_store_b32 v[2:3], v1, off
; GFX11-NEXT:    s_sendmsg sendmsg(MSG_DEALLOC_VGPRS)
; GFX11-NEXT:    s_endpgm
bb:
  %tmp = tail call i32 @llvm.amdgcn.workitem.id.x()
  %tmp1 = and i32 %tmp, 6
  %mulconv = mul nuw nsw i32 %tmp1, 7
  %tmp2 = zext i32 %mulconv to i64
  %tmp3 = getelementptr inbounds i32, i32 addrspace(1)* %arg, i64 %tmp2
  store i32 0, i32 addrspace(1)* %tmp3, align 4
  ret void
}

define amdgpu_kernel void @muli24_shl64(i64 addrspace(1)* nocapture %arg, i32 addrspace(1)* nocapture readonly %arg1) {
; GFX7-LABEL: muli24_shl64:
; GFX7:       ; %bb.0: ; %bb
; GFX7-NEXT:    s_load_dwordx4 s[0:3], s[0:1], 0x0
; GFX7-NEXT:    v_lshlrev_b32_e32 v1, 2, v0
; GFX7-NEXT:    v_mov_b32_e32 v2, 0
; GFX7-NEXT:    s_mov_b32 s6, 0
; GFX7-NEXT:    s_mov_b32 s7, 0xf000
; GFX7-NEXT:    s_waitcnt lgkmcnt(0)
; GFX7-NEXT:    s_mov_b64 s[4:5], s[2:3]
; GFX7-NEXT:    buffer_load_dword v1, v[1:2], s[4:7], 0 addr64
; GFX7-NEXT:    v_lshlrev_b32_e32 v5, 3, v0
; GFX7-NEXT:    v_mov_b32_e32 v4, s1
; GFX7-NEXT:    v_mov_b32_e32 v3, s0
; GFX7-NEXT:    s_waitcnt vmcnt(0)
; GFX7-NEXT:    v_or_b32_e32 v0, 0xff800000, v1
; GFX7-NEXT:    v_mul_i32_i24_e32 v1, -7, v0
; GFX7-NEXT:    v_lshl_b64 v[0:1], v[1:2], 3
; GFX7-NEXT:    v_add_i32_e32 v2, vcc, v3, v5
; GFX7-NEXT:    v_addc_u32_e32 v3, vcc, 0, v4, vcc
; GFX7-NEXT:    flat_store_dwordx2 v[2:3], v[0:1]
; GFX7-NEXT:    s_endpgm
;
; GFX8-LABEL: muli24_shl64:
; GFX8:       ; %bb.0: ; %bb
; GFX8-NEXT:    s_load_dwordx4 s[0:3], s[0:1], 0x0
; GFX8-NEXT:    v_lshlrev_b32_e32 v3, 2, v0
; GFX8-NEXT:    v_lshlrev_b32_e32 v5, 3, v0
; GFX8-NEXT:    s_waitcnt lgkmcnt(0)
; GFX8-NEXT:    v_mov_b32_e32 v1, s2
; GFX8-NEXT:    v_mov_b32_e32 v2, s3
; GFX8-NEXT:    v_add_u32_e32 v1, vcc, v1, v3
; GFX8-NEXT:    v_addc_u32_e32 v2, vcc, 0, v2, vcc
; GFX8-NEXT:    flat_load_dword v4, v[1:2]
; GFX8-NEXT:    v_mov_b32_e32 v3, s1
; GFX8-NEXT:    v_mov_b32_e32 v1, 0
; GFX8-NEXT:    v_mov_b32_e32 v2, s0
; GFX8-NEXT:    v_add_u32_e32 v2, vcc, v2, v5
; GFX8-NEXT:    v_addc_u32_e32 v3, vcc, 0, v3, vcc
; GFX8-NEXT:    s_waitcnt vmcnt(0)
; GFX8-NEXT:    v_or_b32_e32 v0, 0xff800000, v4
; GFX8-NEXT:    v_mul_i32_i24_e32 v0, -7, v0
; GFX8-NEXT:    v_lshlrev_b64 v[0:1], 3, v[0:1]
; GFX8-NEXT:    flat_store_dwordx2 v[2:3], v[0:1]
; GFX8-NEXT:    s_endpgm
;
; GFX9-LABEL: muli24_shl64:
; GFX9:       ; %bb.0: ; %bb
; GFX9-NEXT:    s_load_dwordx4 s[0:3], s[0:1], 0x0
; GFX9-NEXT:    v_lshlrev_b32_e32 v1, 2, v0
; GFX9-NEXT:    v_mov_b32_e32 v2, 0
; GFX9-NEXT:    v_lshlrev_b32_e32 v0, 3, v0
; GFX9-NEXT:    s_waitcnt lgkmcnt(0)
; GFX9-NEXT:    global_load_dword v1, v1, s[2:3]
; GFX9-NEXT:    s_waitcnt vmcnt(0)
; GFX9-NEXT:    v_or_b32_e32 v1, 0xff800000, v1
; GFX9-NEXT:    v_mul_i32_i24_e32 v1, -7, v1
; GFX9-NEXT:    v_lshlrev_b64 v[1:2], 3, v[1:2]
; GFX9-NEXT:    global_store_dwordx2 v0, v[1:2], s[0:1]
; GFX9-NEXT:    s_endpgm
;
; GFX10-LABEL: muli24_shl64:
; GFX10:       ; %bb.0: ; %bb
; GFX10-NEXT:    s_load_dwordx4 s[0:3], s[0:1], 0x0
; GFX10-NEXT:    v_lshlrev_b32_e32 v1, 2, v0
; GFX10-NEXT:    v_mov_b32_e32 v2, 0
; GFX10-NEXT:    v_lshlrev_b32_e32 v0, 3, v0
; GFX10-NEXT:    s_waitcnt lgkmcnt(0)
; GFX10-NEXT:    global_load_dword v1, v1, s[2:3]
; GFX10-NEXT:    s_waitcnt vmcnt(0)
; GFX10-NEXT:    v_or_b32_e32 v1, 0xff800000, v1
; GFX10-NEXT:    v_mul_i32_i24_e32 v1, -7, v1
; GFX10-NEXT:    v_lshlrev_b64 v[1:2], 3, v[1:2]
; GFX10-NEXT:    global_store_dwordx2 v0, v[1:2], s[0:1]
; GFX10-NEXT:    s_endpgm
;
; GFX11-LABEL: muli24_shl64:
; GFX11:       ; %bb.0: ; %bb
; GFX11-NEXT:    s_load_b128 s[0:3], s[0:1], 0x0
; GFX11-NEXT:    v_dual_mov_b32 v2, 0 :: v_dual_lshlrev_b32 v1, 2, v0
; GFX11-NEXT:    v_lshlrev_b32_e32 v0, 3, v0
; GFX11-NEXT:    s_waitcnt lgkmcnt(0)
; GFX11-NEXT:    global_load_b32 v1, v1, s[2:3]
; GFX11-NEXT:    s_waitcnt vmcnt(0)
; GFX11-NEXT:    v_or_b32_e32 v1, 0xff800000, v1
; GFX11-NEXT:    v_mul_i32_i24_e32 v1, -7, v1
; GFX11-NEXT:    v_lshlrev_b64 v[1:2], 3, v[1:2]
; GFX11-NEXT:    global_store_b64 v0, v[1:2], s[0:1]
; GFX11-NEXT:    s_sendmsg sendmsg(MSG_DEALLOC_VGPRS)
; GFX11-NEXT:    s_endpgm
bb:
  %tmp = tail call i32 @llvm.amdgcn.workitem.id.x()
  %tmp2 = sext i32 %tmp to i64
  %tmp3 = getelementptr inbounds i32, i32 addrspace(1)* %arg1, i64 %tmp2
  %tmp4 = load i32, i32 addrspace(1)* %tmp3, align 4
  %tmp5 = or i32 %tmp4, -8388608
  %tmp6 = mul nsw i32 %tmp5, -7
  %tmp7 = zext i32 %tmp6 to i64
  %tmp8 = shl nuw nsw i64 %tmp7, 3
  %tmp9 = getelementptr inbounds i64, i64 addrspace(1)* %arg, i64 %tmp2
  store i64 %tmp8, i64 addrspace(1)* %tmp9, align 8
  ret void
}

define amdgpu_ps <2 x i64> @s_shl_v2i64_zext_v2i32(<2 x i32> inreg %x) {
; GCN-LABEL: s_shl_v2i64_zext_v2i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_brev_b32 s2, -4
; GCN-NEXT:    s_mov_b32 s3, s2
; GCN-NEXT:    s_and_b64 s[0:1], s[0:1], s[2:3]
; GCN-NEXT:    s_bfe_u64 s[2:3], s[0:1], 0x200000
; GCN-NEXT:    s_mov_b32 s0, s1
; GCN-NEXT:    s_bfe_u64 s[4:5], s[0:1], 0x200000
; GCN-NEXT:    s_lshl_b64 s[0:1], s[2:3], 2
; GCN-NEXT:    s_lshl_b64 s[2:3], s[4:5], 2
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_v2i64_zext_v2i32:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_brev_b32 s2, -4
; GFX10PLUS-NEXT:    s_mov_b32 s3, s2
; GFX10PLUS-NEXT:    s_and_b64 s[0:1], s[0:1], s[2:3]
; GFX10PLUS-NEXT:    s_mov_b32 s2, s1
; GFX10PLUS-NEXT:    s_bfe_u64 s[0:1], s[0:1], 0x200000
; GFX10PLUS-NEXT:    s_bfe_u64 s[2:3], s[2:3], 0x200000
; GFX10PLUS-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GFX10PLUS-NEXT:    s_lshl_b64 s[2:3], s[2:3], 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and <2 x i32> %x, <i32 1073741823, i32 1073741823>
  %ext = zext <2 x i32> %and to <2 x i64>
  %shl = shl <2 x i64> %ext, <i64 2, i64 2>
  ret <2 x i64> %shl
}

define <2 x i64> @v_shl_v2i64_zext_v2i32(<2 x i32> %x) {
; GFX7-LABEL: v_shl_v2i64_zext_v2i32:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_and_b32_e32 v2, 0x3fffffff, v1
; GFX7-NEXT:    v_mov_b32_e32 v1, 0
; GFX7-NEXT:    v_and_b32_e32 v0, 0x3fffffff, v0
; GFX7-NEXT:    v_mov_b32_e32 v3, v1
; GFX7-NEXT:    v_lshl_b64 v[0:1], v[0:1], 2
; GFX7-NEXT:    v_lshl_b64 v[2:3], v[2:3], 2
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_v2i64_zext_v2i32:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v2, 0x3fffffff, v1
; GFX8-NEXT:    v_mov_b32_e32 v1, 0
; GFX8-NEXT:    v_and_b32_e32 v0, 0x3fffffff, v0
; GFX8-NEXT:    v_mov_b32_e32 v3, v1
; GFX8-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX8-NEXT:    v_lshlrev_b64 v[2:3], 2, v[2:3]
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_v2i64_zext_v2i32:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    v_and_b32_e32 v2, 0x3fffffff, v1
; GFX9-NEXT:    v_mov_b32_e32 v1, 0
; GFX9-NEXT:    v_and_b32_e32 v0, 0x3fffffff, v0
; GFX9-NEXT:    v_mov_b32_e32 v3, v1
; GFX9-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX9-NEXT:    v_lshlrev_b64 v[2:3], 2, v[2:3]
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10-LABEL: v_shl_v2i64_zext_v2i32:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10-NEXT:    v_mov_b32_e32 v3, 0
; GFX10-NEXT:    v_and_b32_e32 v2, 0x3fffffff, v0
; GFX10-NEXT:    v_and_b32_e32 v4, 0x3fffffff, v1
; GFX10-NEXT:    v_mov_b32_e32 v5, v3
; GFX10-NEXT:    v_lshlrev_b64 v[0:1], 2, v[2:3]
; GFX10-NEXT:    v_lshlrev_b64 v[2:3], 2, v[4:5]
; GFX10-NEXT:    s_setpc_b64 s[30:31]
;
; GFX11-LABEL: v_shl_v2i64_zext_v2i32:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    v_dual_mov_b32 v3, 0 :: v_dual_and_b32 v2, 0x3fffffff, v0
; GFX11-NEXT:    v_dual_mov_b32 v5, v3 :: v_dual_and_b32 v4, 0x3fffffff, v1
; GFX11-NEXT:    v_lshlrev_b64 v[0:1], 2, v[2:3]
; GFX11-NEXT:    v_lshlrev_b64 v[2:3], 2, v[4:5]
; GFX11-NEXT:    s_setpc_b64 s[30:31]
  %and = and <2 x i32> %x, <i32 1073741823, i32 1073741823>
  %ext = zext <2 x i32> %and to <2 x i64>
  %shl = shl <2 x i64> %ext, <i64 2, i64 2>
  ret <2 x i64> %shl
}

define amdgpu_ps <2 x i64> @s_shl_v2i64_sext_v2i32(<2 x i32> inreg %x) {
; GCN-LABEL: s_shl_v2i64_sext_v2i32:
; GCN:       ; %bb.0:
; GCN-NEXT:    s_brev_b32 s2, -8
; GCN-NEXT:    s_mov_b32 s3, s2
; GCN-NEXT:    s_and_b64 s[0:1], s[0:1], s[2:3]
; GCN-NEXT:    s_bfe_i64 s[2:3], s[0:1], 0x200000
; GCN-NEXT:    s_mov_b32 s0, s1
; GCN-NEXT:    s_bfe_i64 s[4:5], s[0:1], 0x200000
; GCN-NEXT:    s_lshl_b64 s[0:1], s[2:3], 2
; GCN-NEXT:    s_lshl_b64 s[2:3], s[4:5], 2
; GCN-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_v2i64_sext_v2i32:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_brev_b32 s2, -8
; GFX10PLUS-NEXT:    s_mov_b32 s3, s2
; GFX10PLUS-NEXT:    s_and_b64 s[0:1], s[0:1], s[2:3]
; GFX10PLUS-NEXT:    s_mov_b32 s2, s1
; GFX10PLUS-NEXT:    s_bfe_i64 s[0:1], s[0:1], 0x200000
; GFX10PLUS-NEXT:    s_bfe_i64 s[2:3], s[2:3], 0x200000
; GFX10PLUS-NEXT:    s_lshl_b64 s[0:1], s[0:1], 2
; GFX10PLUS-NEXT:    s_lshl_b64 s[2:3], s[2:3], 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and <2 x i32> %x, <i32 536870911, i32 536870911>
  %ext = sext <2 x i32> %and to <2 x i64>
  %shl = shl <2 x i64> %ext, <i64 2, i64 2>
  ret <2 x i64> %shl
}

define <2 x i64> @v_shl_v2i64_sext_v2i32(<2 x i32> %x) {
; GFX7-LABEL: v_shl_v2i64_sext_v2i32:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GFX7-NEXT:    v_and_b32_e32 v2, 0x1fffffff, v1
; GFX7-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX7-NEXT:    v_ashrrev_i32_e32 v3, 31, v2
; GFX7-NEXT:    v_lshl_b64 v[0:1], v[0:1], 2
; GFX7-NEXT:    v_lshl_b64 v[2:3], v[2:3], 2
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_v2i64_sext_v2i32:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GFX8-NEXT:    v_and_b32_e32 v2, 0x1fffffff, v1
; GFX8-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX8-NEXT:    v_ashrrev_i32_e32 v3, 31, v2
; GFX8-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX8-NEXT:    v_lshlrev_b64 v[2:3], 2, v[2:3]
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_v2i64_sext_v2i32:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GFX9-NEXT:    v_and_b32_e32 v2, 0x1fffffff, v1
; GFX9-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX9-NEXT:    v_ashrrev_i32_e32 v3, 31, v2
; GFX9-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX9-NEXT:    v_lshlrev_b64 v[2:3], 2, v[2:3]
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10PLUS-LABEL: v_shl_v2i64_sext_v2i32:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10PLUS-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10PLUS-NEXT:    v_and_b32_e32 v0, 0x1fffffff, v0
; GFX10PLUS-NEXT:    v_and_b32_e32 v2, 0x1fffffff, v1
; GFX10PLUS-NEXT:    v_ashrrev_i32_e32 v1, 31, v0
; GFX10PLUS-NEXT:    v_ashrrev_i32_e32 v3, 31, v2
; GFX10PLUS-NEXT:    v_lshlrev_b64 v[0:1], 2, v[0:1]
; GFX10PLUS-NEXT:    v_lshlrev_b64 v[2:3], 2, v[2:3]
; GFX10PLUS-NEXT:    s_setpc_b64 s[30:31]
  %and = and <2 x i32> %x, <i32 536870911, i32 536870911>
  %ext = sext <2 x i32> %and to <2 x i64>
  %shl = shl <2 x i64> %ext, <i64 2, i64 2>
  ret <2 x i64> %shl
}

define amdgpu_ps i32 @s_shl_i32_zext_i16(i16 inreg %x) {
; GFX7-LABEL: s_shl_i32_zext_i16:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_and_b32 s0, s0, 0x3fff
; GFX7-NEXT:    s_lshl_b32 s0, s0, 2
; GFX7-NEXT:    s_and_b32 s0, s0, 0xffff
; GFX7-NEXT:    ; return to shader part epilog
;
; GFX8-LABEL: s_shl_i32_zext_i16:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_and_b32 s0, s0, 0x3fff
; GFX8-NEXT:    s_lshl_b32 s0, s0, 2
; GFX8-NEXT:    ; return to shader part epilog
;
; GFX9-LABEL: s_shl_i32_zext_i16:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_and_b32 s0, s0, 0x3fff
; GFX9-NEXT:    s_lshl_b32 s0, s0, 2
; GFX9-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_i32_zext_i16:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_and_b32 s0, s0, 0x3fff
; GFX10PLUS-NEXT:    s_lshl_b32 s0, s0, 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and i16 %x, 16383
  %ext = zext i16 %and to i32
  %shl = shl i32 %ext, 2
  ret i32 %shl
}

define i32 @v_shl_i32_zext_i16(i16 %x) {
; GFX7-LABEL: v_shl_i32_zext_i16:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_and_b32_e32 v0, 0x3fff, v0
; GFX7-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX7-NEXT:    v_and_b32_e32 v0, 0xffff, v0
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_i32_zext_i16:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v0, 0x3fff, v0
; GFX8-NEXT:    v_lshlrev_b16_e32 v0, 2, v0
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_i32_zext_i16:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    v_and_b32_e32 v0, 0x3fff, v0
; GFX9-NEXT:    v_lshlrev_b16_e32 v0, 2, v0
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10PLUS-LABEL: v_shl_i32_zext_i16:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10PLUS-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10PLUS-NEXT:    v_and_b32_e32 v0, 0x3fff, v0
; GFX10PLUS-NEXT:    v_lshlrev_b16 v0, 2, v0
; GFX10PLUS-NEXT:    v_bfe_u32 v0, v0, 0, 16
; GFX10PLUS-NEXT:    s_setpc_b64 s[30:31]
  %and = and i16 %x, 16383
  %ext = zext i16 %and to i32
  %shl = shl i32 %ext, 2
  ret i32 %shl
}

define amdgpu_ps <2 x i32> @s_shl_v2i32_zext_v2i16(<2 x i16> inreg %x) {
; GFX7-LABEL: s_shl_v2i32_zext_v2i16:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_lshl_b32 s1, s1, 16
; GFX7-NEXT:    s_and_b32 s0, s0, 0xffff
; GFX7-NEXT:    s_or_b32 s0, s1, s0
; GFX7-NEXT:    s_and_b32 s0, s0, 0x3fff3fff
; GFX7-NEXT:    s_lshr_b32 s1, s0, 16
; GFX7-NEXT:    s_and_b32 s0, s0, 0xffff
; GFX7-NEXT:    s_lshl_b32 s0, s0, 2
; GFX7-NEXT:    s_lshl_b32 s1, s1, 2
; GFX7-NEXT:    ; return to shader part epilog
;
; GFX8-LABEL: s_shl_v2i32_zext_v2i16:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_movk_i32 s2, 0x3fff
; GFX8-NEXT:    s_mov_b32 s3, s2
; GFX8-NEXT:    s_lshr_b32 s1, s0, 16
; GFX8-NEXT:    s_and_b32 s0, s0, 0xffff
; GFX8-NEXT:    s_and_b64 s[0:1], s[0:1], s[2:3]
; GFX8-NEXT:    s_lshl_b32 s0, s0, 2
; GFX8-NEXT:    s_lshl_b32 s1, s1, 2
; GFX8-NEXT:    ; return to shader part epilog
;
; GFX9-LABEL: s_shl_v2i32_zext_v2i16:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_and_b32 s0, s0, 0x3fff3fff
; GFX9-NEXT:    s_lshr_b32 s1, s0, 16
; GFX9-NEXT:    s_and_b32 s0, s0, 0xffff
; GFX9-NEXT:    s_lshl_b32 s0, s0, 2
; GFX9-NEXT:    s_lshl_b32 s1, s1, 2
; GFX9-NEXT:    ; return to shader part epilog
;
; GFX10PLUS-LABEL: s_shl_v2i32_zext_v2i16:
; GFX10PLUS:       ; %bb.0:
; GFX10PLUS-NEXT:    s_and_b32 s0, s0, 0x3fff3fff
; GFX10PLUS-NEXT:    s_and_b32 s1, s0, 0xffff
; GFX10PLUS-NEXT:    s_lshr_b32 s2, s0, 16
; GFX10PLUS-NEXT:    s_lshl_b32 s0, s1, 2
; GFX10PLUS-NEXT:    s_lshl_b32 s1, s2, 2
; GFX10PLUS-NEXT:    ; return to shader part epilog
  %and = and <2 x i16> %x, <i16 16383, i16 16383>
  %ext = zext <2 x i16> %and to <2 x i32>
  %shl = shl <2 x i32> %ext, <i32 2, i32 2>
  ret <2 x i32> %shl
}

; FIXME: This doesn't do what we want. The pre-legalizer combiner
; fails to handle the vector splat. The post-legalizer sees the zext
; legalized into the and. This is probably not that important, since
; we really do this combine in the machine level for lowered
; getelementptrs.
define <2 x i32> @v_shl_v2i32_zext_v2i16(<2 x i16> %x) {
; GFX7-LABEL: v_shl_v2i32_zext_v2i16:
; GFX7:       ; %bb.0:
; GFX7-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX7-NEXT:    v_lshlrev_b32_e32 v1, 16, v1
; GFX7-NEXT:    v_and_b32_e32 v0, 0xffff, v0
; GFX7-NEXT:    v_or_b32_e32 v0, v1, v0
; GFX7-NEXT:    v_and_b32_e32 v0, 0x3fff3fff, v0
; GFX7-NEXT:    v_lshrrev_b32_e32 v1, 16, v0
; GFX7-NEXT:    v_and_b32_e32 v0, 0xffff, v0
; GFX7-NEXT:    v_lshlrev_b32_e32 v0, 2, v0
; GFX7-NEXT:    v_lshlrev_b32_e32 v1, 2, v1
; GFX7-NEXT:    s_setpc_b64 s[30:31]
;
; GFX8-LABEL: v_shl_v2i32_zext_v2i16:
; GFX8:       ; %bb.0:
; GFX8-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX8-NEXT:    v_and_b32_e32 v1, 0x3fff3fff, v0
; GFX8-NEXT:    v_mov_b32_e32 v2, 2
; GFX8-NEXT:    v_lshlrev_b32_sdwa v0, v2, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_0
; GFX8-NEXT:    v_lshlrev_b32_sdwa v1, v2, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_1
; GFX8-NEXT:    s_setpc_b64 s[30:31]
;
; GFX9-LABEL: v_shl_v2i32_zext_v2i16:
; GFX9:       ; %bb.0:
; GFX9-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX9-NEXT:    s_mov_b32 s4, 2
; GFX9-NEXT:    v_and_b32_e32 v1, 0x3fff3fff, v0
; GFX9-NEXT:    v_lshlrev_b32_sdwa v0, s4, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_0
; GFX9-NEXT:    v_lshlrev_b32_sdwa v1, s4, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_1
; GFX9-NEXT:    s_setpc_b64 s[30:31]
;
; GFX10-LABEL: v_shl_v2i32_zext_v2i16:
; GFX10:       ; %bb.0:
; GFX10-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX10-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX10-NEXT:    v_and_b32_e32 v1, 0x3fff3fff, v0
; GFX10-NEXT:    s_mov_b32 s4, 2
; GFX10-NEXT:    v_lshlrev_b32_sdwa v0, s4, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_0
; GFX10-NEXT:    v_lshlrev_b32_sdwa v1, s4, v1 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:DWORD src1_sel:WORD_1
; GFX10-NEXT:    s_setpc_b64 s[30:31]
;
; GFX11-LABEL: v_shl_v2i32_zext_v2i16:
; GFX11:       ; %bb.0:
; GFX11-NEXT:    s_waitcnt vmcnt(0) expcnt(0) lgkmcnt(0)
; GFX11-NEXT:    s_waitcnt_vscnt null, 0x0
; GFX11-NEXT:    v_and_b32_e32 v0, 0x3fff3fff, v0
; GFX11-NEXT:    v_and_b32_e32 v1, 0xffff, v0
; GFX11-NEXT:    v_lshrrev_b32_e32 v2, 16, v0
; GFX11-NEXT:    v_lshlrev_b32_e32 v0, 2, v1
; GFX11-NEXT:    v_lshlrev_b32_e32 v1, 2, v2
; GFX11-NEXT:    s_setpc_b64 s[30:31]
  %and = and <2 x i16> %x, <i16 16383, i16 16383>
  %ext = zext <2 x i16> %and to <2 x i32>
  %shl = shl <2 x i32> %ext, <i32 2, i32 2>
  ret <2 x i32> %shl
}

declare i32 @llvm.amdgcn.workitem.id.x() #0

attributes #0 = { nounwind readnone speculatable willreturn }
