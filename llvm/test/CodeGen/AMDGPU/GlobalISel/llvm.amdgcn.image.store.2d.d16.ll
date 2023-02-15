; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=tonga -o - %s | FileCheck -check-prefix=UNPACKED %s
; RUN: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=gfx810 -o - %s | FileCheck -check-prefix=GFX81 %s
; FIXME: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=gfx900 -o - %s | FileCheck -check-prefix=GFX9 %s
; FIXME: llc -global-isel -mtriple=amdgcn-mesa-mesa3d -mcpu=gfx1010 -o - %s | FileCheck -check-prefix=GFX10 %s

define amdgpu_ps void @image_store_f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, half %data) {
; UNPACKED-LABEL: image_store_f16:
; UNPACKED:       ; %bb.0:
; UNPACKED-NEXT:    s_mov_b32 s0, s2
; UNPACKED-NEXT:    s_mov_b32 s1, s3
; UNPACKED-NEXT:    s_mov_b32 s2, s4
; UNPACKED-NEXT:    s_mov_b32 s3, s5
; UNPACKED-NEXT:    s_mov_b32 s4, s6
; UNPACKED-NEXT:    s_mov_b32 s5, s7
; UNPACKED-NEXT:    s_mov_b32 s6, s8
; UNPACKED-NEXT:    s_mov_b32 s7, s9
; UNPACKED-NEXT:    image_store v2, v[0:1], s[0:7] dmask:0x1 unorm d16
; UNPACKED-NEXT:    s_endpgm
;
; GFX81-LABEL: image_store_f16:
; GFX81:       ; %bb.0:
; GFX81-NEXT:    s_mov_b32 s0, s2
; GFX81-NEXT:    s_mov_b32 s1, s3
; GFX81-NEXT:    s_mov_b32 s2, s4
; GFX81-NEXT:    s_mov_b32 s3, s5
; GFX81-NEXT:    s_mov_b32 s4, s6
; GFX81-NEXT:    s_mov_b32 s5, s7
; GFX81-NEXT:    s_mov_b32 s6, s8
; GFX81-NEXT:    s_mov_b32 s7, s9
; GFX81-NEXT:    image_store v2, v[0:1], s[0:7] dmask:0x1 unorm d16
; GFX81-NEXT:    s_endpgm
; PACKED-LABEL: image_store_f16:
; PACKED:       ; %bb.0:
; PACKED-NEXT:    s_mov_b32 s0, s2
; PACKED-NEXT:    s_mov_b32 s1, s3
; PACKED-NEXT:    s_mov_b32 s2, s4
; PACKED-NEXT:    s_mov_b32 s3, s5
; PACKED-NEXT:    s_mov_b32 s4, s6
; PACKED-NEXT:    s_mov_b32 s5, s7
; PACKED-NEXT:    s_mov_b32 s6, s8
; PACKED-NEXT:    s_mov_b32 s7, s9
; PACKED-NEXT:    image_store v2, v[0:1], s[0:7] dmask:0x1 unorm d16
; PACKED-NEXT:    s_endpgm
  call void @llvm.amdgcn.image.store.2d.f16.i32(half %data, i32 1, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v2f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <2 x half> %in) {
; UNPACKED-LABEL: image_store_v2f16:
; UNPACKED:       ; %bb.0:
; UNPACKED-NEXT:    s_mov_b32 s0, s2
; UNPACKED-NEXT:    s_mov_b32 s1, s3
; UNPACKED-NEXT:    s_mov_b32 s2, s4
; UNPACKED-NEXT:    s_mov_b32 s3, s5
; UNPACKED-NEXT:    s_mov_b32 s4, s6
; UNPACKED-NEXT:    s_mov_b32 s5, s7
; UNPACKED-NEXT:    s_mov_b32 s6, s8
; UNPACKED-NEXT:    s_mov_b32 s7, s9
; UNPACKED-NEXT:    v_lshrrev_b32_e32 v3, 16, v2
; UNPACKED-NEXT:    image_store v[2:3], v[0:1], s[0:7] dmask:0x3 unorm d16
; UNPACKED-NEXT:    s_endpgm
;
; GFX81-LABEL: image_store_v2f16:
; GFX81:       ; %bb.0:
; GFX81-NEXT:    s_mov_b32 s0, s2
; GFX81-NEXT:    s_mov_b32 s1, s3
; GFX81-NEXT:    s_mov_b32 s2, s4
; GFX81-NEXT:    s_mov_b32 s3, s5
; GFX81-NEXT:    s_mov_b32 s4, s6
; GFX81-NEXT:    s_mov_b32 s5, s7
; GFX81-NEXT:    s_mov_b32 s6, s8
; GFX81-NEXT:    s_mov_b32 s7, s9
; GFX81-NEXT:    image_store v[2:3], v[0:1], s[0:7] dmask:0x3 unorm d16
; GFX81-NEXT:    s_endpgm
; PACKED-LABEL: image_store_v2f16:
; PACKED:       ; %bb.0:
; PACKED-NEXT:    s_mov_b32 s0, s2
; PACKED-NEXT:    s_mov_b32 s1, s3
; PACKED-NEXT:    s_mov_b32 s2, s4
; PACKED-NEXT:    s_mov_b32 s3, s5
; PACKED-NEXT:    s_mov_b32 s4, s6
; PACKED-NEXT:    s_mov_b32 s5, s7
; PACKED-NEXT:    s_mov_b32 s6, s8
; PACKED-NEXT:    s_mov_b32 s7, s9
; PACKED-NEXT:    image_store v2, v[0:1], s[0:7] dmask:0x3 unorm d16
; PACKED-NEXT:    s_endpgm
  call void @llvm.amdgcn.image.store.2d.v2f16.i32(<2 x half> %in, i32 3, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v3f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <3 x half> %in) {
; UNPACKED-LABEL: image_store_v3f16:
; UNPACKED:       ; %bb.0:
; UNPACKED-NEXT:    v_mov_b32_e32 v5, v1
; UNPACKED-NEXT:    v_mov_b32_e32 v1, v2
; UNPACKED-NEXT:    s_mov_b32 s0, s2
; UNPACKED-NEXT:    s_mov_b32 s1, s3
; UNPACKED-NEXT:    s_mov_b32 s2, s4
; UNPACKED-NEXT:    s_mov_b32 s3, s5
; UNPACKED-NEXT:    s_mov_b32 s4, s6
; UNPACKED-NEXT:    s_mov_b32 s5, s7
; UNPACKED-NEXT:    s_mov_b32 s6, s8
; UNPACKED-NEXT:    s_mov_b32 s7, s9
; UNPACKED-NEXT:    v_mov_b32_e32 v4, v0
; UNPACKED-NEXT:    v_lshrrev_b32_e32 v2, 16, v1
; UNPACKED-NEXT:    image_store v[1:3], v[4:5], s[0:7] dmask:0x7 unorm d16
; UNPACKED-NEXT:    s_endpgm
;
; GFX81-LABEL: image_store_v3f16:
; GFX81:       ; %bb.0:
; GFX81-NEXT:    v_lshrrev_b32_e32 v4, 16, v2
; GFX81-NEXT:    v_lshlrev_b32_e32 v4, 16, v4
; GFX81-NEXT:    s_mov_b32 s0, s2
; GFX81-NEXT:    s_mov_b32 s1, s3
; GFX81-NEXT:    s_mov_b32 s2, s4
; GFX81-NEXT:    s_mov_b32 s3, s5
; GFX81-NEXT:    s_mov_b32 s4, s6
; GFX81-NEXT:    s_mov_b32 s5, s7
; GFX81-NEXT:    s_mov_b32 s6, s8
; GFX81-NEXT:    s_mov_b32 s7, s9
; GFX81-NEXT:    v_or_b32_sdwa v2, v2, v4 dst_sel:DWORD dst_unused:UNUSED_PAD src0_sel:WORD_0 src1_sel:DWORD
; GFX81-NEXT:    v_and_b32_e32 v3, 0xffff, v3
; GFX81-NEXT:    v_mov_b32_e32 v4, 0
; GFX81-NEXT:    image_store v[2:4], v[0:1], s[0:7] dmask:0x7 unorm d16
; GFX81-NEXT:    s_endpgm
  call void @llvm.amdgcn.image.store.2d.v3f16.i32(<3 x half> %in, i32 7, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

define amdgpu_ps void @image_store_v4f16(<8 x i32> inreg %rsrc, i32 %s, i32 %t, <4 x half> %in) {
; UNPACKED-LABEL: image_store_v4f16:
; UNPACKED:       ; %bb.0:
; UNPACKED-NEXT:    v_mov_b32_e32 v6, v1
; UNPACKED-NEXT:    v_mov_b32_e32 v1, v2
; UNPACKED-NEXT:    s_mov_b32 s0, s2
; UNPACKED-NEXT:    s_mov_b32 s1, s3
; UNPACKED-NEXT:    s_mov_b32 s2, s4
; UNPACKED-NEXT:    s_mov_b32 s3, s5
; UNPACKED-NEXT:    s_mov_b32 s4, s6
; UNPACKED-NEXT:    s_mov_b32 s5, s7
; UNPACKED-NEXT:    s_mov_b32 s6, s8
; UNPACKED-NEXT:    s_mov_b32 s7, s9
; UNPACKED-NEXT:    v_mov_b32_e32 v5, v0
; UNPACKED-NEXT:    v_lshrrev_b32_e32 v2, 16, v1
; UNPACKED-NEXT:    v_lshrrev_b32_e32 v4, 16, v3
; UNPACKED-NEXT:    image_store v[1:4], v[5:6], s[0:7] dmask:0xf unorm d16
; UNPACKED-NEXT:    s_endpgm
;
; GFX81-LABEL: image_store_v4f16:
; GFX81:       ; %bb.0:
; GFX81-NEXT:    s_mov_b32 s0, s2
; GFX81-NEXT:    s_mov_b32 s1, s3
; GFX81-NEXT:    s_mov_b32 s2, s4
; GFX81-NEXT:    s_mov_b32 s3, s5
; GFX81-NEXT:    s_mov_b32 s4, s6
; GFX81-NEXT:    s_mov_b32 s5, s7
; GFX81-NEXT:    s_mov_b32 s6, s8
; GFX81-NEXT:    s_mov_b32 s7, s9
; GFX81-NEXT:    image_store v[2:5], v[0:1], s[0:7] dmask:0xf unorm d16
; GFX81-NEXT:    s_endpgm
; PACKED-LABEL: image_store_v4f16:
; PACKED:       ; %bb.0:
; PACKED-NEXT:    s_mov_b32 s0, s2
; PACKED-NEXT:    s_mov_b32 s1, s3
; PACKED-NEXT:    s_mov_b32 s2, s4
; PACKED-NEXT:    s_mov_b32 s3, s5
; PACKED-NEXT:    s_mov_b32 s4, s6
; PACKED-NEXT:    s_mov_b32 s5, s7
; PACKED-NEXT:    s_mov_b32 s6, s8
; PACKED-NEXT:    s_mov_b32 s7, s9
; PACKED-NEXT:    image_store v[2:3], v[0:1], s[0:7] dmask:0xf unorm d16
; PACKED-NEXT:    s_endpgm
  call void @llvm.amdgcn.image.store.2d.v4f16.i32(<4 x half> %in, i32 15, i32 %s, i32 %t, <8 x i32> %rsrc, i32 0, i32 0)
  ret void
}

declare void @llvm.amdgcn.image.store.2d.f16.i32(half, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v2f16.i32(<2 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v3f16.i32(<3 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0
declare void @llvm.amdgcn.image.store.2d.v4f16.i32(<4 x half>, i32 immarg, i32, i32, <8 x i32>, i32 immarg, i32 immarg) #0

attributes #0 = { nounwind writeonly }
