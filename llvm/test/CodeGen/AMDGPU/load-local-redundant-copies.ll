; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -march=amdgcn -verify-machineinstrs < %s | FileCheck %s

; Test that checks for redundant copies to temporary stack slot produced by
; expandUnalignedLoad.

define amdgpu_vs void @test(<4 x i32> inreg %arg1, <6 x float> addrspace(3)* %arg2) {
; CHECK-LABEL: test:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    v_add_i32_e32 v3, vcc, 12, v0
; CHECK-NEXT:    v_add_i32_e32 v1, vcc, 8, v0
; CHECK-NEXT:    v_add_i32_e32 v4, vcc, 4, v0
; CHECK-NEXT:    s_mov_b32 m0, -1
; CHECK-NEXT:    ds_read_b32 v2, v1
; CHECK-NEXT:    ds_read_b32 v1, v4
; CHECK-NEXT:    ds_read_b32 v3, v3
; CHECK-NEXT:    ds_read_b32 v0, v0
; CHECK-NEXT:    s_waitcnt lgkmcnt(0)
; CHECK-NEXT:    exp mrt0 off, off, off, off
; CHECK-NEXT:    v_mov_b32_e32 v4, 0
; CHECK-NEXT:    tbuffer_store_format_xyzw v[0:3], v4, s[0:3], 0 format:[BUF_DATA_FORMAT_32_32_32_32,BUF_NUM_FORMAT_FLOAT] idxen
; CHECK-NEXT:    s_endpgm
  call void @llvm.amdgcn.exp.f32(i32 immarg 0, i32 immarg 0, float undef, float undef, float undef, float undef, i1 immarg false, i1 immarg false)
  %var1 = load <6 x float>, <6 x float> addrspace(3)* %arg2, align 4
  %var2 = shufflevector <6 x float> %var1, <6 x float> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  call void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float> %var2, <4 x i32> %arg1, i32 0, i32 0, i32 0, i32 immarg 126, i32 immarg 0)
  ret void
}

define amdgpu_vs void @test_2(<4 x i32> inreg %arg1, i32 %arg2, i32 inreg %arg3, <8 x float> addrspace(3)* %arg4) {
; CHECK-LABEL: test_2:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    v_add_i32_e32 v5, vcc, 28, v1
; CHECK-NEXT:    v_add_i32_e32 v2, vcc, 24, v1
; CHECK-NEXT:    v_add_i32_e32 v3, vcc, 20, v1
; CHECK-NEXT:    v_add_i32_e32 v6, vcc, 16, v1
; CHECK-NEXT:    v_add_i32_e32 v7, vcc, 12, v1
; CHECK-NEXT:    v_add_i32_e32 v8, vcc, 8, v1
; CHECK-NEXT:    v_add_i32_e32 v10, vcc, 4, v1
; CHECK-NEXT:    s_mov_b32 m0, -1
; CHECK-NEXT:    ds_read_b32 v4, v2
; CHECK-NEXT:    ds_read_b32 v3, v3
; CHECK-NEXT:    ds_read_b32 v2, v6
; CHECK-NEXT:    ds_read_b32 v9, v7
; CHECK-NEXT:    ds_read_b32 v8, v8
; CHECK-NEXT:    ds_read_b32 v7, v10
; CHECK-NEXT:    ds_read_b32 v6, v1
; CHECK-NEXT:    ds_read_b32 v5, v5
; CHECK-NEXT:    s_waitcnt lgkmcnt(1)
; CHECK-NEXT:    tbuffer_store_format_xyzw v[6:9], v0, s[0:3], s4 format:[BUF_DATA_FORMAT_32_32_32,BUF_NUM_FORMAT_UINT] idxen glc slc
; CHECK-NEXT:    s_waitcnt lgkmcnt(0)
; CHECK-NEXT:    tbuffer_store_format_xyzw v[2:5], v0, s[0:3], s4 format:[BUF_DATA_FORMAT_32_32_32,BUF_NUM_FORMAT_UINT] idxen offset:16 glc slc
; CHECK-NEXT:    s_endpgm
  %load = load <8 x float>, <8 x float> addrspace(3)* %arg4, align 4
  %vec1 = shufflevector <8 x float> %load, <8 x float> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  call void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float> %vec1, <4 x i32> %arg1, i32 %arg2, i32 0, i32 %arg3, i32 immarg 77, i32 immarg 3)
  %vec2 = shufflevector <8 x float> %load, <8 x float> undef, <4 x i32> <i32 4, i32 5, i32 6, i32 7>
  call void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float> %vec2, <4 x i32> %arg1, i32 %arg2, i32 16, i32 %arg3, i32 immarg 77, i32 immarg 3)
  ret void
}

define amdgpu_vs void @test_3(i32 inreg %arg1, i32 inreg %arg2, <4 x i32> inreg %arg3, i32 %arg4, <6 x float> addrspace(3)* %arg5, <6 x float> addrspace(3)* %arg6) {
; CHECK-LABEL: test_3:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    s_mov_b32 s7, s5
; CHECK-NEXT:    s_mov_b32 s6, s4
; CHECK-NEXT:    s_mov_b32 s5, s3
; CHECK-NEXT:    s_mov_b32 s4, s2
; CHECK-NEXT:    v_add_i32_e32 v0, vcc, 20, v1
; CHECK-NEXT:    v_add_i32_e32 v3, vcc, 16, v1
; CHECK-NEXT:    v_add_i32_e32 v4, vcc, 12, v1
; CHECK-NEXT:    v_add_i32_e32 v5, vcc, 8, v1
; CHECK-NEXT:    v_add_i32_e32 v8, vcc, 4, v1
; CHECK-NEXT:    v_mov_b32_e32 v9, s0
; CHECK-NEXT:    v_add_i32_e32 v10, vcc, 20, v2
; CHECK-NEXT:    v_add_i32_e32 v11, vcc, 16, v2
; CHECK-NEXT:    s_mov_b32 m0, -1
; CHECK-NEXT:    ds_read_b32 v7, v3
; CHECK-NEXT:    ds_read_b32 v6, v4
; CHECK-NEXT:    ds_read_b32 v5, v5
; CHECK-NEXT:    ds_read_b32 v4, v8
; CHECK-NEXT:    ds_read_b32 v8, v0
; CHECK-NEXT:    ds_read_b32 v3, v1
; CHECK-NEXT:    v_add_i32_e32 v1, vcc, 12, v2
; CHECK-NEXT:    v_add_i32_e32 v12, vcc, 8, v2
; CHECK-NEXT:    v_add_i32_e32 v13, vcc, 4, v2
; CHECK-NEXT:    s_waitcnt lgkmcnt(0)
; CHECK-NEXT:    tbuffer_store_format_xyzw v[3:6], v9, s[4:7], s1 format:[BUF_DATA_FORMAT_32_32_32,BUF_NUM_FORMAT_UINT] idxen offset:264 glc slc
; CHECK-NEXT:    tbuffer_store_format_xy v[7:8], v9, s[4:7], s1 format:[BUF_DATA_FORMAT_INVALID,BUF_NUM_FORMAT_UINT] idxen offset:280 glc slc
; CHECK-NEXT:    ds_read_b32 v0, v11
; CHECK-NEXT:    s_waitcnt expcnt(1)
; CHECK-NEXT:    ds_read_b32 v5, v1
; CHECK-NEXT:    ds_read_b32 v4, v12
; CHECK-NEXT:    ds_read_b32 v3, v13
; CHECK-NEXT:    ds_read_b32 v2, v2
; CHECK-NEXT:    ds_read_b32 v1, v10
; CHECK-NEXT:    s_waitcnt lgkmcnt(5)
; CHECK-NEXT:    exp mrt0 off, off, off, off
; CHECK-NEXT:    s_waitcnt lgkmcnt(1)
; CHECK-NEXT:    tbuffer_store_format_xyzw v[2:5], v9, s[4:7], s1 format:[BUF_DATA_FORMAT_32_32_32,BUF_NUM_FORMAT_UINT] idxen offset:240 glc slc
; CHECK-NEXT:    s_waitcnt lgkmcnt(0)
; CHECK-NEXT:    tbuffer_store_format_xy v[0:1], v9, s[4:7], s1 format:[BUF_DATA_FORMAT_INVALID,BUF_NUM_FORMAT_UINT] idxen offset:256 glc slc
; CHECK-NEXT:    s_endpgm
  %load1 = load <6 x float>, <6 x float> addrspace(3)* %arg5, align 4
  %vec11 = shufflevector <6 x float> %load1, <6 x float> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  call void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float> %vec11, <4 x i32> %arg3, i32 %arg1, i32 264, i32 %arg2, i32 immarg 77, i32 immarg 3)
  %vec12 = shufflevector <6 x float> %load1, <6 x float> undef, <2 x i32> <i32 4, i32 5>
  call void @llvm.amdgcn.struct.tbuffer.store.v2f32(<2 x float> %vec12, <4 x i32> %arg3, i32 %arg1, i32 280, i32 %arg2, i32 immarg 64, i32 immarg 3)

  call void @llvm.amdgcn.exp.f32(i32 immarg 0, i32 immarg 0, float undef, float undef, float undef, float undef, i1 immarg false, i1 immarg false)

  %load2 = load <6 x float>, <6 x float> addrspace(3)* %arg6, align 4
  %vec21 = shufflevector <6 x float> %load2, <6 x float> undef, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  call void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float> %vec21, <4 x i32> %arg3, i32 %arg1, i32 240, i32 %arg2, i32 immarg 77, i32 immarg 3)
  %vec22 = shufflevector <6 x float> %load2, <6 x float> undef, <2 x i32> <i32 4, i32 5>
  call void @llvm.amdgcn.struct.tbuffer.store.v2f32(<2 x float> %vec22, <4 x i32> %arg3, i32 %arg1, i32 256, i32 %arg2, i32 immarg 64, i32 immarg 3)

  ret void
}

declare void @llvm.amdgcn.struct.tbuffer.store.v4f32(<4 x float>, <4 x i32>, i32, i32, i32, i32 immarg, i32 immarg)
declare void @llvm.amdgcn.struct.tbuffer.store.v2f32(<2 x float>, <4 x i32>, i32, i32, i32, i32 immarg, i32 immarg)
declare void @llvm.amdgcn.exp.f32(i32 immarg, i32 immarg, float, float, float, float, i1 immarg, i1 immarg)