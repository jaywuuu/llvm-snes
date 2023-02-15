; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=avx2 | FileCheck %s

; Verify that the backend correctly combines AVX2 builtin intrinsics.

;
; VPBLEND Identities
;

define <16 x i16> @test_x86_avx2_pblendw(<16 x i16> %a0) {
; CHECK-LABEL: test_x86_avx2_pblendw:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <16 x i16> @llvm.x86.avx2.pblendw(<16 x i16> %a0, <16 x i16> %a0, i32 7)
  ret <16 x i16> %res
}

define <4 x i32> @test_x86_avx2_pblendd_128(<4 x i32> %a0) {
; CHECK-LABEL: test_x86_avx2_pblendd_128:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <4 x i32> @llvm.x86.avx2.pblendd.128(<4 x i32> %a0, <4 x i32> %a0, i32 7)
  ret <4 x i32> %res
}

define <8 x i32> @test_x86_avx2_pblendd_256(<8 x i32> %a0) {
; CHECK-LABEL: test_x86_avx2_pblendd_256:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <8 x i32> @llvm.x86.avx2.pblendd.256(<8 x i32> %a0, <8 x i32> %a0, i32 7)
  ret <8 x i32> %res
}

define <16 x i16> @test2_x86_avx2_pblendw(<16 x i16> %a0, <16 x i16> %a1) {
; CHECK-LABEL: test2_x86_avx2_pblendw:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <16 x i16> @llvm.x86.avx2.pblendw(<16 x i16> %a0, <16 x i16> %a1, i32 0)
  ret <16 x i16> %res
}

define <4 x i32> @test2_x86_avx2_pblendd_128(<4 x i32> %a0, <4 x i32> %a1) {
; CHECK-LABEL: test2_x86_avx2_pblendd_128:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <4 x i32> @llvm.x86.avx2.pblendd.128(<4 x i32> %a0, <4 x i32> %a1, i32 0)
  ret <4 x i32> %res
}

define <8 x i32> @test2_x86_avx2_pblendd_256(<8 x i32> %a0, <8 x i32> %a1) {
; CHECK-LABEL: test2_x86_avx2_pblendd_256:
; CHECK:       # %bb.0:
; CHECK-NEXT:    retq
  %res = call <8 x i32> @llvm.x86.avx2.pblendd.256(<8 x i32> %a0, <8 x i32> %a1, i32 0)
  ret <8 x i32> %res
}

define <16 x i16> @test3_x86_avx2_pblendw(<16 x i16> %a0, <16 x i16> %a1) {
; CHECK-LABEL: test3_x86_avx2_pblendw:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vmovaps %ymm1, %ymm0
; CHECK-NEXT:    retq
  %res = call <16 x i16> @llvm.x86.avx2.pblendw(<16 x i16> %a0, <16 x i16> %a1, i32 -1)
  ret <16 x i16> %res
}

define <4 x i32> @test3_x86_avx2_pblendd_128(<4 x i32> %a0, <4 x i32> %a1) {
; CHECK-LABEL: test3_x86_avx2_pblendd_128:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vmovaps %xmm1, %xmm0
; CHECK-NEXT:    retq
  %res = call <4 x i32> @llvm.x86.avx2.pblendd.128(<4 x i32> %a0, <4 x i32> %a1, i32 -1)
  ret <4 x i32> %res
}

define <8 x i32> @test3_x86_avx2_pblendd_256(<8 x i32> %a0, <8 x i32> %a1) {
; CHECK-LABEL: test3_x86_avx2_pblendd_256:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vmovaps %ymm1, %ymm0
; CHECK-NEXT:    retq
  %res = call <8 x i32> @llvm.x86.avx2.pblendd.256(<8 x i32> %a0, <8 x i32> %a1, i32 -1)
  ret <8 x i32> %res
}

;
; Demanded Elts
;

define <2 x i64> @demandedelts_vpsllvd(<2 x i64> %a0, <2 x i64> %a1) {
; CHECK-LABEL: demandedelts_vpsllvd:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsllvq %xmm1, %xmm0, %xmm0
; CHECK-NEXT:    vpbroadcastq %xmm0, %xmm0
; CHECK-NEXT:    retq
  %shuffle = shufflevector <2 x i64> %a0, <2 x i64> undef, <2 x i32> zeroinitializer
  %shift = call <2 x i64> @llvm.x86.avx2.psllv.q(<2 x i64> %shuffle, <2 x i64> %a1)
  %res = shufflevector <2 x i64> %shift, <2 x i64> undef, <2 x i32> zeroinitializer
  ret <2 x i64> %res
}

define <4 x i32> @demandedelts_vpsravd(<4 x i32> %a0, <4 x i32> %a1) {
; CHECK-LABEL: demandedelts_vpsravd:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsravd %xmm1, %xmm0, %xmm0
; CHECK-NEXT:    vpbroadcastd %xmm0, %xmm0
; CHECK-NEXT:    retq
  %shuffle = shufflevector <4 x i32> %a0, <4 x i32> undef, <4 x i32> <i32 0, i32 1, i32 1, i32 1>
  %shift = call <4 x i32> @llvm.x86.avx2.psrav.d(<4 x i32> %shuffle, <4 x i32> %a1)
  %res = shufflevector <4 x i32> %shift, <4 x i32> undef, <4 x i32> zeroinitializer
  ret <4 x i32> %res
}

define <4 x i64> @demandedelts_vpsrlvq(<4 x i64> %a0, <4 x i64> %a1) {
; CHECK-LABEL: demandedelts_vpsrlvq:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsrlvq %xmm1, %xmm0, %xmm0
; CHECK-NEXT:    vpbroadcastq %xmm0, %ymm0
; CHECK-NEXT:    retq
  %shuffle = shufflevector <4 x i64> %a1, <4 x i64> undef, <4 x i32> zeroinitializer
  %shift = call <4 x i64> @llvm.x86.avx2.psrlv.q.256(<4 x i64> %a0, <4 x i64> %shuffle)
  %result = shufflevector <4 x i64> %shift, <4 x i64> undef, <4 x i32> zeroinitializer
  ret <4 x i64> %result
}

;
; isBinOp Handling
;

define <4 x i32> @binop_shuffle_vpsllvd(<4 x i32> %a0, <4 x i32> %a1) {
; CHECK-LABEL: binop_shuffle_vpsllvd:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsllvd %xmm1, %xmm0, %xmm0
; CHECK-NEXT:    retq
  %shuffle0 = shufflevector <4 x i32> %a0, <4 x i32> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  %shuffle1 = shufflevector <4 x i32> %a1, <4 x i32> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  %shift = call <4 x i32> @llvm.x86.avx2.psllv.d(<4 x i32> %shuffle0, <4 x i32> %shuffle1)
  %res = shufflevector <4 x i32> %shift, <4 x i32> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  ret <4 x i32> %res
}

define <8 x i32> @binop_shuffle_vpsravd(<8 x i32> %a0, <8 x i32> %a1) {
; CHECK-LABEL: binop_shuffle_vpsravd:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsravd %ymm1, %ymm0, %ymm0
; CHECK-NEXT:    retq
  %shuffle0 = shufflevector <8 x i32> %a0, <8 x i32> undef, <8 x i32> <i32 3, i32 2, i32 1, i32 0, i32 7, i32 6, i32 5, i32 4>
  %shuffle1 = shufflevector <8 x i32> %a1, <8 x i32> undef, <8 x i32> <i32 3, i32 2, i32 1, i32 0, i32 7, i32 6, i32 5, i32 4>
  %shift = call <8 x i32> @llvm.x86.avx2.psrav.d.256(<8 x i32> %shuffle0, <8 x i32> %shuffle1)
  %res = shufflevector <8 x i32> %shift, <8 x i32> undef, <8 x i32> <i32 3, i32 2, i32 1, i32 0, i32 7, i32 6, i32 5, i32 4>
  ret <8 x i32> %res
}

define <4 x i64> @binop_shuffle_vpsrlvq(<4 x i64> %a0, <4 x i64> %a1) {
; CHECK-LABEL: binop_shuffle_vpsrlvq:
; CHECK:       # %bb.0:
; CHECK-NEXT:    vpsrlvq %ymm1, %ymm0, %ymm0
; CHECK-NEXT:    retq
  %shuffle0 = shufflevector <4 x i64> %a0, <4 x i64> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  %shuffle1 = shufflevector <4 x i64> %a1, <4 x i64> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  %shift = call <4 x i64> @llvm.x86.avx2.psrlv.q.256(<4 x i64> %shuffle0, <4 x i64> %shuffle1)
  %res = shufflevector <4 x i64> %shift, <4 x i64> undef, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  ret <4 x i64> %res
}

declare <16 x i16> @llvm.x86.avx2.pblendw(<16 x i16>, <16 x i16>, i32)
declare <4 x i32> @llvm.x86.avx2.pblendd.128(<4 x i32>, <4 x i32>, i32)
declare <8 x i32> @llvm.x86.avx2.pblendd.256(<8 x i32>, <8 x i32>, i32)

declare <4 x i32> @llvm.x86.avx2.psllv.d(<4 x i32>, <4 x i32>) nounwind readnone
declare <2 x i64> @llvm.x86.avx2.psllv.q(<2 x i64>, <2 x i64>) nounwind readnone
declare <4 x i32> @llvm.x86.avx2.psrlv.d(<4 x i32>, <4 x i32>) nounwind readnone
declare <2 x i64> @llvm.x86.avx2.psrlv.q(<2 x i64>, <2 x i64>) nounwind readnone
declare <4 x i32> @llvm.x86.avx2.psrav.d(<4 x i32>, <4 x i32>) nounwind readnone

declare <8 x i32> @llvm.x86.avx2.psllv.d.256(<8 x i32>, <8 x i32>) nounwind readnone
declare <4 x i64> @llvm.x86.avx2.psllv.q.256(<4 x i64>, <4 x i64>) nounwind readnone
declare <8 x i32> @llvm.x86.avx2.psrlv.d.256(<8 x i32>, <8 x i32>) nounwind readnone
declare <4 x i64> @llvm.x86.avx2.psrlv.q.256(<4 x i64>, <4 x i64>) nounwind readnone
declare <8 x i32> @llvm.x86.avx2.psrav.d.256(<8 x i32>, <8 x i32>) nounwind readnone
