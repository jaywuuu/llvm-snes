; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py
; RUN: opt -passes='print<cost-model>' 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa %s | FileCheck -check-prefixes=GCN,CI %s
; RUN: opt -passes='print<cost-model>' 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa -mcpu=fiji %s | FileCheck -check-prefixes=GCN,GFX89 %s
; RUN: opt -passes='print<cost-model>' 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa -mcpu=gfx900 %s | FileCheck -check-prefixes=GCN,GFX89 %s
; RUN: opt -passes='print<cost-model>' -cost-kind=code-size 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa %s | FileCheck -check-prefixes=GCN-SIZE,CI-SIZE %s
; RUN: opt -passes='print<cost-model>' -cost-kind=code-size 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa -mcpu=fiji %s | FileCheck -check-prefixes=GCN-SIZE,GFX89-SIZE %s
; RUN: opt -passes='print<cost-model>' -cost-kind=code-size 2>&1 -disable-output -mtriple=amdgcn-unknown-amdhsa -mcpu=gfx900 %s | FileCheck -check-prefixes=GCN-SIZE,GFX89-SIZE %s
; END.

define amdgpu_kernel void @extractelement_32(i32 %arg) {
; GCN-LABEL: 'extractelement_32'
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i32_0 = extractelement <2 x i32> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f32_0 = extractelement <2 x float> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_0 = extractelement <3 x i32> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_0 = extractelement <4 x i32> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_0 = extractelement <5 x i32> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_0 = extractelement <8 x i32> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i32_1 = extractelement <2 x i32> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f32_1 = extractelement <2 x float> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_1 = extractelement <3 x i32> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_1 = extractelement <4 x i32> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_1 = extractelement <5 x i32> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_1 = extractelement <8 x i32> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_2 = extractelement <3 x i32> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_2 = extractelement <4 x i32> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_2 = extractelement <5 x i32> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_2 = extractelement <8 x i32> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_3 = extractelement <4 x i32> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_3 = extractelement <5 x i32> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_3 = extractelement <8 x i32> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v2i32_a = extractelement <2 x i32> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v4i32_a = extractelement <4 x i32> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v8i32_a = extractelement <8 x i32> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 10 for instruction: ret void
;
; GCN-SIZE-LABEL: 'extractelement_32'
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i32_0 = extractelement <2 x i32> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f32_0 = extractelement <2 x float> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_0 = extractelement <3 x i32> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_0 = extractelement <4 x i32> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_0 = extractelement <5 x i32> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_0 = extractelement <8 x i32> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i32_1 = extractelement <2 x i32> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f32_1 = extractelement <2 x float> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_1 = extractelement <3 x i32> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_1 = extractelement <4 x i32> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_1 = extractelement <5 x i32> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_1 = extractelement <8 x i32> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i32_2 = extractelement <3 x i32> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_2 = extractelement <4 x i32> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_2 = extractelement <5 x i32> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_2 = extractelement <8 x i32> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i32_3 = extractelement <4 x i32> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i32_3 = extractelement <5 x i32> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i32_3 = extractelement <8 x i32> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v2i32_a = extractelement <2 x i32> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v4i32_a = extractelement <4 x i32> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v8i32_a = extractelement <8 x i32> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v2i32_0 = extractelement <2 x i32> undef, i32 0
  %v2f32_0 = extractelement <2 x float> undef, i32 0
  %v3i32_0 = extractelement <3 x i32> undef, i32 0
  %v4i32_0 = extractelement <4 x i32> undef, i32 0
  %v5i32_0 = extractelement <5 x i32> undef, i32 0
  %v8i32_0 = extractelement <8 x i32> undef, i32 0

  %v2i32_1 = extractelement <2 x i32> undef, i32 1
  %v2f32_1 = extractelement <2 x float> undef, i32 1
  %v3i32_1 = extractelement <3 x i32> undef, i32 1
  %v4i32_1 = extractelement <4 x i32> undef, i32 1
  %v5i32_1 = extractelement <5 x i32> undef, i32 1
  %v8i32_1 = extractelement <8 x i32> undef, i32 1

  %v3i32_2 = extractelement <3 x i32> undef, i32 2
  %v4i32_2 = extractelement <4 x i32> undef, i32 2
  %v5i32_2 = extractelement <5 x i32> undef, i32 2
  %v8i32_2 = extractelement <8 x i32> undef, i32 2

  %v4i32_3 = extractelement <4 x i32> undef, i32 3
  %v5i32_3 = extractelement <5 x i32> undef, i32 3
  %v8i32_3 = extractelement <8 x i32> undef, i32 3

  %v2i32_a = extractelement <2 x i32> undef, i32 %arg
  %v4i32_a = extractelement <4 x i32> undef, i32 %arg
  %v8i32_a = extractelement <8 x i32> undef, i32 %arg
  ret void
}

define amdgpu_kernel void @extractelement_64(i32 %arg) {
; GCN-LABEL: 'extractelement_64'
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i64_0 = extractelement <2 x i64> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f64_0 = extractelement <2 x double> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_0 = extractelement <3 x i64> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_0 = extractelement <4 x i64> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_0 = extractelement <5 x i64> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_0 = extractelement <8 x i64> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i64_1 = extractelement <2 x i64> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f64_1 = extractelement <2 x double> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_1 = extractelement <3 x i64> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_1 = extractelement <4 x i64> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_1 = extractelement <5 x i64> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_1 = extractelement <8 x i64> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_2 = extractelement <3 x i64> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_2 = extractelement <4 x i64> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_2 = extractelement <5 x i64> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_2 = extractelement <8 x i64> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_3 = extractelement <4 x i64> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_3 = extractelement <5 x i64> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_3 = extractelement <8 x i64> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v2i64_a = extractelement <2 x i64> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v4i64_a = extractelement <4 x i64> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v8i64_a = extractelement <8 x i64> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 10 for instruction: ret void
;
; GCN-SIZE-LABEL: 'extractelement_64'
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i64_0 = extractelement <2 x i64> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f64_0 = extractelement <2 x double> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_0 = extractelement <3 x i64> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_0 = extractelement <4 x i64> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_0 = extractelement <5 x i64> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_0 = extractelement <8 x i64> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i64_1 = extractelement <2 x i64> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f64_1 = extractelement <2 x double> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_1 = extractelement <3 x i64> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_1 = extractelement <4 x i64> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_1 = extractelement <5 x i64> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_1 = extractelement <8 x i64> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i64_2 = extractelement <3 x i64> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_2 = extractelement <4 x i64> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_2 = extractelement <5 x i64> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_2 = extractelement <8 x i64> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i64_3 = extractelement <4 x i64> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i64_3 = extractelement <5 x i64> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i64_3 = extractelement <8 x i64> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v2i64_a = extractelement <2 x i64> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v4i64_a = extractelement <4 x i64> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 2 for instruction: %v8i64_a = extractelement <8 x i64> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v2i64_0 = extractelement <2 x i64> undef, i32 0
  %v2f64_0 = extractelement <2 x double> undef, i32 0
  %v3i64_0 = extractelement <3 x i64> undef, i32 0
  %v4i64_0 = extractelement <4 x i64> undef, i32 0
  %v5i64_0 = extractelement <5 x i64> undef, i32 0
  %v8i64_0 = extractelement <8 x i64> undef, i32 0

  %v2i64_1 = extractelement <2 x i64> undef, i32 1
  %v2f64_1 = extractelement <2 x double> undef, i32 1
  %v3i64_1 = extractelement <3 x i64> undef, i32 1
  %v4i64_1 = extractelement <4 x i64> undef, i32 1
  %v5i64_1 = extractelement <5 x i64> undef, i32 1
  %v8i64_1 = extractelement <8 x i64> undef, i32 1

  %v3i64_2 = extractelement <3 x i64> undef, i32 2
  %v4i64_2 = extractelement <4 x i64> undef, i32 2
  %v5i64_2 = extractelement <5 x i64> undef, i32 2
  %v8i64_2 = extractelement <8 x i64> undef, i32 2

  %v4i64_3 = extractelement <4 x i64> undef, i32 3
  %v5i64_3 = extractelement <5 x i64> undef, i32 3
  %v8i64_3 = extractelement <8 x i64> undef, i32 3

  %v2i64_a = extractelement <2 x i64> undef, i32 %arg
  %v4i64_a = extractelement <4 x i64> undef, i32 %arg
  %v8i64_a = extractelement <8 x i64> undef, i32 %arg
  ret void
}

define amdgpu_kernel void @extractelement_8(i32 %arg) {
; GCN-LABEL: 'extractelement_8'
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_0 = extractelement <2 x i8> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_0 = extractelement <3 x i8> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_0 = extractelement <4 x i8> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_0 = extractelement <5 x i8> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_0 = extractelement <8 x i8> undef, i32 0
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_1 = extractelement <2 x i8> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_1 = extractelement <3 x i8> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_1 = extractelement <4 x i8> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_1 = extractelement <5 x i8> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_1 = extractelement <8 x i8> undef, i32 1
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_2 = extractelement <3 x i8> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_2 = extractelement <4 x i8> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_2 = extractelement <5 x i8> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_2 = extractelement <8 x i8> undef, i32 2
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_3 = extractelement <4 x i8> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_3 = extractelement <5 x i8> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_3 = extractelement <8 x i8> undef, i32 3
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_a = extractelement <2 x i8> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_a = extractelement <4 x i8> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_a = extractelement <8 x i8> undef, i32 %arg
; GCN-NEXT:  Cost Model: Found an estimated cost of 10 for instruction: ret void
;
; GCN-SIZE-LABEL: 'extractelement_8'
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_0 = extractelement <2 x i8> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_0 = extractelement <3 x i8> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_0 = extractelement <4 x i8> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_0 = extractelement <5 x i8> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_0 = extractelement <8 x i8> undef, i32 0
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_1 = extractelement <2 x i8> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_1 = extractelement <3 x i8> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_1 = extractelement <4 x i8> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_1 = extractelement <5 x i8> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_1 = extractelement <8 x i8> undef, i32 1
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i8_2 = extractelement <3 x i8> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_2 = extractelement <4 x i8> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_2 = extractelement <5 x i8> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_2 = extractelement <8 x i8> undef, i32 2
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_3 = extractelement <4 x i8> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i8_3 = extractelement <5 x i8> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_3 = extractelement <8 x i8> undef, i32 3
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i8_a = extractelement <2 x i8> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i8_a = extractelement <4 x i8> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i8_a = extractelement <8 x i8> undef, i32 %arg
; GCN-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v2i8_0 = extractelement <2 x i8> undef, i32 0
  %v3i8_0 = extractelement <3 x i8> undef, i32 0
  %v4i8_0 = extractelement <4 x i8> undef, i32 0
  %v5i8_0 = extractelement <5 x i8> undef, i32 0
  %v8i8_0 = extractelement <8 x i8> undef, i32 0

  %v2i8_1 = extractelement <2 x i8> undef, i32 1
  %v3i8_1 = extractelement <3 x i8> undef, i32 1
  %v4i8_1 = extractelement <4 x i8> undef, i32 1
  %v5i8_1 = extractelement <5 x i8> undef, i32 1
  %v8i8_1 = extractelement <8 x i8> undef, i32 1

  %v3i8_2 = extractelement <3 x i8> undef, i32 2
  %v4i8_2 = extractelement <4 x i8> undef, i32 2
  %v5i8_2 = extractelement <5 x i8> undef, i32 2
  %v8i8_2 = extractelement <8 x i8> undef, i32 2

  %v4i8_3 = extractelement <4 x i8> undef, i32 3
  %v5i8_3 = extractelement <5 x i8> undef, i32 3
  %v8i8_3 = extractelement <8 x i8> undef, i32 3

  %v2i8_a = extractelement <2 x i8> undef, i32 %arg
  %v4i8_a = extractelement <4 x i8> undef, i32 %arg
  %v8i8_a = extractelement <8 x i8> undef, i32 %arg
  ret void
}

define amdgpu_kernel void @extractelement_16(i32 %arg) {
; CI-LABEL: 'extractelement_16'
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_0 = extractelement <2 x i16> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_0 = extractelement <2 x half> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_0 = extractelement <3 x i16> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_0 = extractelement <4 x i16> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_0 = extractelement <5 x i16> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_0 = extractelement <8 x i16> undef, i32 0
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_1 = extractelement <2 x i16> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_1 = extractelement <2 x half> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_1 = extractelement <3 x i16> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_1 = extractelement <4 x i16> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_1 = extractelement <5 x i16> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_1 = extractelement <8 x i16> undef, i32 1
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_2 = extractelement <3 x i16> undef, i32 2
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_2 = extractelement <4 x i16> undef, i32 2
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_2 = extractelement <5 x i16> undef, i32 2
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_2 = extractelement <8 x i16> undef, i32 2
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_3 = extractelement <4 x i16> undef, i32 3
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_3 = extractelement <5 x i16> undef, i32 3
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_3 = extractelement <8 x i16> undef, i32 3
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_a = extractelement <2 x i16> undef, i32 %arg
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_a = extractelement <4 x i16> undef, i32 %arg
; CI-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_a = extractelement <8 x i16> undef, i32 %arg
; CI-NEXT:  Cost Model: Found an estimated cost of 10 for instruction: ret void
;
; GFX89-LABEL: 'extractelement_16'
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i16_0 = extractelement <2 x i16> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f16_0 = extractelement <2 x half> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i16_0 = extractelement <3 x i16> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i16_0 = extractelement <4 x i16> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i16_0 = extractelement <5 x i16> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i16_0 = extractelement <8 x i16> undef, i32 0
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_1 = extractelement <2 x i16> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_1 = extractelement <2 x half> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_1 = extractelement <3 x i16> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_1 = extractelement <4 x i16> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_1 = extractelement <5 x i16> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_1 = extractelement <8 x i16> undef, i32 1
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_2 = extractelement <3 x i16> undef, i32 2
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_2 = extractelement <4 x i16> undef, i32 2
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_2 = extractelement <5 x i16> undef, i32 2
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_2 = extractelement <8 x i16> undef, i32 2
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_3 = extractelement <4 x i16> undef, i32 3
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_3 = extractelement <5 x i16> undef, i32 3
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_3 = extractelement <8 x i16> undef, i32 3
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_a = extractelement <2 x i16> undef, i32 %arg
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_a = extractelement <4 x i16> undef, i32 %arg
; GFX89-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_a = extractelement <8 x i16> undef, i32 %arg
; GFX89-NEXT:  Cost Model: Found an estimated cost of 10 for instruction: ret void
;
; CI-SIZE-LABEL: 'extractelement_16'
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_0 = extractelement <2 x i16> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_0 = extractelement <2 x half> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_0 = extractelement <3 x i16> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_0 = extractelement <4 x i16> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_0 = extractelement <5 x i16> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_0 = extractelement <8 x i16> undef, i32 0
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_1 = extractelement <2 x i16> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_1 = extractelement <2 x half> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_1 = extractelement <3 x i16> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_1 = extractelement <4 x i16> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_1 = extractelement <5 x i16> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_1 = extractelement <8 x i16> undef, i32 1
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_2 = extractelement <3 x i16> undef, i32 2
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_2 = extractelement <4 x i16> undef, i32 2
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_2 = extractelement <5 x i16> undef, i32 2
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_2 = extractelement <8 x i16> undef, i32 2
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_3 = extractelement <4 x i16> undef, i32 3
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_3 = extractelement <5 x i16> undef, i32 3
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_3 = extractelement <8 x i16> undef, i32 3
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_a = extractelement <2 x i16> undef, i32 %arg
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_a = extractelement <4 x i16> undef, i32 %arg
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_a = extractelement <8 x i16> undef, i32 %arg
; CI-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
; GFX89-SIZE-LABEL: 'extractelement_16'
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2i16_0 = extractelement <2 x i16> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v2f16_0 = extractelement <2 x half> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v3i16_0 = extractelement <3 x i16> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v4i16_0 = extractelement <4 x i16> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v5i16_0 = extractelement <5 x i16> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 0 for instruction: %v8i16_0 = extractelement <8 x i16> undef, i32 0
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_1 = extractelement <2 x i16> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2f16_1 = extractelement <2 x half> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_1 = extractelement <3 x i16> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_1 = extractelement <4 x i16> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_1 = extractelement <5 x i16> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_1 = extractelement <8 x i16> undef, i32 1
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v3i16_2 = extractelement <3 x i16> undef, i32 2
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_2 = extractelement <4 x i16> undef, i32 2
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_2 = extractelement <5 x i16> undef, i32 2
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_2 = extractelement <8 x i16> undef, i32 2
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_3 = extractelement <4 x i16> undef, i32 3
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v5i16_3 = extractelement <5 x i16> undef, i32 3
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_3 = extractelement <8 x i16> undef, i32 3
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v2i16_a = extractelement <2 x i16> undef, i32 %arg
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v4i16_a = extractelement <4 x i16> undef, i32 %arg
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: %v8i16_a = extractelement <8 x i16> undef, i32 %arg
; GFX89-SIZE-NEXT:  Cost Model: Found an estimated cost of 1 for instruction: ret void
;
  %v2i16_0 = extractelement <2 x i16> undef, i32 0
  %v2f16_0 = extractelement <2 x half> undef, i32 0
  %v3i16_0 = extractelement <3 x i16> undef, i32 0
  %v4i16_0 = extractelement <4 x i16> undef, i32 0
  %v5i16_0 = extractelement <5 x i16> undef, i32 0
  %v8i16_0 = extractelement <8 x i16> undef, i32 0

  %v2i16_1 = extractelement <2 x i16> undef, i32 1
  %v2f16_1 = extractelement <2 x half> undef, i32 1
  %v3i16_1 = extractelement <3 x i16> undef, i32 1
  %v4i16_1 = extractelement <4 x i16> undef, i32 1
  %v5i16_1 = extractelement <5 x i16> undef, i32 1
  %v8i16_1 = extractelement <8 x i16> undef, i32 1

  %v3i16_2 = extractelement <3 x i16> undef, i32 2
  %v4i16_2 = extractelement <4 x i16> undef, i32 2
  %v5i16_2 = extractelement <5 x i16> undef, i32 2
  %v8i16_2 = extractelement <8 x i16> undef, i32 2

  %v4i16_3 = extractelement <4 x i16> undef, i32 3
  %v5i16_3 = extractelement <5 x i16> undef, i32 3
  %v8i16_3 = extractelement <8 x i16> undef, i32 3

  %v2i16_a = extractelement <2 x i16> undef, i32 %arg
  %v4i16_a = extractelement <4 x i16> undef, i32 %arg
  %v8i16_a = extractelement <8 x i16> undef, i32 %arg
  ret void
}
