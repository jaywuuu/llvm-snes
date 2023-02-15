; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -S -mtriple=x86_64-unknown -mattr=+avx -slp-vectorizer | FileCheck %s



define i32 @jumbled-load(i32* noalias nocapture %in, i32* noalias nocapture %inn, i32* noalias nocapture %out) {
; CHECK-LABEL: @jumbled-load(
; CHECK-NEXT:    [[IN_ADDR:%.*]] = getelementptr inbounds i32, i32* [[IN:%.*]], i64 0
; CHECK-NEXT:    [[INN_ADDR:%.*]] = getelementptr inbounds i32, i32* [[INN:%.*]], i64 0
; CHECK-NEXT:    [[GEP_7:%.*]] = getelementptr inbounds i32, i32* [[OUT:%.*]], i64 0
; CHECK-NEXT:    [[TMP1:%.*]] = bitcast i32* [[IN_ADDR]] to <4 x i32>*
; CHECK-NEXT:    [[TMP2:%.*]] = load <4 x i32>, <4 x i32>* [[TMP1]], align 4
; CHECK-NEXT:    [[TMP3:%.*]] = bitcast i32* [[INN_ADDR]] to <4 x i32>*
; CHECK-NEXT:    [[TMP4:%.*]] = load <4 x i32>, <4 x i32>* [[TMP3]], align 4
; CHECK-NEXT:    [[TMP5:%.*]] = mul <4 x i32> [[TMP2]], [[TMP4]]
; CHECK-NEXT:    [[SHUFFLE:%.*]] = shufflevector <4 x i32> [[TMP5]], <4 x i32> poison, <4 x i32> <i32 1, i32 3, i32 0, i32 2>
; CHECK-NEXT:    [[TMP6:%.*]] = bitcast i32* [[GEP_7]] to <4 x i32>*
; CHECK-NEXT:    store <4 x i32> [[SHUFFLE]], <4 x i32>* [[TMP6]], align 4
; CHECK-NEXT:    ret i32 undef
;
  %in.addr = getelementptr inbounds i32, i32* %in, i64 0
  %load.1 = load i32, i32* %in.addr, align 4
  %gep.1 = getelementptr inbounds i32, i32* %in.addr, i64 1
  %load.2 = load i32, i32* %gep.1, align 4
  %gep.2 = getelementptr inbounds i32, i32* %in.addr, i64 2
  %load.3 = load i32, i32* %gep.2, align 4
  %gep.3 = getelementptr inbounds i32, i32* %in.addr, i64 3
  %load.4 = load i32, i32* %gep.3, align 4
  %inn.addr = getelementptr inbounds i32, i32* %inn, i64 0
  %load.5 = load i32, i32* %inn.addr, align 4
  %gep.4 = getelementptr inbounds i32, i32* %inn.addr, i64 1
  %load.6 = load i32, i32* %gep.4, align 4
  %gep.5 = getelementptr inbounds i32, i32* %inn.addr, i64 2
  %load.7 = load i32, i32* %gep.5, align 4
  %gep.6 = getelementptr inbounds i32, i32* %inn.addr, i64 3
  %load.8 = load i32, i32* %gep.6, align 4
  %mul.1 = mul i32 %load.1, %load.5
  %mul.2 = mul i32 %load.2, %load.6
  %mul.3 = mul i32 %load.3, %load.7
  %mul.4 = mul i32 %load.4, %load.8
  %gep.7 = getelementptr inbounds i32, i32* %out, i64 0
  %gep.8 = getelementptr inbounds i32, i32* %out, i64 1
  %gep.9 = getelementptr inbounds i32, i32* %out, i64 2
  %gep.10 = getelementptr inbounds i32, i32* %out, i64 3
  store i32 %mul.1, i32* %gep.9, align 4
  store i32 %mul.2, i32* %gep.7, align 4
  store i32 %mul.3, i32* %gep.10, align 4
  store i32 %mul.4, i32* %gep.8, align 4

  ret i32 undef
}
