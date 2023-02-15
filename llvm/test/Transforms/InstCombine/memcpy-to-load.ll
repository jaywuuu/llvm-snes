; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -passes=instcombine -S                         | FileCheck %s
; RUN: opt < %s -passes=instcombine -S -data-layout=n32        | FileCheck %s
; RUN: opt < %s -passes=instcombine -S -data-layout=n32:64     | FileCheck %s
; RUN: opt < %s -passes=instcombine -S -data-layout=n32:64:128 | FileCheck %s

declare void @llvm.memcpy.p0i8.p0i8.i32(i8* nocapture, i8* nocapture, i32, i1) nounwind

; memcpy can be expanded inline with load/store (based on the datalayout?)

define void @copy_1_byte(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_1_byte(
; CHECK-NEXT:    [[TMP1:%.*]] = load i8, i8* [[S:%.*]], align 1
; CHECK-NEXT:    store i8 [[TMP1]], i8* [[D:%.*]], align 1
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 1, i1 false)
  ret void
}

define void @copy_2_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_2_bytes(
; CHECK-NEXT:    [[TMP1:%.*]] = bitcast i8* [[S:%.*]] to i16*
; CHECK-NEXT:    [[TMP2:%.*]] = bitcast i8* [[D:%.*]] to i16*
; CHECK-NEXT:    [[TMP3:%.*]] = load i16, i16* [[TMP1]], align 1
; CHECK-NEXT:    store i16 [[TMP3]], i16* [[TMP2]], align 1
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 2, i1 false)
  ret void
}

; We don't expand small non-power-of-2. Should we? Might be a target-dependent choice.

define void @copy_3_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_3_bytes(
; CHECK-NEXT:    call void @llvm.memcpy.p0i8.p0i8.i32(i8* noundef nonnull align 1 dereferenceable(3) [[D:%.*]], i8* noundef nonnull align 1 dereferenceable(3) [[S:%.*]], i32 3, i1 false)
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 3, i1 false)
  ret void
}

define void @copy_4_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_4_bytes(
; CHECK-NEXT:    [[TMP1:%.*]] = bitcast i8* [[S:%.*]] to i32*
; CHECK-NEXT:    [[TMP2:%.*]] = bitcast i8* [[D:%.*]] to i32*
; CHECK-NEXT:    [[TMP3:%.*]] = load i32, i32* [[TMP1]], align 1
; CHECK-NEXT:    store i32 [[TMP3]], i32* [[TMP2]], align 1
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 4, i1 false)
  ret void
}

; We don't expand small non-power-of-2. Should we? Might be a target-dependent choice.

define void @copy_5_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_5_bytes(
; CHECK-NEXT:    call void @llvm.memcpy.p0i8.p0i8.i32(i8* noundef nonnull align 1 dereferenceable(5) [[D:%.*]], i8* noundef nonnull align 1 dereferenceable(5) [[S:%.*]], i32 5, i1 false)
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 5, i1 false)
  ret void
}

define void @copy_8_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_8_bytes(
; CHECK-NEXT:    [[TMP1:%.*]] = bitcast i8* [[S:%.*]] to i64*
; CHECK-NEXT:    [[TMP2:%.*]] = bitcast i8* [[D:%.*]] to i64*
; CHECK-NEXT:    [[TMP3:%.*]] = load i64, i64* [[TMP1]], align 1
; CHECK-NEXT:    store i64 [[TMP3]], i64* [[TMP2]], align 1
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 8, i1 false)
  ret void
}

define void @copy_16_bytes(i8* %d, i8* %s) {
; CHECK-LABEL: @copy_16_bytes(
; CHECK-NEXT:    call void @llvm.memcpy.p0i8.p0i8.i32(i8* noundef nonnull align 1 dereferenceable(16) [[D:%.*]], i8* noundef nonnull align 1 dereferenceable(16) [[S:%.*]], i32 16, i1 false)
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %d, i8* %s, i32 16, i1 false)
  ret void
}

