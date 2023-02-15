; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --function-signature
; Use instcombine to cleanup offset computation.
; Use gvn to remove duplicate computation.
; RUN: opt -passes=rewrite-statepoints-for-gc,gvn,instcombine -S < %s | FileCheck %s

target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128-p1:64:64"
target triple = "x86_64-apple-macosx10.11.0"

declare i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* readnone nocapture) nounwind readnone willreturn
declare i8 addrspace(1)* addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1p1i8.p1p1i8(i8 addrspace(1)* addrspace(1)* readnone nocapture) nounwind readnone willreturn
declare i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* readnone nocapture) nounwind readnone willreturn
declare i64 @llvm.experimental.gc.get.pointer.offset.p1p1i8(i8 addrspace(1)* addrspace(1)* readnone nocapture) nounwind readnone willreturn

declare void @foo() readonly

define i8 addrspace(1)* addrspace(1)* @test_simple(i8 addrspace(1)* %obj1, i8 addrspace(1)* %obj2, i32 %len, i1 %c) gc "statepoint-example" {
; CHECK-LABEL: define {{[^@]+}}@test_simple
; CHECK-SAME: (i8 addrspace(1)* [[OBJ1:%.*]], i8 addrspace(1)* [[OBJ2:%.*]], i32 [[LEN:%.*]], i1 [[C:%.*]]) gc "statepoint-example" {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[OBJ1_12:%.*]] = getelementptr inbounds i8, i8 addrspace(1)* [[OBJ1]], i64 12
; CHECK-NEXT:    [[OBJ2_16:%.*]] = getelementptr inbounds i8, i8 addrspace(1)* [[OBJ2]], i64 16
; CHECK-NEXT:    [[OBJ_X_BASE1:%.*]] = select i1 [[C]], i8 addrspace(1)* [[OBJ1]], i8 addrspace(1)* [[OBJ2]], !is_base_value !0
; CHECK-NEXT:    [[OBJ_X:%.*]] = select i1 [[C]], i8 addrspace(1)* [[OBJ1_12]], i8 addrspace(1)* [[OBJ2_16]]
; CHECK-NEXT:    [[OBJ_Y_BASE:%.*]] = select i1 [[C]], i8 addrspace(1)* [[OBJ2]], i8 addrspace(1)* [[OBJ1]], !is_base_value !0
; CHECK-NEXT:    [[OBJ_Y:%.*]] = select i1 [[C]], i8 addrspace(1)* [[OBJ2_16]], i8 addrspace(1)* [[OBJ1_12]]
; CHECK-NEXT:    [[OBJ_YA:%.*]] = bitcast i8 addrspace(1)* [[OBJ_Y]] to i8 addrspace(1)* addrspace(1)*
; CHECK-NEXT:    [[OBJ_X_BASE1_INT:%.*]] = ptrtoint i8 addrspace(1)* [[OBJ_X_BASE1]] to i64
; CHECK-NEXT:    [[OBJ_X_INT:%.*]] = ptrtoint i8 addrspace(1)* [[OBJ_X]] to i64
; CHECK-NEXT:    [[OBJ_X_OFFSET:%.*]] = sub i64 [[OBJ_X_INT]], [[OBJ_X_BASE1_INT]]
; CHECK-NEXT:    [[OBJ_Y_BASE_CAST:%.*]] = bitcast i8 addrspace(1)* [[OBJ_Y_BASE]] to i8 addrspace(1)* addrspace(1)*
; CHECK-NEXT:    [[OBJ_Y_BASE_INT:%.*]] = ptrtoint i8 addrspace(1)* [[OBJ_Y_BASE]] to i64
; CHECK-NEXT:    [[OBJ_YA_INT:%.*]] = ptrtoint i8 addrspace(1)* [[OBJ_Y]] to i64
; CHECK-NEXT:    [[OBJ_YA_OFFSET:%.*]] = sub i64 [[OBJ_YA_INT]], [[OBJ_Y_BASE_INT]]
; CHECK-NEXT:    [[STATEPOINT_TOKEN:%.*]] = call token (i64, i32, void ()*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidf(i64 2882400000, i32 0, void ()* nonnull elementtype(void ()) @foo, i32 0, i32 0, i32 0, i32 0) [ "deopt"(i8 addrspace(1)* [[OBJ_X_BASE1]], i64 [[OBJ_X_OFFSET]], i8 addrspace(1)* [[OBJ_X_BASE1]], i64 [[OBJ_X_OFFSET]], i8 addrspace(1)* addrspace(1)* [[OBJ_Y_BASE_CAST]], i64 [[OBJ_YA_OFFSET]]), "gc-live"(i8 addrspace(1)* addrspace(1)* [[OBJ_YA]], i8 addrspace(1)* [[OBJ_Y_BASE]]) ]
; CHECK-NEXT:    [[OBJ_YA_RELOCATED:%.*]] = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token [[STATEPOINT_TOKEN]], i32 1, i32 0)
; CHECK-NEXT:    [[OBJ_YA_RELOCATED_CASTED:%.*]] = bitcast i8 addrspace(1)* [[OBJ_YA_RELOCATED]] to i8 addrspace(1)* addrspace(1)*
; CHECK-NEXT:    ret i8 addrspace(1)* addrspace(1)* [[OBJ_YA_RELOCATED_CASTED]]
;
entry:
  %obj1.12 = getelementptr inbounds i8, i8 addrspace(1)* %obj1, i64 12
  %obj2.16 = getelementptr inbounds i8, i8 addrspace(1)* %obj2, i64 16
  %obj.x = select i1 %c, i8 addrspace(1)* %obj1.12, i8 addrspace(1)* %obj2.16
  %obj.y = select i1 %c, i8 addrspace(1)* %obj2.16, i8 addrspace(1)* %obj1.12
  %obj.ya = bitcast i8 addrspace(1)* %obj.y to i8 addrspace(1)* addrspace(1)*
  %obj.x.base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x)
  %obj.x.offset = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x)
  %obj.x.base2 = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x)
  %obj.x.offset2 = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x)
  %obj.ya.base = call i8 addrspace(1)* addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1p1i8.p1p1i8(i8 addrspace(1)* addrspace(1)* %obj.ya)
  %obj.ya.offset = call i64 @llvm.experimental.gc.get.pointer.offset.p1p1i8(i8 addrspace(1)* addrspace(1)* %obj.ya)
  call void @foo() readonly [
  "deopt"(i8 addrspace(1)* %obj.x.base, i64 %obj.x.offset, i8 addrspace(1)* %obj.x.base2, i64 %obj.x.offset2, i8 addrspace(1)* addrspace(1)* %obj.ya.base, i64 %obj.ya.offset) ]
  ret i8 addrspace(1)* addrspace(1)* %obj.ya
}

define void @test_base_of_base(i8 addrspace(1)* %obj1, i8 addrspace(1)* %obj2, i32 %len, i1 %c) gc "statepoint-example" {
; CHECK-LABEL: define {{[^@]+}}@test_base_of_base
; CHECK-SAME: (i8 addrspace(1)* [[OBJ1:%.*]], i8 addrspace(1)* [[OBJ2:%.*]], i32 [[LEN:%.*]], i1 [[C:%.*]]) gc "statepoint-example" {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret void
;
entry:
  %obj1.12 = getelementptr inbounds i8, i8 addrspace(1)* %obj1, i64 12
  %obj2.16 = getelementptr inbounds i8, i8 addrspace(1)* %obj2, i64 16
  %obj.x = select i1 %c, i8 addrspace(1)* %obj1.12, i8 addrspace(1)* %obj2.16

  %obj.x.base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x)
  %obj.x.base_of_base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base)

  ret void
}

define i8 addrspace(1)* @test_chained(i8 addrspace(1)* %obj1, i8 addrspace(1)* %obj2, i32 %len, i1 %c) gc "statepoint-example" {
; CHECK-LABEL: define {{[^@]+}}@test_chained
; CHECK-SAME: (i8 addrspace(1)* [[OBJ1:%.*]], i8 addrspace(1)* [[OBJ2:%.*]], i32 [[LEN:%.*]], i1 [[C:%.*]]) gc "statepoint-example" {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[OBJ_X_BASE1:%.*]] = select i1 [[C]], i8 addrspace(1)* [[OBJ1]], i8 addrspace(1)* [[OBJ2]], !is_base_value !0
; CHECK-NEXT:    [[OBJ_X_BASE_GEP:%.*]] = getelementptr inbounds i8, i8 addrspace(1)* [[OBJ_X_BASE1]], i64 8
; CHECK-NEXT:    [[OBJ_X_BASE_OF_BASE_GEP:%.*]] = getelementptr inbounds i8, i8 addrspace(1)* [[OBJ_X_BASE1]], i64 20
; CHECK-NEXT:    [[OBJ_X_BASE_OF_BASE_OF_BASE_GEP:%.*]] = getelementptr inbounds i8, i8 addrspace(1)* [[OBJ_X_BASE1]], i64 24
; CHECK-NEXT:    [[STATEPOINT_TOKEN:%.*]] = call token (i64, i32, void ()*, i32, i32, ...) @llvm.experimental.gc.statepoint.p0f_isVoidf(i64 2882400000, i32 0, void ()* nonnull elementtype(void ()) @foo, i32 0, i32 0, i32 0, i32 0) [ "deopt"(i8 addrspace(1)* [[OBJ_X_BASE1]], i8 addrspace(1)* [[OBJ_X_BASE1]], i8 addrspace(1)* [[OBJ_X_BASE1]], i8 addrspace(1)* [[OBJ_X_BASE_GEP]], i8 addrspace(1)* [[OBJ_X_BASE_OF_BASE_GEP]], i8 addrspace(1)* [[OBJ_X_BASE_OF_BASE_OF_BASE_GEP]], i8 addrspace(1)* [[OBJ_X_BASE1]], i8 addrspace(1)* [[OBJ_X_BASE1]], i8 addrspace(1)* [[OBJ_X_BASE1]], i64 0, i64 0, i64 0, i64 8, i64 20, i64 24, i64 0, i64 0, i64 0), "gc-live"(i8 addrspace(1)* [[OBJ_X_BASE1]]) ]
; CHECK-NEXT:    [[OBJ_X_BASE1_RELOCATED:%.*]] = call coldcc i8 addrspace(1)* @llvm.experimental.gc.relocate.p1i8(token [[STATEPOINT_TOKEN]], i32 0, i32 0)
; CHECK-NEXT:    ret i8 addrspace(1)* [[OBJ_X_BASE1_RELOCATED]]
;
entry:
  %obj1.12 = getelementptr inbounds i8, i8 addrspace(1)* %obj1, i64 12
  %obj2.16 = getelementptr inbounds i8, i8 addrspace(1)* %obj2, i64 16
  %obj.x = select i1 %c, i8 addrspace(1)* %obj1.12, i8 addrspace(1)* %obj2.16

  %obj.x.base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x)
  %obj.x.base_of_base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base)
  %obj.x.base_of_base_of_base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base_of_base)

  %obj.x.base_gep                 = getelementptr inbounds i8, i8 addrspace(1)* %obj.x.base, i64 8
  %obj.x.base_of_base_gep         = getelementptr inbounds i8, i8 addrspace(1)* %obj.x.base_of_base, i64 20
  %obj.x.base_of_base_of_base_gep = getelementptr inbounds i8, i8 addrspace(1)* %obj.x.base_of_base_of_base, i64 24

  %obj.x.base_gep_base                 = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base_gep)
  %obj.x.base_of_base_gep_base         = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base_of_base_gep)
  %obj.x.base_of_base_of_base_gep_base = call i8 addrspace(1)* @llvm.experimental.gc.get.pointer.base.p1i8.p1i8(i8 addrspace(1)* %obj.x.base_of_base_of_base_gep)

  %obj.x.base_offset                          = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base)
  %obj.x.base_of_base_offset                  = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base)
  %obj.x.base_of_base_of_base_offset          = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base_of_base)
  %obj.x.base_gep_offset                      = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_gep)
  %obj.x.base_of_base_gep_offset              = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base_gep)
  %obj.x.base_of_base_of_base_gep_offset      = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base_of_base_gep)
  %obj.x.base_gep_base_offset                 = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_gep_base)
  %obj.x.base_of_base_gep_base_offset         = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base_gep_base)
  %obj.x.base_of_base_of_base_gep_base_offset = call i64 @llvm.experimental.gc.get.pointer.offset.p1i8(i8 addrspace(1)* %obj.x.base_of_base_of_base_gep_base)

  call void @foo() readonly [
  "deopt"(
  i8 addrspace(1)* %obj.x.base,
  i8 addrspace(1)* %obj.x.base_of_base_of_base,
  i8 addrspace(1)* %obj.x.base_of_base,
  i8 addrspace(1)* %obj.x.base_gep,
  i8 addrspace(1)* %obj.x.base_of_base_gep,
  i8 addrspace(1)* %obj.x.base_of_base_of_base_gep,
  i8 addrspace(1)* %obj.x.base_gep_base,
  i8 addrspace(1)* %obj.x.base_of_base_gep_base,
  i8 addrspace(1)* %obj.x.base_of_base_of_base_gep_base,
  i64 %obj.x.base_offset,
  i64 %obj.x.base_of_base_offset,
  i64 %obj.x.base_of_base_of_base_offset,
  i64 %obj.x.base_gep_offset,
  i64 %obj.x.base_of_base_gep_offset,
  i64 %obj.x.base_of_base_of_base_gep_offset,
  i64 %obj.x.base_gep_base_offset,
  i64 %obj.x.base_of_base_gep_base_offset,
  i64 %obj.x.base_of_base_of_base_gep_base_offset) ]

  ret i8 addrspace(1)* %obj.x.base_of_base
}
