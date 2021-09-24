; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --function-signature --check-attributes
; RUN: opt -attributor -enable-new-pm=0 -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=9 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_NPM,NOT_CGSCC_OPM,NOT_TUNIT_NPM,IS__TUNIT____,IS________OPM,IS__TUNIT_OPM
; RUN: opt -aa-pipeline=basic-aa -passes=attributor -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=9 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_OPM,NOT_CGSCC_NPM,NOT_TUNIT_OPM,IS__TUNIT____,IS________NPM,IS__TUNIT_NPM
; RUN: opt -attributor-cgscc -enable-new-pm=0 -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_NPM,IS__CGSCC____,IS________OPM,IS__CGSCC_OPM
; RUN: opt -aa-pipeline=basic-aa -passes=attributor-cgscc -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_OPM,IS__CGSCC____,IS________NPM,IS__CGSCC_NPM

define i32 @leaf() {
; IS__TUNIT____: Function Attrs: nofree nosync nounwind readnone willreturn
; IS__TUNIT____-LABEL: define {{[^@]+}}@leaf
; IS__TUNIT____-SAME: () [[ATTR0:#.*]] {
; IS__TUNIT____-NEXT:    ret i32 1
;
; IS__CGSCC____: Function Attrs: nofree norecurse nosync nounwind readnone willreturn
; IS__CGSCC____-LABEL: define {{[^@]+}}@leaf
; IS__CGSCC____-SAME: () [[ATTR0:#.*]] {
; IS__CGSCC____-NEXT:    ret i32 1
;
  ret i32 1
}

define i32 @self_rec() {
; IS__TUNIT____: Function Attrs: nofree noreturn nosync nounwind readnone willreturn
; IS__TUNIT____-LABEL: define {{[^@]+}}@self_rec
; IS__TUNIT____-SAME: () [[ATTR1:#.*]] {
; IS__TUNIT____-NEXT:    unreachable
;
; IS__CGSCC____: Function Attrs: nofree norecurse noreturn nosync nounwind readnone willreturn
; IS__CGSCC____-LABEL: define {{[^@]+}}@self_rec
; IS__CGSCC____-SAME: () [[ATTR1:#.*]] {
; IS__CGSCC____-NEXT:    unreachable
;
  %a = call i32 @self_rec()
  ret i32 4
}

define i32 @indirect_rec() {
; CHECK: Function Attrs: nofree noreturn nosync nounwind readnone
; CHECK-LABEL: define {{[^@]+}}@indirect_rec
; CHECK-SAME: () [[ATTR2:#.*]] {
; CHECK-NEXT:    [[A:%.*]] = call i32 @indirect_rec2() [[ATTR2]]
; CHECK-NEXT:    unreachable
;
  %a = call i32 @indirect_rec2()
  ret i32 %a
}
define i32 @indirect_rec2() {
; CHECK: Function Attrs: nofree noreturn nosync nounwind readnone
; CHECK-LABEL: define {{[^@]+}}@indirect_rec2
; CHECK-SAME: () [[ATTR2]] {
; CHECK-NEXT:    [[A:%.*]] = call i32 @indirect_rec() [[ATTR2]]
; CHECK-NEXT:    unreachable
;
  %a = call i32 @indirect_rec()
  ret i32 %a
}

define i32 @extern() {
; CHECK: Function Attrs: nosync readnone
; CHECK-LABEL: define {{[^@]+}}@extern
; CHECK-SAME: () [[ATTR3:#.*]] {
; CHECK-NEXT:    [[A:%.*]] = call i32 @k()
; CHECK-NEXT:    ret i32 [[A]]
;
  %a = call i32 @k()
  ret i32 %a
}

; CHECK: Function Attrs
; CHECK-NEXT: declare i32 @k()
declare i32 @k() readnone

define void @intrinsic(i8* %dest, i8* %src, i32 %len) {
; CHECK: Function Attrs: argmemonly nofree nosync nounwind willreturn
; CHECK-LABEL: define {{[^@]+}}@intrinsic
; CHECK-SAME: (i8* nocapture nofree writeonly [[DEST:%.*]], i8* nocapture nofree readonly [[SRC:%.*]], i32 [[LEN:%.*]]) [[ATTR5:#.*]] {
; CHECK-NEXT:    call void @llvm.memcpy.p0i8.p0i8.i32(i8* noalias nocapture nofree writeonly [[DEST]], i8* noalias nocapture nofree readonly [[SRC]], i32 [[LEN]], i1 noundef false) [[ATTR10:#.*]]
; CHECK-NEXT:    ret void
;
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %dest, i8* %src, i32 %len, i1 false)
  ret void
}

; CHECK: Function Attrs
; CHECK-NEXT: declare void @llvm.memcpy.p0i8.p0i8.i32
declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i1)

define internal i32 @called_by_norecurse() {
; IS__TUNIT____: Function Attrs: nosync readnone
; IS__TUNIT____-LABEL: define {{[^@]+}}@called_by_norecurse
; IS__TUNIT____-SAME: () [[ATTR3:#.*]] {
; IS__TUNIT____-NEXT:    [[A:%.*]] = call i32 @k()
; IS__TUNIT____-NEXT:    ret i32 undef
;
; IS__CGSCC____: Function Attrs: norecurse nosync readnone
; IS__CGSCC____-LABEL: define {{[^@]+}}@called_by_norecurse
; IS__CGSCC____-SAME: () [[ATTR6:#.*]] {
; IS__CGSCC____-NEXT:    [[A:%.*]] = call i32 @k()
; IS__CGSCC____-NEXT:    ret i32 undef
;
  %a = call i32 @k()
  ret i32 %a
}
define void @m() norecurse {
; IS__TUNIT____: Function Attrs: norecurse nosync readnone
; IS__TUNIT____-LABEL: define {{[^@]+}}@m
; IS__TUNIT____-SAME: () [[ATTR6:#.*]] {
; IS__TUNIT____-NEXT:    [[A:%.*]] = call i32 @called_by_norecurse() [[ATTR3]]
; IS__TUNIT____-NEXT:    ret void
;
; IS__CGSCC____: Function Attrs: norecurse nosync readnone
; IS__CGSCC____-LABEL: define {{[^@]+}}@m
; IS__CGSCC____-SAME: () [[ATTR6]] {
; IS__CGSCC____-NEXT:    [[A:%.*]] = call i32 @called_by_norecurse()
; IS__CGSCC____-NEXT:    ret void
;
  %a = call i32 @called_by_norecurse()
  ret void
}

define internal i32 @called_by_norecurse_indirectly() {
; CHECK: Function Attrs: nosync readnone
; CHECK-LABEL: define {{[^@]+}}@called_by_norecurse_indirectly
; CHECK-SAME: () [[ATTR3]] {
; CHECK-NEXT:    [[A:%.*]] = call i32 @k()
; CHECK-NEXT:    ret i32 [[A]]
;
  %a = call i32 @k()
  ret i32 %a
}
define internal i32 @o() {
; IS__TUNIT____: Function Attrs: nosync readnone
; IS__TUNIT____-LABEL: define {{[^@]+}}@o
; IS__TUNIT____-SAME: () [[ATTR3]] {
; IS__TUNIT____-NEXT:    [[A:%.*]] = call i32 @called_by_norecurse_indirectly() [[ATTR3]]
; IS__TUNIT____-NEXT:    ret i32 [[A]]
;
; IS__CGSCC____: Function Attrs: norecurse nosync readnone
; IS__CGSCC____-LABEL: define {{[^@]+}}@o
; IS__CGSCC____-SAME: () [[ATTR6]] {
; IS__CGSCC____-NEXT:    [[A:%.*]] = call i32 @called_by_norecurse_indirectly()
; IS__CGSCC____-NEXT:    ret i32 [[A]]
;
  %a = call i32 @called_by_norecurse_indirectly()
  ret i32 %a
}
define i32 @p() norecurse {
; IS__TUNIT____: Function Attrs: norecurse nosync readnone
; IS__TUNIT____-LABEL: define {{[^@]+}}@p
; IS__TUNIT____-SAME: () [[ATTR6]] {
; IS__TUNIT____-NEXT:    [[A:%.*]] = call i32 @o() [[ATTR3]]
; IS__TUNIT____-NEXT:    ret i32 [[A]]
;
; IS__CGSCC____: Function Attrs: norecurse nosync readnone
; IS__CGSCC____-LABEL: define {{[^@]+}}@p
; IS__CGSCC____-SAME: () [[ATTR6]] {
; IS__CGSCC____-NEXT:    [[A:%.*]] = call i32 @o()
; IS__CGSCC____-NEXT:    ret i32 [[A]]
;
  %a = call i32 @o()
  ret i32 %a
}

define void @f(i32 %x)  {
; CHECK: Function Attrs: nofree nosync nounwind readnone
; CHECK-LABEL: define {{[^@]+}}@f
; CHECK-SAME: (i32 [[X:%.*]]) [[ATTR7:#.*]] {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[X_ADDR:%.*]] = alloca i32, align 4
; CHECK-NEXT:    store i32 [[X]], i32* [[X_ADDR]], align 4
; CHECK-NEXT:    [[TMP0:%.*]] = load i32, i32* [[X_ADDR]], align 4
; CHECK-NEXT:    [[TOBOOL:%.*]] = icmp ne i32 [[TMP0]], 0
; CHECK-NEXT:    br i1 [[TOBOOL]], label [[IF_THEN:%.*]], label [[IF_END:%.*]]
; CHECK:       if.then:
; CHECK-NEXT:    call void @g() [[ATTR8:#.*]]
; CHECK-NEXT:    br label [[IF_END]]
; CHECK:       if.end:
; CHECK-NEXT:    ret void
;
entry:
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  %0 = load i32, i32* %x.addr, align 4
  %tobool = icmp ne i32 %0, 0
  br i1 %tobool, label %if.then, label %if.end

if.then:
  call void @g() norecurse
  br label %if.end

if.end:
  ret void
}

define void @g() norecurse {
; CHECK: Function Attrs: nofree norecurse nosync nounwind readnone
; CHECK-LABEL: define {{[^@]+}}@g
; CHECK-SAME: () [[ATTR8]] {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    call void @f(i32 noundef 0) [[ATTR7]]
; CHECK-NEXT:    ret void
;
entry:
  call void @f(i32 0)
  ret void
}

define linkonce_odr i32 @leaf_redefinable() {
; CHECK-LABEL: define {{[^@]+}}@leaf_redefinable() {
; CHECK-NEXT:    ret i32 1
;
  ret i32 1
}

; Call through a function pointer
define i32 @eval_func1(i32 (i32)* , i32) local_unnamed_addr {
; CHECK-LABEL: define {{[^@]+}}@eval_func1
; CHECK-SAME: (i32 (i32)* nocapture nofree nonnull [[TMP0:%.*]], i32 [[TMP1:%.*]]) local_unnamed_addr {
; CHECK-NEXT:    [[TMP3:%.*]] = tail call i32 [[TMP0]](i32 [[TMP1]])
; CHECK-NEXT:    ret i32 [[TMP3]]
;
  %3 = tail call i32 %0(i32 %1) #2
  ret i32 %3
}

define i32 @eval_func2(i32 (i32)* , i32) local_unnamed_addr null_pointer_is_valid{
; CHECK: Function Attrs: null_pointer_is_valid
; CHECK-LABEL: define {{[^@]+}}@eval_func2
; CHECK-SAME: (i32 (i32)* nocapture nofree [[TMP0:%.*]], i32 [[TMP1:%.*]]) local_unnamed_addr [[ATTR9:#.*]] {
; CHECK-NEXT:    [[TMP3:%.*]] = tail call i32 [[TMP0]](i32 [[TMP1]])
; CHECK-NEXT:    ret i32 [[TMP3]]
;
  %3 = tail call i32 %0(i32 %1) #2
  ret i32 %3
}

; Call an unknown function in a dead block.
declare void @unknown()
define i32 @call_unknown_in_dead_block() local_unnamed_addr {
; IS__TUNIT____: Function Attrs: nofree nosync nounwind readnone willreturn
; IS__TUNIT____-LABEL: define {{[^@]+}}@call_unknown_in_dead_block
; IS__TUNIT____-SAME: () local_unnamed_addr [[ATTR0]] {
; IS__TUNIT____-NEXT:    ret i32 0
; IS__TUNIT____:       Dead:
; IS__TUNIT____-NEXT:    unreachable
;
; IS__CGSCC____: Function Attrs: nofree norecurse nosync nounwind readnone willreturn
; IS__CGSCC____-LABEL: define {{[^@]+}}@call_unknown_in_dead_block
; IS__CGSCC____-SAME: () local_unnamed_addr [[ATTR0]] {
; IS__CGSCC____-NEXT:    ret i32 0
; IS__CGSCC____:       Dead:
; IS__CGSCC____-NEXT:    unreachable
;
  ret i32 0
Dead:
  tail call void @unknown()
  ret i32 1
}

