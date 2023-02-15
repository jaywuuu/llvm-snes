; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --function-signature --scrub-attributes --check-globals
; call site specific analysis is enabled

; RUN: opt -attributor -enable-new-pm=0 -attributor-enable-call-site-specific-deduction=true -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=2 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_NPM,NOT_CGSCC_OPM,NOT_TUNIT_NPM,IS__TUNIT____,IS________OPM,IS__TUNIT_OPM

; RUN: opt -aa-pipeline=basic-aa -passes=attributor -attributor-enable-call-site-specific-deduction=true -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=2 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_OPM,NOT_CGSCC_NPM,NOT_TUNIT_OPM,IS__TUNIT____,IS________NPM,IS__TUNIT_NPM

; RUN: opt -attributor-cgscc -attributor-enable-call-site-specific-deduction=true -enable-new-pm=0 -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_NPM,IS__CGSCC____,IS________OPM,IS__CGSCC_OPM

; RUN: opt -aa-pipeline=basic-aa -passes=attributor-cgscc -attributor-enable-call-site-specific-deduction=true -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_OPM,IS__CGSCC____,IS________NPM,IS__CGSCC_NPM

define dso_local i32 @test_range1(i32 %0) #0 {
; CHECK-LABEL: define {{[^@]+}}@test_range1
; CHECK-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0:[0-9]+]] {
; CHECK-NEXT:    [[TMP2:%.*]] = icmp ne i32 [[TMP0]], 0
; CHECK-NEXT:    br i1 [[TMP2]], label [[TMP3:%.*]], label [[TMP4:%.*]]
; CHECK:       3:
; CHECK-NEXT:    br label [[TMP5:%.*]]
; CHECK:       4:
; CHECK-NEXT:    br label [[TMP5]]
; CHECK:       5:
; CHECK-NEXT:    [[DOT0:%.*]] = phi i32 [ 100, [[TMP3]] ], [ 0, [[TMP4]] ]
; CHECK-NEXT:    ret i32 [[DOT0]]
;
  %2 = icmp ne i32 %0, 0
  br i1 %2, label %3, label %4

3:                                                ; preds = %1
  br label %5

4:                                                ; preds = %1
  br label %5

5:                                                ; preds = %4, %3
  %.0 = phi i32 [ 100, %3 ], [ 0, %4 ]
  ret i32 %.0
}

define i32 @test_range2(i32 %0) #0 {
; CHECK-LABEL: define {{[^@]+}}@test_range2
; CHECK-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0]] {
; CHECK-NEXT:    [[TMP2:%.*]] = icmp ne i32 [[TMP0]], 0
; CHECK-NEXT:    br i1 [[TMP2]], label [[TMP3:%.*]], label [[TMP4:%.*]]
; CHECK:       3:
; CHECK-NEXT:    br label [[TMP5:%.*]]
; CHECK:       4:
; CHECK-NEXT:    br label [[TMP5]]
; CHECK:       5:
; CHECK-NEXT:    [[DOT0:%.*]] = phi i32 [ 100, [[TMP3]] ], [ 200, [[TMP4]] ]
; CHECK-NEXT:    ret i32 [[DOT0]]
;
  %2 = icmp ne i32 %0, 0
  br i1 %2, label %3, label %4

3:                                                ; preds = %1
  br label %5

4:                                                ; preds = %1
  br label %5

5:                                                ; preds = %4, %3
  %.0 = phi i32 [ 100, %3 ], [ 200, %4 ]
  ret i32 %.0
}
define i32 @test(i32 %0, i32 %1) #0 {
; IS__TUNIT____-LABEL: define {{[^@]+}}@test
; IS__TUNIT____-SAME: (i32 [[TMP0:%.*]], i32 [[TMP1:%.*]]) #[[ATTR0]] {
; IS__TUNIT____-NEXT:    [[TMP3:%.*]] = icmp ne i32 [[TMP1]], 0
; IS__TUNIT____-NEXT:    br i1 [[TMP3]], label [[TMP4:%.*]], label [[TMP6:%.*]]
; IS__TUNIT____:       4:
; IS__TUNIT____-NEXT:    [[TMP5:%.*]] = call i32 @test_range1(i32 [[TMP0]])
; IS__TUNIT____-NEXT:    br label [[TMP8:%.*]]
; IS__TUNIT____:       6:
; IS__TUNIT____-NEXT:    [[TMP7:%.*]] = call i32 @test_range2(i32 [[TMP0]])
; IS__TUNIT____-NEXT:    br label [[TMP8]]
; IS__TUNIT____:       8:
; IS__TUNIT____-NEXT:    [[DOT0:%.*]] = phi i32 [ [[TMP5]], [[TMP4]] ], [ [[TMP7]], [[TMP6]] ]
; IS__TUNIT____-NEXT:    ret i32 [[DOT0]]
;
; IS__CGSCC____-LABEL: define {{[^@]+}}@test
; IS__CGSCC____-SAME: (i32 [[TMP0:%.*]], i32 [[TMP1:%.*]]) #[[ATTR1:[0-9]+]] {
; IS__CGSCC____-NEXT:    [[TMP3:%.*]] = icmp ne i32 [[TMP1]], 0
; IS__CGSCC____-NEXT:    br i1 [[TMP3]], label [[TMP4:%.*]], label [[TMP6:%.*]]
; IS__CGSCC____:       4:
; IS__CGSCC____-NEXT:    [[TMP5:%.*]] = call noundef i32 @test_range1(i32 [[TMP0]])
; IS__CGSCC____-NEXT:    br label [[TMP8:%.*]]
; IS__CGSCC____:       6:
; IS__CGSCC____-NEXT:    [[TMP7:%.*]] = call noundef i32 @test_range2(i32 [[TMP0]])
; IS__CGSCC____-NEXT:    br label [[TMP8]]
; IS__CGSCC____:       8:
; IS__CGSCC____-NEXT:    [[DOT0:%.*]] = phi i32 [ [[TMP5]], [[TMP4]] ], [ [[TMP7]], [[TMP6]] ]
; IS__CGSCC____-NEXT:    ret i32 [[DOT0]]
;
  %3 = icmp ne i32 %1, 0
  br i1 %3, label %4, label %6

4:                                                ; preds = %2
  %5 = call i32 @test_range1(i32 %0)
  br label %8

6:                                                ; preds = %2
  %7 = call i32 @test_range2(i32 %0)
  br label %8

8:                                                ; preds = %6, %4
  %.0 = phi i32 [ %5, %4 ], [ %7, %6 ]
  ret i32 %.0
}

define i32 @test_pcheck1(i32 %0) #0 {
; IS__TUNIT____-LABEL: define {{[^@]+}}@test_pcheck1
; IS__TUNIT____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0]] {
; IS__TUNIT____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 1)
; IS__TUNIT____-NEXT:    [[TMP3:%.*]] = icmp slt i32 [[TMP2]], 101
; IS__TUNIT____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__TUNIT____-NEXT:    ret i32 [[TMP4]]
;
; IS__CGSCC____-LABEL: define {{[^@]+}}@test_pcheck1
; IS__CGSCC____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR1]] {
; IS__CGSCC____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 1)
; IS__CGSCC____-NEXT:    [[TMP3:%.*]] = icmp slt i32 [[TMP2]], 101
; IS__CGSCC____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__CGSCC____-NEXT:    ret i32 [[TMP4]]
;
; IS__TUNIT_____ENABLED-LABEL: define {{[^@]+}}@test_pcheck1
; IS__TUNIT_____ENABLED-SAME: (i32 [[TMP0:%.*]])
; IS__TUNIT_____ENABLED-NEXT:    ret i32 1
  %2 = call i32 @test(i32 %0, i32 1)
  %3 = icmp slt i32 %2, 101
  %4 = zext i1 %3 to i32
  ret i32 %4
}

define i32 @test_pcheck2(i32 %0) #0 {
; IS__TUNIT____-LABEL: define {{[^@]+}}@test_pcheck2
; IS__TUNIT____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0]] {
; IS__TUNIT____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 0)
; IS__TUNIT____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 99
; IS__TUNIT____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__TUNIT____-NEXT:    ret i32 [[TMP4]]
;
; IS__CGSCC____-LABEL: define {{[^@]+}}@test_pcheck2
; IS__CGSCC____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR1]] {
; IS__CGSCC____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 0)
; IS__CGSCC____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 99
; IS__CGSCC____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__CGSCC____-NEXT:    ret i32 [[TMP4]]
;
  %2 = call i32 @test(i32 %0, i32 0)
  %3 = icmp sgt i32 %2, 99
  %4 = zext i1 %3 to i32
  ret i32 %4
}

define i32 @test_ncheck1(i32 %0) #0 {
; IS__TUNIT____-LABEL: define {{[^@]+}}@test_ncheck1
; IS__TUNIT____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0]] {
; IS__TUNIT____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 1)
; IS__TUNIT____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 50
; IS__TUNIT____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__TUNIT____-NEXT:    ret i32 [[TMP4]]
;
; IS__CGSCC____-LABEL: define {{[^@]+}}@test_ncheck1
; IS__CGSCC____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR1]] {
; IS__CGSCC____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 1)
; IS__CGSCC____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 50
; IS__CGSCC____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__CGSCC____-NEXT:    ret i32 [[TMP4]]
;
  %2 = call i32 @test(i32 %0, i32 1)
  %3 = icmp sgt i32 %2, 50
  %4 = zext i1 %3 to i32
  ret i32 %4
}

define i32 @test_ncheck2(i32 %0) #0 {
; IS__TUNIT____-LABEL: define {{[^@]+}}@test_ncheck2
; IS__TUNIT____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR0]] {
; IS__TUNIT____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 0)
; IS__TUNIT____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 150
; IS__TUNIT____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__TUNIT____-NEXT:    ret i32 [[TMP4]]
;
; IS__CGSCC____-LABEL: define {{[^@]+}}@test_ncheck2
; IS__CGSCC____-SAME: (i32 [[TMP0:%.*]]) #[[ATTR1]] {
; IS__CGSCC____-NEXT:    [[TMP2:%.*]] = call i32 @test(i32 [[TMP0]], i32 noundef 0)
; IS__CGSCC____-NEXT:    [[TMP3:%.*]] = icmp sgt i32 [[TMP2]], 150
; IS__CGSCC____-NEXT:    [[TMP4:%.*]] = zext i1 [[TMP3]] to i32
; IS__CGSCC____-NEXT:    ret i32 [[TMP4]]
;
  %2 = call i32 @test(i32 %0, i32 0)
  %3 = icmp sgt i32 %2, 150
  %4 = zext i1 %3 to i32
  ret i32 %4
}

attributes #0 = { noinline nounwind sspstrong uwtable}

; IS__TUNIT_____: !0 = !{i32 0, i32 101}
; IS__TUNIT_____: !1 = !{i32 100, i32 201}
;.
; IS__TUNIT____: attributes #[[ATTR0]] = { nofree noinline norecurse nosync nounwind readnone sspstrong willreturn uwtable }
; IS__TUNIT____: attributes #[[ATTR1:[0-9]+]] = { nofree nosync nounwind readnone willreturn }
;.
; IS__CGSCC____: attributes #[[ATTR0]] = { nofree noinline norecurse nosync nounwind readnone sspstrong willreturn uwtable }
; IS__CGSCC____: attributes #[[ATTR1]] = { nofree noinline nosync nounwind readnone sspstrong willreturn uwtable }
; IS__CGSCC____: attributes #[[ATTR2:[0-9]+]] = { readnone willreturn }
;.
