; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --function-signature --check-attributes --check-globals
; RUN: opt -attributor -enable-new-pm=0 -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=2 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_NPM,NOT_CGSCC_OPM,NOT_TUNIT_NPM,IS__TUNIT____,IS________OPM,IS__TUNIT_OPM
; RUN: opt -aa-pipeline=basic-aa -passes=attributor -attributor-manifest-internal  -attributor-max-iterations-verify -attributor-annotate-decl-cs -attributor-max-iterations=2 -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_CGSCC_OPM,NOT_CGSCC_NPM,NOT_TUNIT_OPM,IS__TUNIT____,IS________NPM,IS__TUNIT_NPM
; RUN: opt -attributor-cgscc -enable-new-pm=0 -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_NPM,IS__CGSCC____,IS________OPM,IS__CGSCC_OPM
; RUN: opt -aa-pipeline=basic-aa -passes=attributor-cgscc -attributor-manifest-internal  -attributor-annotate-decl-cs -S < %s | FileCheck %s --check-prefixes=CHECK,NOT_TUNIT_NPM,NOT_TUNIT_OPM,NOT_CGSCC_OPM,IS__CGSCC____,IS________NPM,IS__CGSCC_NPM

; Should not propagate the result of a weak function.
; PR2411

define weak i32 @foo() nounwind  {
; CHECK: Function Attrs: nounwind
; CHECK-LABEL: define {{[^@]+}}@foo
; CHECK-SAME: () #[[ATTR0:[0-9]+]] {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    ret i32 1
;
entry:
  ret i32 1
}

define i32 @main() nounwind  {
; IS__TUNIT____: Function Attrs: norecurse nounwind
; IS__TUNIT____-LABEL: define {{[^@]+}}@main
; IS__TUNIT____-SAME: () #[[ATTR1:[0-9]+]] {
; IS__TUNIT____-NEXT:  entry:
; IS__TUNIT____-NEXT:    [[R:%.*]] = call i32 @foo() #[[ATTR0]]
; IS__TUNIT____-NEXT:    ret i32 [[R]]
;
; IS__CGSCC____: Function Attrs: nounwind
; IS__CGSCC____-LABEL: define {{[^@]+}}@main
; IS__CGSCC____-SAME: () #[[ATTR0]] {
; IS__CGSCC____-NEXT:  entry:
; IS__CGSCC____-NEXT:    [[R:%.*]] = call i32 @foo() #[[ATTR0]]
; IS__CGSCC____-NEXT:    ret i32 [[R]]
;
entry:
  %r = call i32 @foo( ) nounwind
  ret i32 %r
}

;.
; IS__TUNIT____: attributes #[[ATTR0]] = { nounwind }
; IS__TUNIT____: attributes #[[ATTR1]] = { norecurse nounwind }
;.
; IS__CGSCC____: attributes #[[ATTR0]] = { nounwind }
;.
