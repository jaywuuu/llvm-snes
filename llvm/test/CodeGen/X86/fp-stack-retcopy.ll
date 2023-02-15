; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; This should not copy the result of foo into an xmm register.
; RUN: llc < %s -mcpu=yonah -mtriple=i686-apple-darwin9 | FileCheck %s
; rdar://5689903

declare double @foo()

define double @carg(ptr byval({ double, double }) %z) nounwind  {
; CHECK-LABEL: carg:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    jmp _foo ## TAILCALL
entry:
	%tmp5 = tail call double @foo() nounwind 		; <double> [#uses=1]
	ret double %tmp5
}

