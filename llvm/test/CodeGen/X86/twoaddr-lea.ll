; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mcpu=generic -mtriple=x86_64-apple-darwin | FileCheck %s

;; X's live range extends beyond the shift, so the register allocator
;; cannot coalesce it with Y.  Because of this, a copy needs to be
;; emitted before the shift to save the register value before it is
;; clobbered.  However, this copy is not needed if the register
;; allocator turns the shift into an LEA.  This also occurs for ADD.

; Check that the shift gets turned into an LEA.

@G = external global i32

define i32 @test1(i32 %X) nounwind {
; CHECK-LABEL: test1:
; CHECK:       ## %bb.0:
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    leal 1(%rax), %ecx
; CHECK-NEXT:    movq _G@GOTPCREL(%rip), %rdx
; CHECK-NEXT:    movl %ecx, (%rdx)
; CHECK-NEXT:    ## kill: def $eax killed $eax killed $rax
; CHECK-NEXT:    retq
        %Z = add i32 %X, 1
        store volatile i32 %Z, ptr @G
        ret i32 %X
}

; rdar://8977508
; The second add should not be transformed to leal nor should it be
; commutted (which would require inserting a copy).
define i32 @test2(i32 inreg %a, i32 inreg %b, i32 %c, i32 %d) nounwind {
; CHECK-LABEL: test2:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    ## kill: def $esi killed $esi def $rsi
; CHECK-NEXT:    ## kill: def $edi killed $edi def $rdi
; CHECK-NEXT:    leal (%rdi,%rsi), %eax
; CHECK-NEXT:    addl %edx, %eax
; CHECK-NEXT:    addl %ecx, %eax
; CHECK-NEXT:    retq
entry:
 %add = add i32 %b, %a
 %add3 = add i32 %add, %c
 %add5 = add i32 %add3, %d
 ret i32 %add5
}

; rdar://9002648
define i64 @test3(i64 %x) nounwind readnone ssp {
; CHECK-LABEL: test3:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    leaq (%rdi,%rdi), %rax
; CHECK-NEXT:    retq
entry:
  %0 = shl i64 %x, 1
  ret i64 %0
}

@global = external global i32, align 4
@global2 = external global i64, align 8

; Test that liveness is properly updated and we do not encounter the
; assert/crash from http://llvm.org/PR28301
define void @ham() {
; CHECK-LABEL: ham:
; CHECK:       ## %bb.0: ## %bb
; CHECK-NEXT:    xorl %r8d, %r8d
; CHECK-NEXT:    movq _global@GOTPCREL(%rip), %rdx
; CHECK-NEXT:    movq _global2@GOTPCREL(%rip), %rsi
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    cmpl $10, %eax
; CHECK-NEXT:    jle LBB3_2
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB3_6: ## %bb2
; CHECK-NEXT:    ## =>This Loop Header: Depth=1
; CHECK-NEXT:    ## Child Loop BB3_7 Depth 2
; CHECK-NEXT:    movl (%rdx), %edi
; CHECK-NEXT:    leal (%rdi,%rax), %ecx
; CHECK-NEXT:    movslq %ecx, %rcx
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB3_7: ## %bb6
; CHECK-NEXT:    ## Parent Loop BB3_6 Depth=1
; CHECK-NEXT:    ## => This Inner Loop Header: Depth=2
; CHECK-NEXT:    movq %rax, (%rsi)
; CHECK-NEXT:    movq %rcx, (%rsi)
; CHECK-NEXT:    movl %edi, (%rdx)
; CHECK-NEXT:    testb %r8b, %r8b
; CHECK-NEXT:    jne LBB3_7
; CHECK-NEXT:  ## %bb.8: ## %bb9
; CHECK-NEXT:    ## in Loop: Header=BB3_6 Depth=1
; CHECK-NEXT:    addq $4, %rax
; CHECK-NEXT:    cmpl $10, %eax
; CHECK-NEXT:    jg LBB3_6
; CHECK-NEXT:  LBB3_2: ## %bb3.preheader
; CHECK-NEXT:    xorl %ecx, %ecx
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB3_3: ## %bb3
; CHECK-NEXT:    ## =>This Loop Header: Depth=1
; CHECK-NEXT:    ## Child Loop BB3_4 Depth 2
; CHECK-NEXT:    movq %rcx, %rdx
; CHECK-NEXT:    addq $4, %rcx
; CHECK-NEXT:    movl %eax, %esi
; CHECK-NEXT:    subl %edx, %esi
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB3_4: ## %bb4
; CHECK-NEXT:    ## Parent Loop BB3_3 Depth=1
; CHECK-NEXT:    ## => This Inner Loop Header: Depth=2
; CHECK-NEXT:    testl %esi, %esi
; CHECK-NEXT:    jne LBB3_9
; CHECK-NEXT:  ## %bb.5: ## %bb5
; CHECK-NEXT:    ## in Loop: Header=BB3_4 Depth=2
; CHECK-NEXT:    incq %rdx
; CHECK-NEXT:    cmpq %rcx, %rdx
; CHECK-NEXT:    jl LBB3_4
; CHECK-NEXT:    jmp LBB3_3
; CHECK-NEXT:  LBB3_9: ## %bb8
; CHECK-NEXT:    ud2
bb:
  br label %bb1

bb1:
  %tmp = phi i64 [ %tmp40, %bb9 ], [ 0, %bb ]
  %tmp2 = phi i32 [ %tmp39, %bb9 ], [ 0, %bb ]
  %tmp3 = icmp sgt i32 undef, 10
  br i1 %tmp3, label %bb2, label %bb3

bb2:
  %tmp6 = load i32, ptr @global, align 4
  %tmp8 = add nsw i32 %tmp6, %tmp2
  %tmp9 = sext i32 %tmp8 to i64
  br label %bb6

bb3:
  %tmp14 = phi i64 [ %tmp15, %bb5 ], [ 0, %bb1 ]
  %tmp15 = add nuw i64 %tmp14, 4
  %tmp16 = trunc i64 %tmp14 to i32
  %tmp17 = sub i32 %tmp2, %tmp16
  br label %bb4

bb4:
  %tmp20 = phi i64 [ %tmp14, %bb3 ], [ %tmp34, %bb5 ]
  %tmp28 = icmp eq i32 %tmp17, 0
  br i1 %tmp28, label %bb5, label %bb8

bb5:
  %tmp34 = add nuw nsw i64 %tmp20, 1
  %tmp35 = icmp slt i64 %tmp34, %tmp15
  br i1 %tmp35, label %bb4, label %bb3

bb6:
  store volatile i64 %tmp, ptr @global2, align 8
  store volatile i64 %tmp9, ptr @global2, align 8
  store volatile i32 %tmp6, ptr @global, align 4
  %tmp45 = icmp slt i32 undef, undef
  br i1 %tmp45, label %bb6, label %bb9

bb8:
  unreachable

bb9:
  %tmp39 = add nuw nsw i32 %tmp2, 4
  %tmp40 = add nuw i64 %tmp, 4
  br label %bb1
}
