; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc -march=hexagon -O3 -hexagon-instsimplify=0 < %s | FileCheck %s

define void @f0(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f0:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB0_1,#3)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp0: // Block address taken
; CHECK-NEXT:  .LBB0_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 1
  %v2 = icmp slt i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

define void @f1(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f1:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB1_1,#2)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp1: // Block address taken
; CHECK-NEXT:  .LBB1_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 2
  %v2 = icmp slt i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

define void @f2(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f2:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB2_1,#1)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp2: // Block address taken
; CHECK-NEXT:  .LBB2_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 3
  %v2 = icmp slt i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

define void @f3(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f3:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB3_1,#4)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp3: // Block address taken
; CHECK-NEXT:  .LBB3_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 1
  %v2 = icmp sle i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

define void @f4(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f4:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB4_1,#2)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp4: // Block address taken
; CHECK-NEXT:  .LBB4_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 2
  %v2 = icmp sle i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

define void @f5(i8* nocapture %a0, i32 %a1, i32 %a2) #0 {
; CHECK-LABEL: f5:
; CHECK:       // %bb.0: // %b0
; CHECK-NEXT:    {
; CHECK-NEXT:     loop0(.LBB5_1,#2)
; CHECK-NEXT:    }
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .Ltmp5: // Block address taken
; CHECK-NEXT:  .LBB5_1: // %b2
; CHECK-NEXT:    // =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    {
; CHECK-NEXT:     nop
; CHECK-NEXT:     nop
; CHECK-NEXT:    } :endloop0
; CHECK-NEXT:  // %bb.2: // %b3
; CHECK-NEXT:    {
; CHECK-NEXT:     jumpr r31
; CHECK-NEXT:    }
b0:
  br label %b1

b1:                                               ; preds = %b0
  br label %b2

b2:                                               ; preds = %b2, %b1
  %v0 = phi i32 [ 0, %b1 ], [ %v1, %b2 ]
  %v1 = add nsw i32 %v0, 3
  %v2 = icmp sle i32 %v1, 3
  br i1 %v2, label %b2, label %b3

b3:                                               ; preds = %b2
  ret void
}

attributes #0 = { nounwind }
