; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py
; RUN: opt < %s --data-layout="p:64:64:64:64" -S -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck --check-prefixes=PTR64_IDX64 %s
; RUN: opt < %s --data-layout="p:64:64:64:32" -S -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck --check-prefixes=PTR64_IDX32 %s
; RUN: opt < %s --data-layout="p:16:16:16:16" -S -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck --check-prefixes=PTR16_IDX16 %s
; RUN: opt < %s --data-layout="p:16:16:16:32" -S -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck --check-prefixes=PTR16_IDX32 %s

@global = external hidden global [0 x i8]

declare void @use16(i16)

define hidden i32* @trunc_ptr_to_i64(i8* %arg, i32* %arg10) {
; PTR64_IDX64-LABEL: 'trunc_ptr_to_i64'
; PTR64_IDX64-NEXT:  Classifying expressions for: @trunc_ptr_to_i64
; PTR64_IDX64-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX64-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i64 ptrtoint ([0 x i8]* @global to i64)
; PTR64_IDX64-NEXT:    --> ((ptrtoint [0 x i8]* @global to i64) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX64-NEXT:    --> ((ptrtoint [0 x i8]* @global to i64) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX64-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX64-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX64-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i64
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR64_IDX32-LABEL: 'trunc_ptr_to_i64'
; PTR64_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i64
; PTR64_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i64 ptrtoint ([0 x i8]* @global to i64)
; PTR64_IDX32-NEXT:    --> ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) U: full-set S: full-set Exits: ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX32-NEXT:    --> ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) U: full-set S: full-set Exits: ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i64
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX16-LABEL: 'trunc_ptr_to_i64'
; PTR16_IDX16-NEXT:  Classifying expressions for: @trunc_ptr_to_i64
; PTR16_IDX16-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX16-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i64 ptrtoint ([0 x i8]* @global to i64)
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX16-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX16-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX16-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i64
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX32-LABEL: 'trunc_ptr_to_i64'
; PTR16_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i64
; PTR16_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i64 ptrtoint ([0 x i8]* @global to i64)
; PTR16_IDX32-NEXT:    --> ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) U: [0,131071) S: [0,131071) Exits: ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX32-NEXT:    --> ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) U: [0,131071) S: [0,131071) Exits: ((trunc i64 ptrtoint ([0 x i8]* @global to i64) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i64
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
bb:
  br label %bb11

bb11:                                             ; preds = %bb17, %bb
  %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
  %tmp12 = getelementptr i8, i8* %arg, i64 ptrtoint ([0 x i8]* @global to i64)
  %tmp13 = bitcast i8* %tmp12 to i32*
  %tmp14 = load i32, i32* %tmp13, align 4
  %tmp15 = icmp eq i32 %tmp14, 6
  br i1 %tmp15, label %bb16, label %bb17

bb16:                                             ; preds = %bb11
  ret i32* %arg10

bb17:                                             ; preds = %bb11
  %tmp18 = add i32 %tmp, 2
  br label %bb11
}
define hidden i32* @trunc_ptr_to_i32(i8* %arg, i32* %arg10) {
; PTR64_IDX64-LABEL: 'trunc_ptr_to_i32'
; PTR64_IDX64-NEXT:  Classifying expressions for: @trunc_ptr_to_i32
; PTR64_IDX64-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX64-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i32 ptrtoint ([0 x i8]* @global to i32)
; PTR64_IDX64-NEXT:    --> ((sext i32 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32) to i64) + %arg) U: full-set S: full-set Exits: ((sext i32 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32) to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX64-NEXT:    --> ((sext i32 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32) to i64) + %arg) U: full-set S: full-set Exits: ((sext i32 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32) to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX64-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX64-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX64-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i32
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR64_IDX32-LABEL: 'trunc_ptr_to_i32'
; PTR64_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i32
; PTR64_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i32 ptrtoint ([0 x i8]* @global to i32)
; PTR64_IDX32-NEXT:    --> (ptrtoint ([0 x i8]* @global to i32) + %arg) U: full-set S: full-set Exits: (ptrtoint ([0 x i8]* @global to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX32-NEXT:    --> (ptrtoint ([0 x i8]* @global to i32) + %arg) U: full-set S: full-set Exits: (ptrtoint ([0 x i8]* @global to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i32
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX16-LABEL: 'trunc_ptr_to_i32'
; PTR16_IDX16-NEXT:  Classifying expressions for: @trunc_ptr_to_i32
; PTR16_IDX16-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX16-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i32 ptrtoint ([0 x i8]* @global to i32)
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX16-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX16-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX16-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i32
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX32-LABEL: 'trunc_ptr_to_i32'
; PTR16_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i32
; PTR16_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i32 ptrtoint ([0 x i8]* @global to i32)
; PTR16_IDX32-NEXT:    --> (ptrtoint ([0 x i8]* @global to i32) + %arg) U: [0,131071) S: [0,131071) Exits: (ptrtoint ([0 x i8]* @global to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX32-NEXT:    --> (ptrtoint ([0 x i8]* @global to i32) + %arg) U: [0,131071) S: [0,131071) Exits: (ptrtoint ([0 x i8]* @global to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i32
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
bb:
  br label %bb11

bb11:                                             ; preds = %bb17, %bb
  %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
  %tmp12 = getelementptr i8, i8* %arg, i32 ptrtoint ([0 x i8]* @global to i32)
  %tmp13 = bitcast i8* %tmp12 to i32*
  %tmp14 = load i32, i32* %tmp13, align 4
  %tmp15 = icmp eq i32 %tmp14, 6
  br i1 %tmp15, label %bb16, label %bb17

bb16:                                             ; preds = %bb11
  ret i32* %arg10

bb17:                                             ; preds = %bb11
  %tmp18 = add i32 %tmp, 2
  br label %bb11
}
define hidden i32* @trunc_ptr_to_i128(i8* %arg, i32* %arg10) {
; PTR64_IDX64-LABEL: 'trunc_ptr_to_i128'
; PTR64_IDX64-NEXT:  Classifying expressions for: @trunc_ptr_to_i128
; PTR64_IDX64-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX64-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i128 ptrtoint ([0 x i8]* @global to i128)
; PTR64_IDX64-NEXT:    --> ((ptrtoint [0 x i8]* @global to i64) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX64-NEXT:    --> ((ptrtoint [0 x i8]* @global to i64) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i64) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX64-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX64-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX64-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX64-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i128
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR64_IDX32-LABEL: 'trunc_ptr_to_i128'
; PTR64_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i128
; PTR64_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR64_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i128 ptrtoint ([0 x i8]* @global to i128)
; PTR64_IDX32-NEXT:    --> ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) U: full-set S: full-set Exits: ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR64_IDX32-NEXT:    --> ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) U: full-set S: full-set Exits: ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR64_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR64_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR64_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR64_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i128
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX16-LABEL: 'trunc_ptr_to_i128'
; PTR16_IDX16-NEXT:  Classifying expressions for: @trunc_ptr_to_i128
; PTR16_IDX16-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX16-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i128 ptrtoint ([0 x i8]* @global to i128)
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX16-NEXT:    --> ((ptrtoint [0 x i8]* @global to i16) + %arg) U: full-set S: full-set Exits: ((ptrtoint [0 x i8]* @global to i16) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX16-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX16-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX16-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX16-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i128
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX32-LABEL: 'trunc_ptr_to_i128'
; PTR16_IDX32-NEXT:  Classifying expressions for: @trunc_ptr_to_i128
; PTR16_IDX32-NEXT:    %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
; PTR16_IDX32-NEXT:    --> {0,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:    %tmp12 = getelementptr i8, i8* %arg, i128 ptrtoint ([0 x i8]* @global to i128)
; PTR16_IDX32-NEXT:    --> ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) U: [0,131071) S: [0,131071) Exits: ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp13 = bitcast i8* %tmp12 to i32*
; PTR16_IDX32-NEXT:    --> ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) U: [0,131071) S: [0,131071) Exits: ((trunc i128 ptrtoint ([0 x i8]* @global to i128) to i32) + %arg) LoopDispositions: { %bb11: Invariant }
; PTR16_IDX32-NEXT:    %tmp14 = load i32, i32* %tmp13, align 4
; PTR16_IDX32-NEXT:    --> %tmp14 U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb11: Variant }
; PTR16_IDX32-NEXT:    %tmp18 = add i32 %tmp, 2
; PTR16_IDX32-NEXT:    --> {2,+,2}<%bb11> U: [0,-1) S: [-2147483648,2147483647) Exits: <<Unknown>> LoopDispositions: { %bb11: Computable }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @trunc_ptr_to_i128
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable max backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb11: Unpredictable predicated backedge-taken count.
;
bb:
  br label %bb11

bb11:                                             ; preds = %bb17, %bb
  %tmp = phi i32 [ 0, %bb ], [ %tmp18, %bb17 ]
  %tmp12 = getelementptr i8, i8* %arg, i128 ptrtoint ([0 x i8]* @global to i128)
  %tmp13 = bitcast i8* %tmp12 to i32*
  %tmp14 = load i32, i32* %tmp13, align 4
  %tmp15 = icmp eq i32 %tmp14, 6
  br i1 %tmp15, label %bb16, label %bb17

bb16:                                             ; preds = %bb11
  ret i32* %arg10

bb17:                                             ; preds = %bb11
  %tmp18 = add i32 %tmp, 2
  br label %bb11
}

define void @zext_ptr_to_i32(i32 %arg, i32 %arg6) {
; PTR64_IDX64-LABEL: 'zext_ptr_to_i32'
; PTR64_IDX64-NEXT:  Classifying expressions for: @zext_ptr_to_i32
; PTR64_IDX64-NEXT:    %tmp = sub i32 %arg, ptrtoint ([0 x i8]* @global to i32)
; PTR64_IDX64-NEXT:    --> ((-1 * (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32)) + %arg) U: full-set S: full-set Exits: ((-1 * (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i32)) + %arg) LoopDispositions: { %bb7: Invariant }
; PTR64_IDX64-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR64_IDX64-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @zext_ptr_to_i32
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR64_IDX32-LABEL: 'zext_ptr_to_i32'
; PTR64_IDX32-NEXT:  Classifying expressions for: @zext_ptr_to_i32
; PTR64_IDX32-NEXT:    %tmp = sub i32 %arg, ptrtoint ([0 x i8]* @global to i32)
; PTR64_IDX32-NEXT:    --> ((-1 * ptrtoint ([0 x i8]* @global to i32)) + %arg) U: full-set S: full-set Exits: ((-1 * ptrtoint ([0 x i8]* @global to i32)) + %arg) LoopDispositions: { %bb7: Invariant }
; PTR64_IDX32-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR64_IDX32-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @zext_ptr_to_i32
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX16-LABEL: 'zext_ptr_to_i32'
; PTR16_IDX16-NEXT:  Classifying expressions for: @zext_ptr_to_i32
; PTR16_IDX16-NEXT:    %tmp = sub i32 %arg, ptrtoint ([0 x i8]* @global to i32)
; PTR16_IDX16-NEXT:    --> ((-1 * (zext i16 (ptrtoint [0 x i8]* @global to i16) to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * (zext i16 (ptrtoint [0 x i8]* @global to i16) to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR16_IDX16-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR16_IDX16-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @zext_ptr_to_i32
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX32-LABEL: 'zext_ptr_to_i32'
; PTR16_IDX32-NEXT:  Classifying expressions for: @zext_ptr_to_i32
; PTR16_IDX32-NEXT:    %tmp = sub i32 %arg, ptrtoint ([0 x i8]* @global to i32)
; PTR16_IDX32-NEXT:    --> ((-1 * ptrtoint ([0 x i8]* @global to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * ptrtoint ([0 x i8]* @global to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR16_IDX32-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR16_IDX32-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @zext_ptr_to_i32
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
bb:
  br label %bb7

bb7:                                              ; preds = %bb7, %bb
  %tmp = sub i32 %arg, ptrtoint ([0 x i8]* @global to i32)
  %tmp8 = icmp eq i32 %tmp, %arg6
  %tmp9 = select i1 %tmp8, i16 0, i16 1
  call void @use16(i16 %tmp9)
  br i1 %tmp8, label %bb7, label %bb10

bb10:                                             ; preds = %bb7
  ret void
}

define void @sext_to_i32(i32 %arg, i32 %arg6) {
; PTR64_IDX64-LABEL: 'sext_to_i32'
; PTR64_IDX64-NEXT:  Classifying expressions for: @sext_to_i32
; PTR64_IDX64-NEXT:    %tmp = sub i32 %arg, sext (i16 ptrtoint ([0 x i8]* @global to i16) to i32)
; PTR64_IDX64-NEXT:    --> ((-1 * (sext i16 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i16) to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * (sext i16 (trunc i64 (ptrtoint [0 x i8]* @global to i64) to i16) to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR64_IDX64-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR64_IDX64-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @sext_to_i32
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR64_IDX64-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR64_IDX32-LABEL: 'sext_to_i32'
; PTR64_IDX32-NEXT:  Classifying expressions for: @sext_to_i32
; PTR64_IDX32-NEXT:    %tmp = sub i32 %arg, sext (i16 ptrtoint ([0 x i8]* @global to i16) to i32)
; PTR64_IDX32-NEXT:    --> ((-1 * (sext i16 ptrtoint ([0 x i8]* @global to i16) to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * (sext i16 ptrtoint ([0 x i8]* @global to i16) to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR64_IDX32-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR64_IDX32-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @sext_to_i32
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR64_IDX32-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX16-LABEL: 'sext_to_i32'
; PTR16_IDX16-NEXT:  Classifying expressions for: @sext_to_i32
; PTR16_IDX16-NEXT:    %tmp = sub i32 %arg, sext (i16 ptrtoint ([0 x i8]* @global to i16) to i32)
; PTR16_IDX16-NEXT:    --> ((-1 * (sext i16 (ptrtoint [0 x i8]* @global to i16) to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * (sext i16 (ptrtoint [0 x i8]* @global to i16) to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR16_IDX16-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR16_IDX16-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @sext_to_i32
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR16_IDX16-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
; PTR16_IDX32-LABEL: 'sext_to_i32'
; PTR16_IDX32-NEXT:  Classifying expressions for: @sext_to_i32
; PTR16_IDX32-NEXT:    %tmp = sub i32 %arg, sext (i16 ptrtoint ([0 x i8]* @global to i16) to i32)
; PTR16_IDX32-NEXT:    --> ((-1 * (sext i16 ptrtoint ([0 x i8]* @global to i16) to i32))<nsw> + %arg) U: full-set S: full-set Exits: ((-1 * (sext i16 ptrtoint ([0 x i8]* @global to i16) to i32))<nsw> + %arg) LoopDispositions: { %bb7: Invariant }
; PTR16_IDX32-NEXT:    %tmp9 = select i1 %tmp8, i16 0, i16 1
; PTR16_IDX32-NEXT:    --> %tmp9 U: [0,2) S: [0,2) Exits: <<Unknown>> LoopDispositions: { %bb7: Variant }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @sext_to_i32
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable max backedge-taken count.
; PTR16_IDX32-NEXT:  Loop %bb7: Unpredictable predicated backedge-taken count.
;
bb:
  br label %bb7

bb7:                                              ; preds = %bb7, %bb
  %tmp = sub i32 %arg, sext (i16 ptrtoint ([0 x i8]* @global to i16) to i32)
  %tmp8 = icmp eq i32 %tmp, %arg6
  %tmp9 = select i1 %tmp8, i16 0, i16 1
  call void @use16(i16 %tmp9)
  br i1 %tmp8, label %bb7, label %bb10

bb10:                                             ; preds = %bb7
  ret void
}

define i64 @sext_like_noop(i32 %n) {
; PTR64_IDX64-LABEL: 'sext_like_noop'
; PTR64_IDX64-NEXT:  Classifying expressions for: @sext_like_noop
; PTR64_IDX64-NEXT:    %ii = sext i32 %i to i64
; PTR64_IDX64-NEXT:    --> (sext i32 {1,+,1}<nuw><%for.body> to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648) --> (sext i32 (-1 + (trunc i64 (ptrtoint i64 (i32)* @sext_like_noop to i64) to i32)) to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648)
; PTR64_IDX64-NEXT:    %div = sdiv i64 55555, %ii
; PTR64_IDX64-NEXT:    --> %div U: full-set S: full-set
; PTR64_IDX64-NEXT:    %i = phi i32 [ %inc, %for.body ], [ 1, %entry ]
; PTR64_IDX64-NEXT:    --> {1,+,1}<nuw><%for.body> U: [1,0) S: [1,0) Exits: (-1 + (trunc i64 (ptrtoint i64 (i32)* @sext_like_noop to i64) to i32)) LoopDispositions: { %for.body: Computable }
; PTR64_IDX64-NEXT:    %inc = add nuw i32 %i, 1
; PTR64_IDX64-NEXT:    --> {2,+,1}<nuw><%for.body> U: [2,0) S: [2,0) Exits: (trunc i64 (ptrtoint i64 (i32)* @sext_like_noop to i64) to i32) LoopDispositions: { %for.body: Computable }
; PTR64_IDX64-NEXT:  Determining loop execution counts for: @sext_like_noop
; PTR64_IDX64-NEXT:  Loop %for.body: backedge-taken count is (-2 + (trunc i64 (ptrtoint i64 (i32)* @sext_like_noop to i64) to i32))
; PTR64_IDX64-NEXT:  Loop %for.body: max backedge-taken count is -1
; PTR64_IDX64-NEXT:  Loop %for.body: Predicated backedge-taken count is (-2 + (trunc i64 (ptrtoint i64 (i32)* @sext_like_noop to i64) to i32))
; PTR64_IDX64-NEXT:   Predicates:
; PTR64_IDX64:       Loop %for.body: Trip multiple is 1
;
; PTR64_IDX32-LABEL: 'sext_like_noop'
; PTR64_IDX32-NEXT:  Classifying expressions for: @sext_like_noop
; PTR64_IDX32-NEXT:    %ii = sext i32 %i to i64
; PTR64_IDX32-NEXT:    --> (sext i32 {1,+,1}<nuw><%for.body> to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648) --> (sext i32 (-1 + ptrtoint (i64 (i32)* @sext_like_noop to i32)) to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648)
; PTR64_IDX32-NEXT:    %div = sdiv i64 55555, %ii
; PTR64_IDX32-NEXT:    --> %div U: full-set S: full-set
; PTR64_IDX32-NEXT:    %i = phi i32 [ %inc, %for.body ], [ 1, %entry ]
; PTR64_IDX32-NEXT:    --> {1,+,1}<nuw><%for.body> U: [1,0) S: [1,0) Exits: (-1 + ptrtoint (i64 (i32)* @sext_like_noop to i32)) LoopDispositions: { %for.body: Computable }
; PTR64_IDX32-NEXT:    %inc = add nuw i32 %i, 1
; PTR64_IDX32-NEXT:    --> {2,+,1}<nuw><%for.body> U: [2,0) S: [2,0) Exits: ptrtoint (i64 (i32)* @sext_like_noop to i32) LoopDispositions: { %for.body: Computable }
; PTR64_IDX32-NEXT:  Determining loop execution counts for: @sext_like_noop
; PTR64_IDX32-NEXT:  Loop %for.body: backedge-taken count is (-2 + ptrtoint (i64 (i32)* @sext_like_noop to i32))
; PTR64_IDX32-NEXT:  Loop %for.body: max backedge-taken count is -1
; PTR64_IDX32-NEXT:  Loop %for.body: Predicated backedge-taken count is (-2 + ptrtoint (i64 (i32)* @sext_like_noop to i32))
; PTR64_IDX32-NEXT:   Predicates:
; PTR64_IDX32:       Loop %for.body: Trip multiple is 1
;
; PTR16_IDX16-LABEL: 'sext_like_noop'
; PTR16_IDX16-NEXT:  Classifying expressions for: @sext_like_noop
; PTR16_IDX16-NEXT:    %ii = sext i32 %i to i64
; PTR16_IDX16-NEXT:    --> (sext i32 {1,+,1}<nuw><%for.body> to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648) --> (-1 + (zext i16 (ptrtoint i64 (i32)* @sext_like_noop to i16) to i64))<nsw> U: [-1,65535) S: [-1,65535)
; PTR16_IDX16-NEXT:    %div = sdiv i64 55555, %ii
; PTR16_IDX16-NEXT:    --> %div U: full-set S: full-set
; PTR16_IDX16-NEXT:    %i = phi i32 [ %inc, %for.body ], [ 1, %entry ]
; PTR16_IDX16-NEXT:    --> {1,+,1}<nuw><%for.body> U: [1,0) S: [1,0) Exits: (-1 + (zext i16 (ptrtoint i64 (i32)* @sext_like_noop to i16) to i32))<nsw> LoopDispositions: { %for.body: Computable }
; PTR16_IDX16-NEXT:    %inc = add nuw i32 %i, 1
; PTR16_IDX16-NEXT:    --> {2,+,1}<nuw><%for.body> U: [2,0) S: [2,0) Exits: (zext i16 (ptrtoint i64 (i32)* @sext_like_noop to i16) to i32) LoopDispositions: { %for.body: Computable }
; PTR16_IDX16-NEXT:  Determining loop execution counts for: @sext_like_noop
; PTR16_IDX16-NEXT:  Loop %for.body: backedge-taken count is (-2 + (zext i16 (ptrtoint i64 (i32)* @sext_like_noop to i16) to i32))<nsw>
; PTR16_IDX16-NEXT:  Loop %for.body: max backedge-taken count is -1
; PTR16_IDX16-NEXT:  Loop %for.body: Predicated backedge-taken count is (-2 + (zext i16 (ptrtoint i64 (i32)* @sext_like_noop to i16) to i32))<nsw>
; PTR16_IDX16-NEXT:   Predicates:
; PTR16_IDX16:       Loop %for.body: Trip multiple is 1
;
; PTR16_IDX32-LABEL: 'sext_like_noop'
; PTR16_IDX32-NEXT:  Classifying expressions for: @sext_like_noop
; PTR16_IDX32-NEXT:    %ii = sext i32 %i to i64
; PTR16_IDX32-NEXT:    --> (sext i32 {1,+,1}<nuw><%for.body> to i64) U: [-2147483648,2147483648) S: [-2147483648,2147483648) --> (-1 + (zext i32 ptrtoint (i64 (i32)* @sext_like_noop to i32) to i64))<nsw> U: [-1,65535) S: [-1,65535)
; PTR16_IDX32-NEXT:    %div = sdiv i64 55555, %ii
; PTR16_IDX32-NEXT:    --> %div U: full-set S: full-set
; PTR16_IDX32-NEXT:    %i = phi i32 [ %inc, %for.body ], [ 1, %entry ]
; PTR16_IDX32-NEXT:    --> {1,+,1}<nuw><%for.body> U: [1,0) S: [1,0) Exits: (-1 + ptrtoint (i64 (i32)* @sext_like_noop to i32))<nsw> LoopDispositions: { %for.body: Computable }
; PTR16_IDX32-NEXT:    %inc = add nuw i32 %i, 1
; PTR16_IDX32-NEXT:    --> {2,+,1}<nuw><%for.body> U: [2,0) S: [2,0) Exits: ptrtoint (i64 (i32)* @sext_like_noop to i32) LoopDispositions: { %for.body: Computable }
; PTR16_IDX32-NEXT:  Determining loop execution counts for: @sext_like_noop
; PTR16_IDX32-NEXT:  Loop %for.body: backedge-taken count is (-2 + ptrtoint (i64 (i32)* @sext_like_noop to i32))<nsw>
; PTR16_IDX32-NEXT:  Loop %for.body: max backedge-taken count is -1
; PTR16_IDX32-NEXT:  Loop %for.body: Predicated backedge-taken count is (-2 + ptrtoint (i64 (i32)* @sext_like_noop to i32))<nsw>
; PTR16_IDX32-NEXT:   Predicates:
; PTR16_IDX32:       Loop %for.body: Trip multiple is 1
;
entry:
  %cmp6 = icmp sgt i32 %n, 1
  br label %for.body

for.cond.cleanup:
  %ii = sext i32 %i to i64
  %div = sdiv i64 55555, %ii
  ret i64 %div

for.body:
  %i = phi i32 [ %inc, %for.body ], [ 1, %entry ]
  %inc = add nuw i32 %i, 1
  %exitcond = icmp eq i32 %inc, ptrtoint (i64 (i32)* @sext_like_noop to i32)
  br i1 %exitcond, label %for.cond.cleanup, label %for.body
}
declare void @f(i64)
