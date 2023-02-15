; NOTE: Assertions have been autogenerated by utils/update_analyze_test_checks.py
; RUN: opt < %s -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck %s

; The addrecs in this loop are analyzable only by using nsw information.

target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64"

define void @test1(double* %p) nounwind {
; CHECK-LABEL: 'test1'
; CHECK-NEXT:  Classifying expressions for: @test1
; CHECK-NEXT:    %i.01 = phi i32 [ %tmp8, %bb1 ], [ 0, %bb.nph ]
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%bb> U: [0,-2147483648) S: [0,-2147483648) Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp2 = sext i32 %i.01 to i64
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%bb> U: [0,-9223372036854775808) S: [0,-9223372036854775808) Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp3 = getelementptr double, double* %p, i64 %tmp2
; CHECK-NEXT:    --> {%p,+,8}<%bb> U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp6 = sext i32 %i.01 to i64
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%bb> U: [0,-9223372036854775808) S: [0,-9223372036854775808) Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp7 = getelementptr double, double* %p, i64 %tmp6
; CHECK-NEXT:    --> {%p,+,8}<%bb> U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp8 = add nsw i32 %i.01, 1
; CHECK-NEXT:    --> {1,+,1}<nuw><nsw><%bb> U: [1,-2147483648) S: [1,-2147483648) Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %p.gep = getelementptr double, double* %p, i32 %tmp8
; CHECK-NEXT:    --> {(8 + %p),+,8}<%bb> U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %phitmp = sext i32 %tmp8 to i64
; CHECK-NEXT:    --> {1,+,1}<nuw><nsw><%bb> U: [1,-9223372036854775808) S: [1,-9223372036854775808) Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:    %tmp9 = getelementptr inbounds double, double* %p, i64 %phitmp
; CHECK-NEXT:    --> {(8 + %p),+,8}<%bb> U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %bb: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @test1
; CHECK-NEXT:  Loop %bb: Unpredictable backedge-taken count.
; CHECK-NEXT:  Loop %bb: Unpredictable max backedge-taken count.
; CHECK-NEXT:  Loop %bb: Unpredictable predicated backedge-taken count.
;
entry:
  %tmp = load double, double* %p, align 8		; <double> [#uses=1]
  %tmp1 = fcmp ogt double %tmp, 2.000000e+00		; <i1> [#uses=1]
  br i1 %tmp1, label %bb.nph, label %return

bb.nph:		; preds = %entry
  br label %bb

bb:		; preds = %bb1, %bb.nph
  %i.01 = phi i32 [ %tmp8, %bb1 ], [ 0, %bb.nph ]		; <i32> [#uses=3]
  %tmp2 = sext i32 %i.01 to i64		; <i64> [#uses=1]
  %tmp3 = getelementptr double, double* %p, i64 %tmp2		; <double*> [#uses=1]
  %tmp4 = load double, double* %tmp3, align 8		; <double> [#uses=1]
  %tmp5 = fmul double %tmp4, 9.200000e+00		; <double> [#uses=1]
  %tmp6 = sext i32 %i.01 to i64		; <i64> [#uses=1]
  %tmp7 = getelementptr double, double* %p, i64 %tmp6		; <double*> [#uses=1]
  store double %tmp5, double* %tmp7, align 8
  %tmp8 = add nsw i32 %i.01, 1		; <i32> [#uses=2]
  %p.gep = getelementptr double, double* %p, i32 %tmp8
  %p.val = load double, double* %p.gep
  br label %bb1

bb1:		; preds = %bb
  %phitmp = sext i32 %tmp8 to i64		; <i64> [#uses=1]
  %tmp9 = getelementptr inbounds double, double* %p, i64 %phitmp		; <double*> [#uses=1]
  %tmp10 = load double, double* %tmp9, align 8		; <double> [#uses=1]
  %tmp11 = fcmp ogt double %tmp10, 2.000000e+00		; <i1> [#uses=1]
  br i1 %tmp11, label %bb, label %bb1.return_crit_edge

bb1.return_crit_edge:		; preds = %bb1
  br label %return

return:		; preds = %bb1.return_crit_edge, %entry
  ret void
}

define void @test2(i32* %begin, i32* %end) ssp {
; CHECK-LABEL: 'test2'
; CHECK-NEXT:  Classifying expressions for: @test2
; CHECK-NEXT:    %__first.addr.02.i.i = phi i32* [ %begin, %for.body.lr.ph.i.i ], [ %ptrincdec.i.i, %for.body.i.i ]
; CHECK-NEXT:    --> {%begin,+,4}<nuw><%for.body.i.i> U: full-set S: full-set Exits: ((4 * ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4))<nuw> + %begin) LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:    %ptrincdec.i.i = getelementptr inbounds i32, i32* %__first.addr.02.i.i, i64 1
; CHECK-NEXT:    --> {(4 + %begin),+,4}<nuw><%for.body.i.i> U: full-set S: full-set Exits: (4 + (4 * ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4))<nuw> + %begin) LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @test2
; CHECK-NEXT:  Loop %for.body.i.i: backedge-taken count is ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4)
; CHECK-NEXT:  Loop %for.body.i.i: max backedge-taken count is 4611686018427387903
; CHECK-NEXT:  Loop %for.body.i.i: Predicated backedge-taken count is ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4)
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %for.body.i.i: Trip multiple is 1
;
entry:
  %cmp1.i.i = icmp eq i32* %begin, %end
  br i1 %cmp1.i.i, label %_ZSt4fillIPiiEvT_S1_RKT0_.exit, label %for.body.lr.ph.i.i

for.body.lr.ph.i.i:                               ; preds = %entry
  br label %for.body.i.i

for.body.i.i:                                     ; preds = %for.body.i.i, %for.body.lr.ph.i.i
  %__first.addr.02.i.i = phi i32* [ %begin, %for.body.lr.ph.i.i ], [ %ptrincdec.i.i, %for.body.i.i ]
  store i32 0, i32* %__first.addr.02.i.i, align 4
  %ptrincdec.i.i = getelementptr inbounds i32, i32* %__first.addr.02.i.i, i64 1
  %cmp.i.i = icmp eq i32* %ptrincdec.i.i, %end
  br i1 %cmp.i.i, label %for.cond.for.end_crit_edge.i.i, label %for.body.i.i

for.cond.for.end_crit_edge.i.i:                   ; preds = %for.body.i.i
  br label %_ZSt4fillIPiiEvT_S1_RKT0_.exit

_ZSt4fillIPiiEvT_S1_RKT0_.exit:                   ; preds = %entry, %for.cond.for.end_crit_edge.i.i
  ret void
}

; Various checks for inbounds geps.
define void @test3(i32* %begin, i32* %end) nounwind ssp {
; CHECK-LABEL: 'test3'
; CHECK-NEXT:  Classifying expressions for: @test3
; CHECK-NEXT:    %indvar.i.i = phi i64 [ %tmp, %for.body.i.i ], [ 0, %entry ]
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%for.body.i.i> U: [0,4611686018427387904) S: [0,4611686018427387904) Exits: ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4) LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:    %tmp = add nsw i64 %indvar.i.i, 1
; CHECK-NEXT:    --> {1,+,1}<nuw><nsw><%for.body.i.i> U: [1,4611686018427387905) S: [1,4611686018427387905) Exits: (1 + ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4))<nuw><nsw> LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:    %ptrincdec.i.i = getelementptr inbounds i32, i32* %begin, i64 %tmp
; CHECK-NEXT:    --> {(4 + %begin),+,4}<nuw><%for.body.i.i> U: full-set S: full-set Exits: (4 + (4 * ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4))<nuw> + %begin) LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:    %__first.addr.08.i.i = getelementptr inbounds i32, i32* %begin, i64 %indvar.i.i
; CHECK-NEXT:    --> {%begin,+,4}<nuw><%for.body.i.i> U: full-set S: full-set Exits: ((4 * ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4))<nuw> + %begin) LoopDispositions: { %for.body.i.i: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @test3
; CHECK-NEXT:  Loop %for.body.i.i: backedge-taken count is ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4)
; CHECK-NEXT:  Loop %for.body.i.i: max backedge-taken count is 4611686018427387903
; CHECK-NEXT:  Loop %for.body.i.i: Predicated backedge-taken count is ((-4 + (-1 * (ptrtoint i32* %begin to i64)) + (ptrtoint i32* %end to i64)) /u 4)
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %for.body.i.i: Trip multiple is 1
;
entry:
  %cmp7.i.i = icmp eq i32* %begin, %end
  br i1 %cmp7.i.i, label %_ZSt4fillIPiiEvT_S1_RKT0_.exit, label %for.body.i.i

for.body.i.i:                                     ; preds = %entry, %for.body.i.i
  %indvar.i.i = phi i64 [ %tmp, %for.body.i.i ], [ 0, %entry ]
  %tmp = add nsw i64 %indvar.i.i, 1
  %ptrincdec.i.i = getelementptr inbounds i32, i32* %begin, i64 %tmp
  %__first.addr.08.i.i = getelementptr inbounds i32, i32* %begin, i64 %indvar.i.i
  store i32 0, i32* %__first.addr.08.i.i, align 4
  %cmp.i.i = icmp eq i32* %ptrincdec.i.i, %end
  br i1 %cmp.i.i, label %_ZSt4fillIPiiEvT_S1_RKT0_.exit, label %for.body.i.i
_ZSt4fillIPiiEvT_S1_RKT0_.exit:                   ; preds = %for.body.i.i, %entry
  ret void
}

; A single AddExpr exists for (%a + %b), which is not always <nsw>.
define i32 @addnsw(i32 %a, i32 %b) nounwind ssp {
; CHECK-LABEL: 'addnsw'
; CHECK-NEXT:  Classifying expressions for: @addnsw
; CHECK-NEXT:    %tmp = add i32 %a, %b
; CHECK-NEXT:    --> (%a + %b) U: full-set S: full-set
; CHECK-NEXT:    %tmp2 = add nsw i32 %a, %b
; CHECK-NEXT:    --> (%a + %b) U: full-set S: full-set
; CHECK-NEXT:    %result = phi i32 [ %a, %entry ], [ %tmp2, %greater ]
; CHECK-NEXT:    --> %result U: full-set S: full-set
; CHECK-NEXT:  Determining loop execution counts for: @addnsw
;
entry:
  %tmp = add i32 %a, %b
  %cmp = icmp sgt i32 %tmp, 0
  br i1 %cmp, label %greater, label %exit

greater:
  %tmp2 = add nsw i32 %a, %b
  br label %exit

exit:
  %result = phi i32 [ %a, %entry ], [ %tmp2, %greater ]
  ret i32 %result
}

define i32 @PR12375(i32* readnone %arg) {
; CHECK-LABEL: 'PR12375'
; CHECK-NEXT:  Classifying expressions for: @PR12375
; CHECK-NEXT:    %tmp = getelementptr inbounds i32, i32* %arg, i64 2
; CHECK-NEXT:    --> (8 + %arg)<nuw> U: [8,0) S: [8,0)
; CHECK-NEXT:    %tmp2 = phi i32* [ %arg, %bb ], [ %tmp5, %bb1 ]
; CHECK-NEXT:    --> {%arg,+,4}<nuw><%bb1> U: full-set S: full-set Exits: (4 + %arg)<nuw> LoopDispositions: { %bb1: Computable }
; CHECK-NEXT:    %tmp3 = phi i32 [ 0, %bb ], [ %tmp4, %bb1 ]
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%bb1> U: [0,-2147483648) S: [0,-2147483648) Exits: 1 LoopDispositions: { %bb1: Computable }
; CHECK-NEXT:    %tmp4 = add nsw i32 %tmp3, 1
; CHECK-NEXT:    --> {1,+,1}<nuw><%bb1> U: [1,0) S: [1,0) Exits: 2 LoopDispositions: { %bb1: Computable }
; CHECK-NEXT:    %tmp5 = getelementptr inbounds i32, i32* %tmp2, i64 1
; CHECK-NEXT:    --> {(4 + %arg)<nuw>,+,4}<nuw><%bb1> U: [4,0) S: [4,0) Exits: (8 + %arg)<nuw> LoopDispositions: { %bb1: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @PR12375
; CHECK-NEXT:  Loop %bb1: backedge-taken count is 1
; CHECK-NEXT:  Loop %bb1: max backedge-taken count is 1
; CHECK-NEXT:  Loop %bb1: Predicated backedge-taken count is 1
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %bb1: Trip multiple is 2
;
bb:
  %tmp = getelementptr inbounds i32, i32* %arg, i64 2
  br label %bb1

bb1:                                              ; preds = %bb1, %bb
  %tmp2 = phi i32* [ %arg, %bb ], [ %tmp5, %bb1 ]
  %tmp3 = phi i32 [ 0, %bb ], [ %tmp4, %bb1 ]
  %tmp4 = add nsw i32 %tmp3, 1
  %tmp5 = getelementptr inbounds i32, i32* %tmp2, i64 1
  %tmp6 = icmp ult i32* %tmp5, %tmp
  br i1 %tmp6, label %bb1, label %bb7

bb7:                                              ; preds = %bb1
  ret i32 %tmp4
}

define void @PR12376(i32* nocapture %arg, i32* nocapture %arg1)  {
; CHECK-LABEL: 'PR12376'
; CHECK-NEXT:  Classifying expressions for: @PR12376
; CHECK-NEXT:    %tmp = phi i32* [ %arg, %bb ], [ %tmp4, %bb2 ]
; CHECK-NEXT:    --> {%arg,+,4}<nuw><%bb2> U: full-set S: full-set Exits: ((4 * ((-1 + (-1 * (ptrtoint i32* %arg to i64)) + ((4 + (ptrtoint i32* %arg to i64))<nuw> umax (ptrtoint i32* %arg1 to i64))) /u 4))<nuw> + %arg) LoopDispositions: { %bb2: Computable }
; CHECK-NEXT:    %tmp4 = getelementptr inbounds i32, i32* %tmp, i64 1
; CHECK-NEXT:    --> {(4 + %arg)<nuw>,+,4}<nuw><%bb2> U: [4,0) S: [4,0) Exits: (4 + (4 * ((-1 + (-1 * (ptrtoint i32* %arg to i64)) + ((4 + (ptrtoint i32* %arg to i64))<nuw> umax (ptrtoint i32* %arg1 to i64))) /u 4))<nuw> + %arg) LoopDispositions: { %bb2: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @PR12376
; CHECK-NEXT:  Loop %bb2: backedge-taken count is ((-1 + (-1 * (ptrtoint i32* %arg to i64)) + ((4 + (ptrtoint i32* %arg to i64))<nuw> umax (ptrtoint i32* %arg1 to i64))) /u 4)
; CHECK-NEXT:  Loop %bb2: max backedge-taken count is 4611686018427387902
; CHECK-NEXT:  Loop %bb2: Predicated backedge-taken count is ((-1 + (-1 * (ptrtoint i32* %arg to i64)) + ((4 + (ptrtoint i32* %arg to i64))<nuw> umax (ptrtoint i32* %arg1 to i64))) /u 4)
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %bb2: Trip multiple is 1
;
bb:
  br label %bb2

bb2:                                              ; preds = %bb2, %bb
  %tmp = phi i32* [ %arg, %bb ], [ %tmp4, %bb2 ]
  %tmp4 = getelementptr inbounds i32, i32* %tmp, i64 1
  %tmp3 = icmp ult i32* %tmp4, %arg1
  br i1 %tmp3, label %bb2, label %bb5

bb5:                                              ; preds = %bb2
  ret void
}

declare void @f(i32)

define void @nswnowrap(i32 %v, i32* %buf) {
; CHECK-LABEL: 'nswnowrap'
; CHECK-NEXT:  Classifying expressions for: @nswnowrap
; CHECK-NEXT:    %add = add nsw i32 %v, 1
; CHECK-NEXT:    --> (1 + %v) U: full-set S: full-set
; CHECK-NEXT:    %i.04 = phi i32 [ %v, %entry ], [ %inc, %for.body ]
; CHECK-NEXT:    --> {%v,+,1}<nsw><%for.body> U: full-set S: full-set Exits: ((1 + %v) smax %v) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %inc = add nsw i32 %i.04, 1
; CHECK-NEXT:    --> {(1 + %v)<nsw>,+,1}<nsw><%for.body> U: full-set S: full-set Exits: (1 + ((1 + %v)<nsw> smax %v)) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %buf.gep = getelementptr inbounds i32, i32* %buf, i32 %inc
; CHECK-NEXT:    --> {(4 + (4 * (sext i32 %v to i64))<nsw> + %buf),+,4}<nw><%for.body> U: full-set S: full-set Exits: (4 + (4 * (zext i32 ((-1 * %v) + ((1 + %v)<nsw> smax %v)) to i64))<nuw><nsw> + (4 * (sext i32 %v to i64))<nsw> + %buf) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %buf.val = load i32, i32* %buf.gep, align 4
; CHECK-NEXT:    --> %buf.val U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %for.body: Variant }
; CHECK-NEXT:  Determining loop execution counts for: @nswnowrap
; CHECK-NEXT:  Loop %for.body: backedge-taken count is ((-1 * %v) + ((1 + %v)<nsw> smax %v))
; CHECK-NEXT:  Loop %for.body: max backedge-taken count is 1, actual taken count either this or zero.
; CHECK-NEXT:  Loop %for.body: Predicated backedge-taken count is ((-1 * %v) + ((1 + %v)<nsw> smax %v))
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %for.body: Trip multiple is 1
;
entry:
  %add = add nsw i32 %v, 1
  br label %for.body

for.body:
  %i.04 = phi i32 [ %v, %entry ], [ %inc, %for.body ]
  %inc = add nsw i32 %i.04, 1
  %buf.gep = getelementptr inbounds i32, i32* %buf, i32 %inc
  %buf.val = load i32, i32* %buf.gep
  %cmp = icmp slt i32 %i.04, %add
  tail call void @f(i32 %i.04)
  br i1 %cmp, label %for.body, label %for.end

for.end:
  ret void
}

; This test checks if no-wrap flags are propagated when folding {S,+,X}+T ==> {S+T,+,X}
define void @test4(i32 %arg) {
; CHECK-LABEL: 'test4'
; CHECK-NEXT:  Classifying expressions for: @test4
; CHECK-NEXT:    %array = alloca [10 x i32], align 4
; CHECK-NEXT:    --> %array U: [0,-3) S: [-9223372036854775808,9223372036854775805)
; CHECK-NEXT:    %index = phi i32 [ %inc5, %for.body ], [ %arg, %entry ]
; CHECK-NEXT:    --> {%arg,+,1}<nsw><%for.body> U: full-set S: full-set Exits: (-1 + (10 smax (1 + %arg)<nsw>))<nsw> LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %sub = add nsw i32 %index, -2
; CHECK-NEXT:    --> {(-2 + %arg)<nsw>,+,1}<nsw><%for.body> U: full-set S: full-set Exits: (-3 + (10 smax (1 + %arg)<nsw>))<nsw> LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %idxprom = sext i32 %sub to i64
; CHECK-NEXT:    --> {(-2 + (sext i32 %arg to i64))<nsw>,+,1}<nsw><%for.body> U: [-2147483650,4294967304) S: [-2147483650,4294967304) Exits: (-2 + (zext i32 (-1 + (-1 * %arg) + (10 smax (1 + %arg)<nsw>)) to i64) + (sext i32 %arg to i64)) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %arrayidx = getelementptr inbounds [10 x i32], [10 x i32]* %array, i64 0, i64 %idxprom
; CHECK-NEXT:    --> {(-8 + (4 * (sext i32 %arg to i64))<nsw> + %array),+,4}<nw><%for.body> U: [0,-3) S: [-9223372036854775808,9223372036854775805) Exits: (-8 + (4 * (zext i32 (-1 + (-1 * %arg) + (10 smax (1 + %arg)<nsw>)) to i64))<nuw><nsw> + (4 * (sext i32 %arg to i64))<nsw> + %array) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:    %data = load i32, i32* %arrayidx, align 4
; CHECK-NEXT:    --> %data U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %for.body: Variant }
; CHECK-NEXT:    %inc5 = add nsw i32 %index, 1
; CHECK-NEXT:    --> {(1 + %arg)<nsw>,+,1}<nsw><%for.body> U: full-set S: full-set Exits: (10 smax (1 + %arg)<nsw>) LoopDispositions: { %for.body: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @test4
; CHECK-NEXT:  Loop %for.body: backedge-taken count is (-1 + (-1 * %arg) + (10 smax (1 + %arg)<nsw>))
; CHECK-NEXT:  Loop %for.body: max backedge-taken count is -2147483638
; CHECK-NEXT:  Loop %for.body: Predicated backedge-taken count is (-1 + (-1 * %arg) + (10 smax (1 + %arg)<nsw>))
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %for.body: Trip multiple is 1
;
entry:
  %array = alloca [10 x i32], align 4
  br label %for.body

for.body:
  %index = phi i32 [ %inc5, %for.body ], [ %arg, %entry ]
  %sub = add nsw i32 %index, -2
  %idxprom = sext i32 %sub to i64
  %arrayidx = getelementptr inbounds [10 x i32], [10 x i32]* %array, i64 0, i64 %idxprom
  %data = load i32, i32* %arrayidx, align 4
  %inc5 = add nsw i32 %index, 1
  %cmp2 = icmp slt i32 %inc5, 10
  br i1 %cmp2, label %for.body, label %for.end

for.end:
  ret void
}


define void @bad_postinc_nsw_a(i32 %n) {
; CHECK-LABEL: 'bad_postinc_nsw_a'
; CHECK-NEXT:  Classifying expressions for: @bad_postinc_nsw_a
; CHECK-NEXT:    %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
; CHECK-NEXT:    --> {0,+,7}<nuw><nsw><%loop> U: [0,-2147483648) S: [0,-2147483648) Exits: (7 * ((((-1 * (1 umin %n))<nuw><nsw> + %n) /u 7) + (1 umin %n))) LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %iv.inc = add nsw i32 %iv, 7
; CHECK-NEXT:    --> {7,+,7}<nuw><%loop> U: [7,0) S: [7,0) Exits: (7 + (7 * ((((-1 * (1 umin %n))<nuw><nsw> + %n) /u 7) + (1 umin %n)))) LoopDispositions: { %loop: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @bad_postinc_nsw_a
; CHECK-NEXT:  Loop %loop: backedge-taken count is ((((-1 * (1 umin %n))<nuw><nsw> + %n) /u 7) + (1 umin %n))
; CHECK-NEXT:  Loop %loop: max backedge-taken count is 613566756
; CHECK-NEXT:  Loop %loop: Predicated backedge-taken count is ((((-1 * (1 umin %n))<nuw><nsw> + %n) /u 7) + (1 umin %n))
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %loop: Trip multiple is 1
;
entry:
  br label %loop

loop:
  %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
  %iv.inc = add nsw i32 %iv, 7
  %becond = icmp ult i32 %iv, %n
  br i1 %becond, label %loop, label %leave

leave:
  ret void
}

; Unlike @bad_postinc_nsw_a(), the SCEV expression of %iv.inc has <nsw> flag
; because poison can be propagated through 'and %iv.inc, 0'.
define void @postinc_poison_prop_through_and(i32 %n) {
; CHECK-LABEL: 'postinc_poison_prop_through_and'
; CHECK-NEXT:  Classifying expressions for: @postinc_poison_prop_through_and
; CHECK-NEXT:    %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
; CHECK-NEXT:    --> {0,+,7}<nuw><nsw><%loop> U: [0,-2147483648) S: [0,-2147483648) Exits: <<Unknown>> LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %iv.inc = add nsw i32 %iv, 7
; CHECK-NEXT:    --> {7,+,7}<nuw><nsw><%loop> U: [7,-2147483648) S: [7,-2147483648) Exits: <<Unknown>> LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %iv.inc.and = and i32 %iv.inc, 0
; CHECK-NEXT:    --> 0 U: [0,1) S: [0,1) Exits: 0 LoopDispositions: { %loop: Invariant }
; CHECK-NEXT:  Determining loop execution counts for: @postinc_poison_prop_through_and
; CHECK-NEXT:  Loop %loop: Unpredictable backedge-taken count.
; CHECK-NEXT:  Loop %loop: Unpredictable max backedge-taken count.
; CHECK-NEXT:  Loop %loop: Unpredictable predicated backedge-taken count.
;
entry:
  br label %loop

loop:
  %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
  %iv.inc = add nsw i32 %iv, 7
  %iv.inc.and = and i32 %iv.inc, 0
  %becond = icmp ult i32 %iv.inc.and, %n
  br i1 %becond, label %loop, label %leave

leave:
  ret void
}

declare void @may_exit() nounwind

define void @pr28012(i32 %n) {
; CHECK-LABEL: 'pr28012'
; CHECK-NEXT:  Classifying expressions for: @pr28012
; CHECK-NEXT:    %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
; CHECK-NEXT:    --> {0,+,7}<nuw><nsw><%loop> U: [0,-2147483648) S: [0,-2147483648) Exits: (7 * ((-1 + (7 umax %n)) /u 7))<nuw> LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %iv.inc = add nsw i32 %iv, 7
; CHECK-NEXT:    --> {7,+,7}<nuw><%loop> U: [7,-3) S: [7,-3) Exits: (7 + (7 * ((-1 + (7 umax %n)) /u 7))<nuw>) LoopDispositions: { %loop: Computable }
; CHECK-NEXT:  Determining loop execution counts for: @pr28012
; CHECK-NEXT:  Loop %loop: backedge-taken count is ((-1 + (7 umax %n)) /u 7)
; CHECK-NEXT:  Loop %loop: max backedge-taken count is 613566755
; CHECK-NEXT:  Loop %loop: Predicated backedge-taken count is ((-1 + (7 umax %n)) /u 7)
; CHECK-NEXT:   Predicates:
; CHECK:       Loop %loop: Trip multiple is 1
;
entry:
  br label %loop

loop:
  %iv = phi i32 [ 0, %entry ], [ %iv.inc, %loop ]
  %iv.inc = add nsw i32 %iv, 7
  %becond = icmp ult i32 %iv.inc, %n
  call void @may_exit()
  br i1 %becond, label %loop, label %leave

leave:
  ret void
}

define void @select_cond_poison_propagation(double* %p, i32 %x) nounwind {
; CHECK-LABEL: 'select_cond_poison_propagation'
; CHECK-NEXT:  Classifying expressions for: @select_cond_poison_propagation
; CHECK-NEXT:    %iv = phi i32 [ %iv.next, %loop ], [ 0, %entry ]
; CHECK-NEXT:    --> {0,+,1}<nuw><nsw><%loop> U: [0,-2147483648) S: [0,-2147483648) Exits: <<Unknown>> LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %iv.next = add nsw i32 %iv, 1
; CHECK-NEXT:    --> {1,+,1}<nuw><%loop> U: [1,0) S: [1,0) Exits: <<Unknown>> LoopDispositions: { %loop: Computable }
; CHECK-NEXT:    %sel = select i1 %cmp, i32 10, i32 20
; CHECK-NEXT:    --> %sel U: [0,31) S: [0,31) Exits: <<Unknown>> LoopDispositions: { %loop: Variant }
; CHECK-NEXT:    %cond = call i1 @cond()
; CHECK-NEXT:    --> %cond U: full-set S: full-set Exits: <<Unknown>> LoopDispositions: { %loop: Variant }
; CHECK-NEXT:  Determining loop execution counts for: @select_cond_poison_propagation
; CHECK-NEXT:  Loop %loop: Unpredictable backedge-taken count.
; CHECK-NEXT:  Loop %loop: Unpredictable max backedge-taken count.
; CHECK-NEXT:  Loop %loop: Unpredictable predicated backedge-taken count.
;
entry:
  br label %loop

loop:
  %iv = phi i32 [ %iv.next, %loop ], [ 0, %entry ]
  %iv.next = add nsw i32 %iv, 1
  %cmp = icmp ult i32 %iv.next, %x
  %sel = select i1 %cmp, i32 10, i32 20
  call void @foo(i32 noundef %sel)
  %cond = call i1 @cond()
  br i1 %cond, label %loop, label %return

return:
  ret void
}

declare void @foo(i32)

declare i1 @cond()
