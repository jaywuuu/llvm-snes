; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -loop-vectorize -S -mattr=avx512f -instcombine < %s | FileCheck %s

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

define i32 @inv_load_conditional(i32* %a, i64 %n, i32* %b, i32 %k) {
; CHECK-LABEL: @inv_load_conditional(
; CHECK-NEXT:  iter.check:
; CHECK-NEXT:    [[NTRUNC:%.*]] = trunc i64 [[N:%.*]] to i32
; CHECK-NEXT:    [[SMAX6:%.*]] = call i64 @llvm.smax.i64(i64 [[N]], i64 1)
; CHECK-NEXT:    [[MIN_ITERS_CHECK:%.*]] = icmp ult i64 [[SMAX6]], 8
; CHECK-NEXT:    br i1 [[MIN_ITERS_CHECK]], label [[VEC_EPILOG_SCALAR_PH:%.*]], label [[VECTOR_MEMCHECK:%.*]]
; CHECK:       vector.memcheck:
; CHECK-NEXT:    [[SMAX:%.*]] = call i64 @llvm.smax.i64(i64 [[N]], i64 1)
; CHECK-NEXT:    [[SCEVGEP:%.*]] = getelementptr i32, i32* [[B:%.*]], i64 [[SMAX]]
; CHECK-NEXT:    [[SCEVGEP4:%.*]] = getelementptr i32, i32* [[A:%.*]], i64 1
; CHECK-NEXT:    [[BOUND0:%.*]] = icmp ugt i32* [[SCEVGEP4]], [[B]]
; CHECK-NEXT:    [[BOUND1:%.*]] = icmp ugt i32* [[SCEVGEP]], [[A]]
; CHECK-NEXT:    [[FOUND_CONFLICT:%.*]] = and i1 [[BOUND0]], [[BOUND1]]
; CHECK-NEXT:    br i1 [[FOUND_CONFLICT]], label [[VEC_EPILOG_SCALAR_PH]], label [[VECTOR_MAIN_LOOP_ITER_CHECK:%.*]]
; CHECK:       vector.main.loop.iter.check:
; CHECK-NEXT:    [[MIN_ITERS_CHECK7:%.*]] = icmp ult i64 [[SMAX6]], 16
; CHECK-NEXT:    br i1 [[MIN_ITERS_CHECK7]], label [[VEC_EPILOG_PH:%.*]], label [[VECTOR_PH:%.*]]
; CHECK:       vector.ph:
; CHECK-NEXT:    [[N_VEC:%.*]] = and i64 [[SMAX6]], 9223372036854775792
; CHECK-NEXT:    [[BROADCAST_SPLATINSERT:%.*]] = insertelement <16 x i32*> poison, i32* [[A]], i64 0
; CHECK-NEXT:    [[BROADCAST_SPLAT:%.*]] = shufflevector <16 x i32*> [[BROADCAST_SPLATINSERT]], <16 x i32*> poison, <16 x i32> zeroinitializer
; CHECK-NEXT:    [[BROADCAST_SPLATINSERT8:%.*]] = insertelement <16 x i32> poison, i32 [[NTRUNC]], i64 0
; CHECK-NEXT:    [[BROADCAST_SPLAT9:%.*]] = shufflevector <16 x i32> [[BROADCAST_SPLATINSERT8]], <16 x i32> poison, <16 x i32> zeroinitializer
; CHECK-NEXT:    br label [[VECTOR_BODY:%.*]]
; CHECK:       vector.body:
; CHECK-NEXT:    [[INDEX:%.*]] = phi i64 [ 0, [[VECTOR_PH]] ], [ [[INDEX_NEXT:%.*]], [[VECTOR_BODY]] ]
; CHECK-NEXT:    [[TMP0:%.*]] = getelementptr inbounds i32, i32* [[B]], i64 [[INDEX]]
; CHECK-NEXT:    [[TMP1:%.*]] = bitcast i32* [[TMP0]] to <16 x i32>*
; CHECK-NEXT:    store <16 x i32> [[BROADCAST_SPLAT9]], <16 x i32>* [[TMP1]], align 4, !alias.scope !0, !noalias !3
; CHECK-NEXT:    [[INDEX_NEXT]] = add nuw i64 [[INDEX]], 16
; CHECK-NEXT:    [[TMP2:%.*]] = icmp eq i64 [[INDEX_NEXT]], [[N_VEC]]
; CHECK-NEXT:    br i1 [[TMP2]], label [[MIDDLE_BLOCK:%.*]], label [[VECTOR_BODY]], !llvm.loop [[LOOP5:![0-9]+]]
; CHECK:       middle.block:
; CHECK-NEXT:    [[TMP3:%.*]] = icmp ne <16 x i32*> [[BROADCAST_SPLAT]], zeroinitializer
; CHECK-NEXT:    [[WIDE_MASKED_GATHER:%.*]] = call <16 x i32> @llvm.masked.gather.v16i32.v16p0i32(<16 x i32*> [[BROADCAST_SPLAT]], i32 4, <16 x i1> [[TMP3]], <16 x i32> undef), !alias.scope !3
; CHECK-NEXT:    [[PREDPHI:%.*]] = select <16 x i1> [[TMP3]], <16 x i32> [[WIDE_MASKED_GATHER]], <16 x i32> <i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 1>
; CHECK-NEXT:    [[TMP4:%.*]] = extractelement <16 x i32> [[PREDPHI]], i64 15
; CHECK-NEXT:    [[CMP_N:%.*]] = icmp eq i64 [[SMAX6]], [[N_VEC]]
; CHECK-NEXT:    br i1 [[CMP_N]], label [[FOR_END:%.*]], label [[VEC_EPILOG_ITER_CHECK:%.*]]
; CHECK:       vec.epilog.iter.check:
; CHECK-NEXT:    [[N_VEC_REMAINING:%.*]] = and i64 [[SMAX6]], 8
; CHECK-NEXT:    [[MIN_EPILOG_ITERS_CHECK_NOT_NOT:%.*]] = icmp eq i64 [[N_VEC_REMAINING]], 0
; CHECK-NEXT:    br i1 [[MIN_EPILOG_ITERS_CHECK_NOT_NOT]], label [[VEC_EPILOG_SCALAR_PH]], label [[VEC_EPILOG_PH]]
; CHECK:       vec.epilog.ph:
; CHECK-NEXT:    [[VEC_EPILOG_RESUME_VAL:%.*]] = phi i64 [ [[N_VEC]], [[VEC_EPILOG_ITER_CHECK]] ], [ 0, [[VECTOR_MAIN_LOOP_ITER_CHECK]] ]
; CHECK-NEXT:    [[N_VEC11:%.*]] = and i64 [[SMAX6]], 9223372036854775800
; CHECK-NEXT:    [[BROADCAST_SPLATINSERT15:%.*]] = insertelement <8 x i32*> poison, i32* [[A]], i64 0
; CHECK-NEXT:    [[BROADCAST_SPLAT16:%.*]] = shufflevector <8 x i32*> [[BROADCAST_SPLATINSERT15]], <8 x i32*> poison, <8 x i32> zeroinitializer
; CHECK-NEXT:    [[BROADCAST_SPLATINSERT17:%.*]] = insertelement <8 x i32> poison, i32 [[NTRUNC]], i64 0
; CHECK-NEXT:    [[BROADCAST_SPLAT18:%.*]] = shufflevector <8 x i32> [[BROADCAST_SPLATINSERT17]], <8 x i32> poison, <8 x i32> zeroinitializer
; CHECK-NEXT:    br label [[VEC_EPILOG_VECTOR_BODY:%.*]]
; CHECK:       vec.epilog.vector.body:
; CHECK-NEXT:    [[OFFSET_IDX:%.*]] = phi i64 [ [[VEC_EPILOG_RESUME_VAL]], [[VEC_EPILOG_PH]] ], [ [[INDEX_NEXT21:%.*]], [[VEC_EPILOG_VECTOR_BODY]] ]
; CHECK-NEXT:    [[TMP5:%.*]] = getelementptr inbounds i32, i32* [[B]], i64 [[OFFSET_IDX]]
; CHECK-NEXT:    [[TMP6:%.*]] = bitcast i32* [[TMP5]] to <8 x i32>*
; CHECK-NEXT:    store <8 x i32> [[BROADCAST_SPLAT18]], <8 x i32>* [[TMP6]], align 4
; CHECK-NEXT:    [[INDEX_NEXT21]] = add nuw i64 [[OFFSET_IDX]], 8
; CHECK-NEXT:    [[TMP7:%.*]] = icmp eq i64 [[INDEX_NEXT21]], [[N_VEC11]]
; CHECK-NEXT:    br i1 [[TMP7]], label [[VEC_EPILOG_MIDDLE_BLOCK:%.*]], label [[VEC_EPILOG_VECTOR_BODY]], !llvm.loop [[LOOP7:![0-9]+]]
; CHECK:       vec.epilog.middle.block:
; CHECK-NEXT:    [[TMP8:%.*]] = icmp ne <8 x i32*> [[BROADCAST_SPLAT16]], zeroinitializer
; CHECK-NEXT:    [[WIDE_MASKED_GATHER19:%.*]] = call <8 x i32> @llvm.masked.gather.v8i32.v8p0i32(<8 x i32*> [[BROADCAST_SPLAT16]], i32 4, <8 x i1> [[TMP8]], <8 x i32> undef)
; CHECK-NEXT:    [[PREDPHI20:%.*]] = select <8 x i1> [[TMP8]], <8 x i32> [[WIDE_MASKED_GATHER19]], <8 x i32> <i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 1>
; CHECK-NEXT:    [[TMP9:%.*]] = extractelement <8 x i32> [[PREDPHI20]], i64 7
; CHECK-NEXT:    [[CMP_N12:%.*]] = icmp eq i64 [[SMAX6]], [[N_VEC11]]
; CHECK-NEXT:    br i1 [[CMP_N12]], label [[FOR_END]], label [[VEC_EPILOG_SCALAR_PH]]
; CHECK:       vec.epilog.scalar.ph:
; CHECK-NEXT:    [[BC_RESUME_VAL:%.*]] = phi i64 [ [[N_VEC11]], [[VEC_EPILOG_MIDDLE_BLOCK]] ], [ [[N_VEC]], [[VEC_EPILOG_ITER_CHECK]] ], [ 0, [[VECTOR_MEMCHECK]] ], [ 0, [[ITER_CHECK:%.*]] ]
; CHECK-NEXT:    br label [[FOR_BODY:%.*]]
; CHECK:       for.body:
; CHECK-NEXT:    [[I:%.*]] = phi i64 [ [[I_NEXT:%.*]], [[LATCH:%.*]] ], [ [[BC_RESUME_VAL]], [[VEC_EPILOG_SCALAR_PH]] ]
; CHECK-NEXT:    [[I1:%.*]] = getelementptr inbounds i32, i32* [[B]], i64 [[I]]
; CHECK-NEXT:    [[CMP_NOT:%.*]] = icmp eq i32* [[A]], null
; CHECK-NEXT:    store i32 [[NTRUNC]], i32* [[I1]], align 4
; CHECK-NEXT:    br i1 [[CMP_NOT]], label [[LATCH]], label [[COND_LOAD:%.*]]
; CHECK:       cond_load:
; CHECK-NEXT:    [[ALOAD:%.*]] = load i32, i32* [[A]], align 4
; CHECK-NEXT:    br label [[LATCH]]
; CHECK:       latch:
; CHECK-NEXT:    [[A_LCSSA:%.*]] = phi i32 [ [[ALOAD]], [[COND_LOAD]] ], [ 1, [[FOR_BODY]] ]
; CHECK-NEXT:    [[I_NEXT]] = add nuw nsw i64 [[I]], 1
; CHECK-NEXT:    [[COND:%.*]] = icmp slt i64 [[I_NEXT]], [[N]]
; CHECK-NEXT:    br i1 [[COND]], label [[FOR_BODY]], label [[FOR_END]], !llvm.loop [[LOOP9:![0-9]+]]
; CHECK:       for.end:
; CHECK-NEXT:    [[A_LCSSA_LCSSA:%.*]] = phi i32 [ [[A_LCSSA]], [[LATCH]] ], [ [[TMP4]], [[MIDDLE_BLOCK]] ], [ [[TMP9]], [[VEC_EPILOG_MIDDLE_BLOCK]] ]
; CHECK-NEXT:    ret i32 [[A_LCSSA_LCSSA]]
;
entry:
  %ntrunc = trunc i64 %n to i32
  br label %for.body

for.body:                                         ; preds = %for.body, %entry
  %i = phi i64 [ %i.next, %latch ], [ 0, %entry ]
  %i1 = getelementptr inbounds i32, i32* %b, i64 %i
  %i2 = load i32, i32* %i1, align 8
  %cmp = icmp ne i32* %a, null
  store i32 %ntrunc, i32* %i1
  br i1 %cmp, label %cond_load, label %latch

cond_load:
  %aload = load i32, i32* %a, align 4
  br label %latch

latch:
  %a.lcssa = phi i32 [ %aload, %cond_load ], [ 1, %for.body ]
  %i.next = add nuw nsw i64 %i, 1
  %cond = icmp slt i64 %i.next, %n
  br i1 %cond, label %for.body, label %for.end

for.end:                                          ; preds = %for.body
  ret i32 %a.lcssa
}
