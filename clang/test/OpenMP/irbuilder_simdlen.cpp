// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py UTC_ARGS: --check-globals
// RUN: %clang_cc1 -no-opaque-pointers -fopenmp-enable-irbuilder -verify -fopenmp -fopenmp-version=45 -x c++ -triple x86_64-unknown-unknown -emit-llvm %s -o - | FileCheck %s
// expected-no-diagnostics

struct S {
  int a, b;
};

struct P {
  int a, b;
};

// CHECK-LABEL: @_Z6simplePfS_Pi(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[A_ADDR:%.*]] = alloca float*, align 8
// CHECK-NEXT:    [[B_ADDR:%.*]] = alloca float*, align 8
// CHECK-NEXT:    [[C_ADDR:%.*]] = alloca i32*, align 8
// CHECK-NEXT:    [[S:%.*]] = alloca [[STRUCT_S:%.*]], align 4
// CHECK-NEXT:    [[P:%.*]] = alloca %struct.S*, align 8
// CHECK-NEXT:    [[PP:%.*]] = alloca [[STRUCT_P:%.*]], align 4
// CHECK-NEXT:    [[I:%.*]] = alloca i32, align 4
// CHECK-NEXT:    [[AGG_CAPTURED:%.*]] = alloca [[STRUCT_ANON:%.*]], align 8
// CHECK-NEXT:    [[AGG_CAPTURED1:%.*]] = alloca [[STRUCT_ANON_0:%.*]], align 4
// CHECK-NEXT:    [[DOTCOUNT_ADDR:%.*]] = alloca i32, align 4
// CHECK-NEXT:    [[J:%.*]] = alloca i32, align 4
// CHECK-NEXT:    [[AGG_CAPTURED8:%.*]] = alloca [[STRUCT_ANON_1:%.*]], align 8
// CHECK-NEXT:    [[AGG_CAPTURED9:%.*]] = alloca [[STRUCT_ANON_2:%.*]], align 4
// CHECK-NEXT:    [[DOTCOUNT_ADDR10:%.*]] = alloca i32, align 4
// CHECK-NEXT:    store float* [[A:%.*]], float** [[A_ADDR]], align 8
// CHECK-NEXT:    store float* [[B:%.*]], float** [[B_ADDR]], align 8
// CHECK-NEXT:    store i32* [[C:%.*]], i32** [[C_ADDR]], align 8
// CHECK-NEXT:    store i32 3, i32* [[I]], align 4
// CHECK-NEXT:    [[TMP0:%.*]] = getelementptr inbounds [[STRUCT_ANON]], %struct.anon* [[AGG_CAPTURED]], i32 0, i32 0
// CHECK-NEXT:    store i32* [[I]], i32** [[TMP0]], align 8
// CHECK-NEXT:    [[TMP1:%.*]] = getelementptr inbounds [[STRUCT_ANON_0]], %struct.anon.0* [[AGG_CAPTURED1]], i32 0, i32 0
// CHECK-NEXT:    [[TMP2:%.*]] = load i32, i32* [[I]], align 4
// CHECK-NEXT:    store i32 [[TMP2]], i32* [[TMP1]], align 4
// CHECK-NEXT:    call void @__captured_stmt(i32* [[DOTCOUNT_ADDR]], %struct.anon* [[AGG_CAPTURED]])
// CHECK-NEXT:    [[DOTCOUNT:%.*]] = load i32, i32* [[DOTCOUNT_ADDR]], align 4
// CHECK-NEXT:    br label [[OMP_LOOP_PREHEADER:%.*]]
// CHECK:       omp_loop.preheader:
// CHECK-NEXT:    br label [[OMP_LOOP_HEADER:%.*]]
// CHECK:       omp_loop.header:
// CHECK-NEXT:    [[OMP_LOOP_IV:%.*]] = phi i32 [ 0, [[OMP_LOOP_PREHEADER]] ], [ [[OMP_LOOP_NEXT:%.*]], [[OMP_LOOP_INC:%.*]] ]
// CHECK-NEXT:    br label [[OMP_LOOP_COND:%.*]]
// CHECK:       omp_loop.cond:
// CHECK-NEXT:    [[OMP_LOOP_CMP:%.*]] = icmp ult i32 [[OMP_LOOP_IV]], [[DOTCOUNT]]
// CHECK-NEXT:    br i1 [[OMP_LOOP_CMP]], label [[OMP_LOOP_BODY:%.*]], label [[OMP_LOOP_EXIT:%.*]]
// CHECK:       omp_loop.body:
// CHECK-NEXT:    call void @__captured_stmt.1(i32* [[I]], i32 [[OMP_LOOP_IV]], %struct.anon.0* [[AGG_CAPTURED1]]), !llvm.access.group [[ACC_GRP3:![0-9]+]]
// CHECK-NEXT:    [[TMP3:%.*]] = load float*, float** [[B_ADDR]], align 8, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[TMP4:%.*]] = load i32, i32* [[I]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[IDXPROM:%.*]] = sext i32 [[TMP4]] to i64
// CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds float, float* [[TMP3]], i64 [[IDXPROM]]
// CHECK-NEXT:    [[TMP5:%.*]] = load float, float* [[ARRAYIDX]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[A2:%.*]] = getelementptr inbounds [[STRUCT_S]], %struct.S* [[S]], i32 0, i32 0
// CHECK-NEXT:    [[TMP6:%.*]] = load i32, i32* [[A2]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[CONV:%.*]] = sitofp i32 [[TMP6]] to float
// CHECK-NEXT:    [[ADD:%.*]] = fadd float [[TMP5]], [[CONV]]
// CHECK-NEXT:    [[TMP7:%.*]] = load %struct.S*, %struct.S** [[P]], align 8, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[A3:%.*]] = getelementptr inbounds [[STRUCT_S]], %struct.S* [[TMP7]], i32 0, i32 0
// CHECK-NEXT:    [[TMP8:%.*]] = load i32, i32* [[A3]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[CONV4:%.*]] = sitofp i32 [[TMP8]] to float
// CHECK-NEXT:    [[ADD5:%.*]] = fadd float [[ADD]], [[CONV4]]
// CHECK-NEXT:    [[TMP9:%.*]] = load float*, float** [[A_ADDR]], align 8, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[TMP10:%.*]] = load i32, i32* [[I]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    [[IDXPROM6:%.*]] = sext i32 [[TMP10]] to i64
// CHECK-NEXT:    [[ARRAYIDX7:%.*]] = getelementptr inbounds float, float* [[TMP9]], i64 [[IDXPROM6]]
// CHECK-NEXT:    store float [[ADD5]], float* [[ARRAYIDX7]], align 4, !llvm.access.group [[ACC_GRP3]]
// CHECK-NEXT:    br label [[OMP_LOOP_INC]]
// CHECK:       omp_loop.inc:
// CHECK-NEXT:    [[OMP_LOOP_NEXT]] = add nuw i32 [[OMP_LOOP_IV]], 1
// CHECK-NEXT:    br label [[OMP_LOOP_HEADER]], !llvm.loop [[LOOP4:![0-9]+]]
// CHECK:       omp_loop.exit:
// CHECK-NEXT:    br label [[OMP_LOOP_AFTER:%.*]]
// CHECK:       omp_loop.after:
// CHECK-NEXT:    store i32 3, i32* [[J]], align 4
// CHECK-NEXT:    [[TMP11:%.*]] = getelementptr inbounds [[STRUCT_ANON_1]], %struct.anon.1* [[AGG_CAPTURED8]], i32 0, i32 0
// CHECK-NEXT:    store i32* [[J]], i32** [[TMP11]], align 8
// CHECK-NEXT:    [[TMP12:%.*]] = getelementptr inbounds [[STRUCT_ANON_2]], %struct.anon.2* [[AGG_CAPTURED9]], i32 0, i32 0
// CHECK-NEXT:    [[TMP13:%.*]] = load i32, i32* [[J]], align 4
// CHECK-NEXT:    store i32 [[TMP13]], i32* [[TMP12]], align 4
// CHECK-NEXT:    call void @__captured_stmt.2(i32* [[DOTCOUNT_ADDR10]], %struct.anon.1* [[AGG_CAPTURED8]])
// CHECK-NEXT:    [[DOTCOUNT11:%.*]] = load i32, i32* [[DOTCOUNT_ADDR10]], align 4
// CHECK-NEXT:    br label [[OMP_LOOP_PREHEADER12:%.*]]
// CHECK:       omp_loop.preheader12:
// CHECK-NEXT:    br label [[OMP_LOOP_HEADER13:%.*]]
// CHECK:       omp_loop.header13:
// CHECK-NEXT:    [[OMP_LOOP_IV19:%.*]] = phi i32 [ 0, [[OMP_LOOP_PREHEADER12]] ], [ [[OMP_LOOP_NEXT21:%.*]], [[OMP_LOOP_INC16:%.*]] ]
// CHECK-NEXT:    br label [[OMP_LOOP_COND14:%.*]]
// CHECK:       omp_loop.cond14:
// CHECK-NEXT:    [[OMP_LOOP_CMP20:%.*]] = icmp ult i32 [[OMP_LOOP_IV19]], [[DOTCOUNT11]]
// CHECK-NEXT:    br i1 [[OMP_LOOP_CMP20]], label [[OMP_LOOP_BODY15:%.*]], label [[OMP_LOOP_EXIT17:%.*]]
// CHECK:       omp_loop.body15:
// CHECK-NEXT:    call void @__captured_stmt.3(i32* [[J]], i32 [[OMP_LOOP_IV19]], %struct.anon.2* [[AGG_CAPTURED9]]), !llvm.access.group [[ACC_GRP8:![0-9]+]]
// CHECK-NEXT:    [[A22:%.*]] = getelementptr inbounds [[STRUCT_P]], %struct.P* [[PP]], i32 0, i32 0
// CHECK-NEXT:    [[TMP14:%.*]] = load i32, i32* [[A22]], align 4, !llvm.access.group [[ACC_GRP8]]
// CHECK-NEXT:    [[TMP15:%.*]] = load i32*, i32** [[C_ADDR]], align 8, !llvm.access.group [[ACC_GRP8]]
// CHECK-NEXT:    [[TMP16:%.*]] = load i32, i32* [[J]], align 4, !llvm.access.group [[ACC_GRP8]]
// CHECK-NEXT:    [[IDXPROM23:%.*]] = sext i32 [[TMP16]] to i64
// CHECK-NEXT:    [[ARRAYIDX24:%.*]] = getelementptr inbounds i32, i32* [[TMP15]], i64 [[IDXPROM23]]
// CHECK-NEXT:    store i32 [[TMP14]], i32* [[ARRAYIDX24]], align 4, !llvm.access.group [[ACC_GRP8]]
// CHECK-NEXT:    br label [[OMP_LOOP_INC16]]
// CHECK:       omp_loop.inc16:
// CHECK-NEXT:    [[OMP_LOOP_NEXT21]] = add nuw i32 [[OMP_LOOP_IV19]], 1
// CHECK-NEXT:    br label [[OMP_LOOP_HEADER13]], !llvm.loop [[LOOP9:![0-9]+]]
// CHECK:       omp_loop.exit17:
// CHECK-NEXT:    br label [[OMP_LOOP_AFTER18:%.*]]
// CHECK:       omp_loop.after18:
// CHECK-NEXT:    ret void
//
void simple(float *a, float *b, int *c) {
  S s, *p;
  P pp;
#pragma omp simd simdlen(3)
  for (int i = 3; i < 32; i += 5) {
    a[i] = b[i] + s.a + p->a;
  }

#pragma omp simd
  for (int j = 3; j < 32; j += 5) {
    c[j] = pp.a;
  }
}
//.
// CHECK: attributes #0 = { mustprogress noinline nounwind optnone "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+cx8,+mmx,+sse,+sse2,+x87" }
// CHECK: attributes #1 = { noinline nounwind optnone "frame-pointer"="none" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-features"="+cx8,+mmx,+sse,+sse2,+x87" }
//.
// CHECK: !0 = !{i32 1, !"wchar_size", i32 4}
// CHECK: !1 = !{i32 7, !"openmp", i32 45}
// CHECK: !3 = distinct !{}
// CHECK: !4 = distinct !{!4, !5, !6, !7}
// CHECK: !5 = !{!"llvm.loop.parallel_accesses", !3}
// CHECK: !6 = !{!"llvm.loop.vectorize.enable", i1 true}
// CHECK: !7 = !{!"llvm.loop.vectorize.width", i32 3}
// CHECK: !8 = distinct !{}
// CHECK: !9 = distinct !{!9, !10, !6}
// CHECK: !10 = !{!"llvm.loop.parallel_accesses", !8}
//.
