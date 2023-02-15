// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py UTC_ARGS: --function-signature --include-generated-funcs
// RUN: %clang_cc1 -no-opaque-pointers -fopenmp-enable-irbuilder -verify -fopenmp -fopenmp-version=51 -x c -triple x86_64-unknown-unknown -emit-llvm %s -o - | FileCheck %s
// expected-no-diagnostics

// REQUIRES: x86-registered-target

#ifndef HEADER
#define HEADER

double sind(double);

// CHECK-LABEL: define {{.*}}@unroll_partial_heuristic_for(
// CHECK-NEXT:  [[ENTRY:.*]]:
// CHECK-NEXT:    %[[M_ADDR:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[A_ADDR:.+]] = alloca float*, align 8
// CHECK-NEXT:    %[[B_ADDR:.+]] = alloca float*, align 8
// CHECK-NEXT:    %[[C_ADDR:.+]] = alloca float*, align 8
// CHECK-NEXT:    %[[D_ADDR:.+]] = alloca float*, align 8
// CHECK-NEXT:    %[[E_ADDR:.+]] = alloca float*, align 8
// CHECK-NEXT:    %[[OFFSET_ADDR:.+]] = alloca float, align 4
// CHECK-NEXT:    %[[DOTOMP_IV:.+]] = alloca i64, align 8
// CHECK-NEXT:    %[[TMP:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[TMP1:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTCAPTURE_EXPR_:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[J:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTCAPTURE_EXPR_2:.+]] = alloca i64, align 8
// CHECK-NEXT:    %[[I:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTUNROLLED_IV_J:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTOMP_LB:.+]] = alloca i64, align 8
// CHECK-NEXT:    %[[DOTOMP_UB:.+]] = alloca i64, align 8
// CHECK-NEXT:    %[[DOTOMP_STRIDE:.+]] = alloca i64, align 8
// CHECK-NEXT:    %[[DOTOMP_IS_LAST:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[I6:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTUNROLLED_IV_J7:.+]] = alloca i32, align 4
// CHECK-NEXT:    %[[DOTUNROLL_INNER_IV_J:.+]] = alloca i32, align 4
// CHECK-NEXT:    store i32 %[[M:.+]], i32* %[[M_ADDR]], align 4
// CHECK-NEXT:    store float* %[[A:.+]], float** %[[A_ADDR]], align 8
// CHECK-NEXT:    store float* %[[B:.+]], float** %[[B_ADDR]], align 8
// CHECK-NEXT:    store float* %[[C:.+]], float** %[[C_ADDR]], align 8
// CHECK-NEXT:    store float* %[[D:.+]], float** %[[D_ADDR]], align 8
// CHECK-NEXT:    store float* %[[E:.+]], float** %[[E_ADDR]], align 8
// CHECK-NEXT:    store float %[[OFFSET:.+]], float* %[[OFFSET_ADDR]], align 4
// CHECK-NEXT:    %[[TMP0:.+]] = load i32, i32* %[[M_ADDR]], align 4
// CHECK-NEXT:    store i32 %[[TMP0]], i32* %[[DOTCAPTURE_EXPR_]], align 4
// CHECK-NEXT:    store i32 0, i32* %[[J]], align 4
// CHECK-NEXT:    %[[TMP1_1:.+]] = load i32, i32* %[[DOTCAPTURE_EXPR_]], align 4
// CHECK-NEXT:    %[[SUB:.+]] = sub nsw i32 %[[TMP1_1]], 0
// CHECK-NEXT:    %[[DIV:.+]] = sdiv i32 %[[SUB]], 1
// CHECK-NEXT:    %[[CONV:.+]] = sext i32 %[[DIV]] to i64
// CHECK-NEXT:    %[[MUL:.+]] = mul nsw i64 %[[CONV]], 4
// CHECK-NEXT:    %[[SUB3:.+]] = sub nsw i64 %[[MUL]], 1
// CHECK-NEXT:    store i64 %[[SUB3]], i64* %[[DOTCAPTURE_EXPR_2]], align 8
// CHECK-NEXT:    store i32 0, i32* %[[I]], align 4
// CHECK-NEXT:    store i32 0, i32* %[[DOTUNROLLED_IV_J]], align 4
// CHECK-NEXT:    %[[TMP2:.+]] = load i32, i32* %[[DOTCAPTURE_EXPR_]], align 4
// CHECK-NEXT:    %[[CMP:.+]] = icmp slt i32 0, %[[TMP2]]
// CHECK-NEXT:    br i1 %[[CMP]], label %[[OMP_PRECOND_THEN:.+]], label %[[OMP_PRECOND_END:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_PRECOND_THEN]]:
// CHECK-NEXT:    store i64 0, i64* %[[DOTOMP_LB]], align 8
// CHECK-NEXT:    %[[TMP3:.+]] = load i64, i64* %[[DOTCAPTURE_EXPR_2]], align 8
// CHECK-NEXT:    store i64 %[[TMP3]], i64* %[[DOTOMP_UB]], align 8
// CHECK-NEXT:    store i64 1, i64* %[[DOTOMP_STRIDE]], align 8
// CHECK-NEXT:    store i32 0, i32* %[[DOTOMP_IS_LAST]], align 4
// CHECK-NEXT:    %[[OMP_GLOBAL_THREAD_NUM:.+]] = call i32 @__kmpc_global_thread_num(%struct.ident_t* @3)
// CHECK-NEXT:    call void @__kmpc_for_static_init_8(%struct.ident_t* @1, i32 %[[OMP_GLOBAL_THREAD_NUM]], i32 34, i32* %[[DOTOMP_IS_LAST]], i64* %[[DOTOMP_LB]], i64* %[[DOTOMP_UB]], i64* %[[DOTOMP_STRIDE]], i64 1, i64 1)
// CHECK-NEXT:    %[[TMP4:.+]] = load i64, i64* %[[DOTOMP_UB]], align 8
// CHECK-NEXT:    %[[TMP5:.+]] = load i64, i64* %[[DOTCAPTURE_EXPR_2]], align 8
// CHECK-NEXT:    %[[CMP8:.+]] = icmp sgt i64 %[[TMP4]], %[[TMP5]]
// CHECK-NEXT:    br i1 %[[CMP8]], label %[[COND_TRUE:.+]], label %[[COND_FALSE:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[COND_TRUE]]:
// CHECK-NEXT:    %[[TMP6:.+]] = load i64, i64* %[[DOTCAPTURE_EXPR_2]], align 8
// CHECK-NEXT:    br label %[[COND_END:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[COND_FALSE]]:
// CHECK-NEXT:    %[[TMP7:.+]] = load i64, i64* %[[DOTOMP_UB]], align 8
// CHECK-NEXT:    br label %[[COND_END]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[COND_END]]:
// CHECK-NEXT:    %[[COND:.+]] = phi i64 [ %[[TMP6]], %[[COND_TRUE]] ], [ %[[TMP7]], %[[COND_FALSE]] ]
// CHECK-NEXT:    store i64 %[[COND]], i64* %[[DOTOMP_UB]], align 8
// CHECK-NEXT:    %[[TMP8:.+]] = load i64, i64* %[[DOTOMP_LB]], align 8
// CHECK-NEXT:    store i64 %[[TMP8]], i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    br label %[[OMP_INNER_FOR_COND:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_INNER_FOR_COND]]:
// CHECK-NEXT:    %[[TMP9:.+]] = load i64, i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    %[[TMP10:.+]] = load i64, i64* %[[DOTOMP_UB]], align 8
// CHECK-NEXT:    %[[CMP10:.+]] = icmp sle i64 %[[TMP9]], %[[TMP10]]
// CHECK-NEXT:    br i1 %[[CMP10]], label %[[OMP_INNER_FOR_BODY:.+]], label %[[OMP_INNER_FOR_END:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_INNER_FOR_BODY]]:
// CHECK-NEXT:    %[[TMP11:.+]] = load i64, i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    %[[DIV12:.+]] = sdiv i64 %[[TMP11]], 4
// CHECK-NEXT:    %[[MUL13:.+]] = mul nsw i64 %[[DIV12]], 1
// CHECK-NEXT:    %[[ADD:.+]] = add nsw i64 0, %[[MUL13]]
// CHECK-NEXT:    %[[CONV14:.+]] = trunc i64 %[[ADD]] to i32
// CHECK-NEXT:    store i32 %[[CONV14]], i32* %[[I6]], align 4
// CHECK-NEXT:    %[[TMP12:.+]] = load i64, i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    %[[TMP13:.+]] = load i64, i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    %[[DIV15:.+]] = sdiv i64 %[[TMP13]], 4
// CHECK-NEXT:    %[[MUL16:.+]] = mul nsw i64 %[[DIV15]], 4
// CHECK-NEXT:    %[[SUB17:.+]] = sub nsw i64 %[[TMP12]], %[[MUL16]]
// CHECK-NEXT:    %[[MUL18:.+]] = mul nsw i64 %[[SUB17]], 2
// CHECK-NEXT:    %[[ADD19:.+]] = add nsw i64 0, %[[MUL18]]
// CHECK-NEXT:    %[[CONV20:.+]] = trunc i64 %[[ADD19]] to i32
// CHECK-NEXT:    store i32 %[[CONV20]], i32* %[[DOTUNROLLED_IV_J7]], align 4
// CHECK-NEXT:    %[[TMP14:.+]] = load i32, i32* %[[DOTUNROLLED_IV_J7]], align 4
// CHECK-NEXT:    store i32 %[[TMP14]], i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    br label %[[FOR_COND:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[FOR_COND]]:
// CHECK-NEXT:    %[[TMP15:.+]] = load i32, i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    %[[TMP16:.+]] = load i32, i32* %[[DOTUNROLLED_IV_J7]], align 4
// CHECK-NEXT:    %[[ADD21:.+]] = add nsw i32 %[[TMP16]], 2
// CHECK-NEXT:    %[[CMP22:.+]] = icmp slt i32 %[[TMP15]], %[[ADD21]]
// CHECK-NEXT:    br i1 %[[CMP22]], label %[[LAND_RHS:.+]], label %[[LAND_END:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[LAND_RHS]]:
// CHECK-NEXT:    %[[TMP17:.+]] = load i32, i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    %[[CMP24:.+]] = icmp slt i32 %[[TMP17]], 8
// CHECK-NEXT:    br label %[[LAND_END]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[LAND_END]]:
// CHECK-NEXT:    %[[TMP18:.+]] = phi i1 [ false, %[[FOR_COND]] ], [ %[[CMP24]], %[[LAND_RHS]] ]
// CHECK-NEXT:    br i1 %[[TMP18]], label %[[FOR_BODY:.+]], label %[[FOR_END:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[FOR_BODY]]:
// CHECK-NEXT:    %[[TMP19:.+]] = load i32, i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    %[[MUL26:.+]] = mul nsw i32 %[[TMP19]], 1
// CHECK-NEXT:    %[[ADD27:.+]] = add nsw i32 0, %[[MUL26]]
// CHECK-NEXT:    store i32 %[[ADD27]], i32* %[[J]], align 4
// CHECK-NEXT:    %[[TMP20:.+]] = load float*, float** %[[B_ADDR]], align 8
// CHECK-NEXT:    %[[TMP21:.+]] = load i32, i32* %[[I6]], align 4
// CHECK-NEXT:    %[[IDXPROM:.+]] = sext i32 %[[TMP21]] to i64
// CHECK-NEXT:    %[[ARRAYIDX:.+]] = getelementptr inbounds float, float* %[[TMP20]], i64 %[[IDXPROM]]
// CHECK-NEXT:    %[[TMP22:.+]] = load float, float* %[[ARRAYIDX]], align 4
// CHECK-NEXT:    %[[CONV28:.+]] = fpext float %[[TMP22]] to double
// CHECK-NEXT:    %[[CALL:.+]] = call double @sind(double noundef %[[CONV28]])
// CHECK-NEXT:    %[[TMP23:.+]] = load float*, float** %[[C_ADDR]], align 8
// CHECK-NEXT:    %[[TMP24:.+]] = load i32, i32* %[[I6]], align 4
// CHECK-NEXT:    %[[IDXPROM29:.+]] = sext i32 %[[TMP24]] to i64
// CHECK-NEXT:    %[[ARRAYIDX30:.+]] = getelementptr inbounds float, float* %[[TMP23]], i64 %[[IDXPROM29]]
// CHECK-NEXT:    %[[TMP25:.+]] = load float, float* %[[ARRAYIDX30]], align 4
// CHECK-NEXT:    %[[CONV31:.+]] = fpext float %[[TMP25]] to double
// CHECK-NEXT:    %[[MUL32:.+]] = fmul double %[[CALL]], %[[CONV31]]
// CHECK-NEXT:    %[[TMP26:.+]] = load float*, float** %[[D_ADDR]], align 8
// CHECK-NEXT:    %[[TMP27:.+]] = load i32, i32* %[[I6]], align 4
// CHECK-NEXT:    %[[IDXPROM33:.+]] = sext i32 %[[TMP27]] to i64
// CHECK-NEXT:    %[[ARRAYIDX34:.+]] = getelementptr inbounds float, float* %[[TMP26]], i64 %[[IDXPROM33]]
// CHECK-NEXT:    %[[TMP28:.+]] = load float, float* %[[ARRAYIDX34]], align 4
// CHECK-NEXT:    %[[CONV35:.+]] = fpext float %[[TMP28]] to double
// CHECK-NEXT:    %[[MUL36:.+]] = fmul double %[[MUL32]], %[[CONV35]]
// CHECK-NEXT:    %[[TMP29:.+]] = load float*, float** %[[E_ADDR]], align 8
// CHECK-NEXT:    %[[TMP30:.+]] = load i32, i32* %[[I6]], align 4
// CHECK-NEXT:    %[[IDXPROM37:.+]] = sext i32 %[[TMP30]] to i64
// CHECK-NEXT:    %[[ARRAYIDX38:.+]] = getelementptr inbounds float, float* %[[TMP29]], i64 %[[IDXPROM37]]
// CHECK-NEXT:    %[[TMP31:.+]] = load float, float* %[[ARRAYIDX38]], align 4
// CHECK-NEXT:    %[[CONV39:.+]] = fpext float %[[TMP31]] to double
// CHECK-NEXT:    %[[MUL40:.+]] = fmul double %[[MUL36]], %[[CONV39]]
// CHECK-NEXT:    %[[TMP32:.+]] = load float, float* %[[OFFSET_ADDR]], align 4
// CHECK-NEXT:    %[[CONV41:.+]] = fpext float %[[TMP32]] to double
// CHECK-NEXT:    %[[ADD42:.+]] = fadd double %[[MUL40]], %[[CONV41]]
// CHECK-NEXT:    %[[TMP33:.+]] = load float*, float** %[[A_ADDR]], align 8
// CHECK-NEXT:    %[[TMP34:.+]] = load i32, i32* %[[I6]], align 4
// CHECK-NEXT:    %[[IDXPROM43:.+]] = sext i32 %[[TMP34]] to i64
// CHECK-NEXT:    %[[ARRAYIDX44:.+]] = getelementptr inbounds float, float* %[[TMP33]], i64 %[[IDXPROM43]]
// CHECK-NEXT:    %[[TMP35:.+]] = load float, float* %[[ARRAYIDX44]], align 4
// CHECK-NEXT:    %[[CONV45:.+]] = fpext float %[[TMP35]] to double
// CHECK-NEXT:    %[[ADD46:.+]] = fadd double %[[CONV45]], %[[ADD42]]
// CHECK-NEXT:    %[[CONV47:.+]] = fptrunc double %[[ADD46]] to float
// CHECK-NEXT:    store float %[[CONV47]], float* %[[ARRAYIDX44]], align 4
// CHECK-NEXT:    br label %[[FOR_INC:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[FOR_INC]]:
// CHECK-NEXT:    %[[TMP36:.+]] = load i32, i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    %[[INC:.+]] = add nsw i32 %[[TMP36]], 1
// CHECK-NEXT:    store i32 %[[INC]], i32* %[[DOTUNROLL_INNER_IV_J]], align 4
// CHECK-NEXT:    br label %[[FOR_COND]], !llvm.loop ![[LOOP3:[0-9]+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[FOR_END]]:
// CHECK-NEXT:    br label %[[OMP_BODY_CONTINUE:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_BODY_CONTINUE]]:
// CHECK-NEXT:    br label %[[OMP_INNER_FOR_INC:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_INNER_FOR_INC]]:
// CHECK-NEXT:    %[[TMP37:.+]] = load i64, i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    %[[ADD48:.+]] = add nsw i64 %[[TMP37]], 1
// CHECK-NEXT:    store i64 %[[ADD48]], i64* %[[DOTOMP_IV]], align 8
// CHECK-NEXT:    br label %[[OMP_INNER_FOR_COND]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_INNER_FOR_END]]:
// CHECK-NEXT:    br label %[[OMP_LOOP_EXIT:.+]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_LOOP_EXIT]]:
// CHECK-NEXT:    %[[OMP_GLOBAL_THREAD_NUM49:.+]] = call i32 @__kmpc_global_thread_num(%struct.ident_t* @5)
// CHECK-NEXT:    call void @__kmpc_for_static_fini(%struct.ident_t* @1, i32 %[[OMP_GLOBAL_THREAD_NUM49]])
// CHECK-NEXT:    br label %[[OMP_PRECOND_END]]
// CHECK-EMPTY:
// CHECK-NEXT:  [[OMP_PRECOND_END]]:
// CHECK-NEXT:    %[[OMP_GLOBAL_THREAD_NUM50:.+]] = call i32 @__kmpc_global_thread_num(%struct.ident_t* @7)
// CHECK-NEXT:    call void @__kmpc_barrier(%struct.ident_t* @6, i32 %[[OMP_GLOBAL_THREAD_NUM50]])
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }


void unroll_partial_heuristic_for(int m, float *a, float *b, float *c, float *d, float *e, float offset) {
#pragma omp for collapse(2)
  for (int i = 0; i < m; i++) {
#pragma omp unroll partial
    for (int j = 0; j < 8; j++) {
      a[i] += sind(b[i]) * c[i] * d[i] * e[i] + offset;
    }
  }
}

#endif // HEADER
//

// CHECK: ![[META0:[0-9]+]] = !{i32 1, !"wchar_size", i32 4}
// CHECK: ![[META1:[0-9]+]] = !{i32 7, !"openmp", i32 51}
// CHECK: ![[META2:[0-9]+]] =
// CHECK: ![[LOOP3]] = distinct !{![[LOOP3]], ![[LOOPPROP4:[0-9]+]], ![[LOOPPROP5:[0-9]+]]}
// CHECK: ![[LOOPPROP4]] = !{!"llvm.loop.mustprogress"}
// CHECK: ![[LOOPPROP5]] = !{!"llvm.loop.unroll.count", i32 2}
