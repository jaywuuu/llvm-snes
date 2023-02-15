// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py UTC_ARGS: --function-signature --include-generated-funcs --replace-value-regex "__omp_offloading_[0-9a-z]+_[0-9a-z]+" "reduction_size[.].+[.]" "pl_cond[.].+[.|,]" --prefix-filecheck-ir-name _
// Test target codegen - host bc file has to be created first.
// RUN: %clang_cc1 -no-opaque-pointers -verify -fopenmp -x c++ -triple powerpc64le-unknown-unknown -fopenmp-targets=nvptx64-nvidia-cuda -emit-llvm-bc %s -o %t-ppc-host.bc
// RUN: %clang_cc1 -no-opaque-pointers -verify -fopenmp -x c++ -triple nvptx64-unknown-unknown -fopenmp-targets=nvptx64-nvidia-cuda -emit-llvm %s -fopenmp-is-device -fopenmp-host-ir-file-path %t-ppc-host.bc -o - | FileCheck %s --check-prefix=CHECK1
// RUN: %clang_cc1 -no-opaque-pointers -verify -fopenmp -x c++ -triple i386-unknown-unknown -fopenmp-targets=nvptx-nvidia-cuda -emit-llvm-bc %s -o %t-x86-host.bc
// RUN: %clang_cc1 -no-opaque-pointers -verify -fopenmp -x c++ -triple nvptx-unknown-unknown -fopenmp-targets=nvptx-nvidia-cuda -emit-llvm %s -fopenmp-is-device -fopenmp-host-ir-file-path %t-x86-host.bc -o - | FileCheck %s --check-prefix=CHECK2
// RUN: %clang_cc1 -no-opaque-pointers -verify -fopenmp -fexceptions -fcxx-exceptions -x c++ -triple nvptx-unknown-unknown -fopenmp-targets=nvptx-nvidia-cuda -emit-llvm %s -fopenmp-is-device -fopenmp-host-ir-file-path %t-x86-host.bc -o - | FileCheck %s --check-prefix=CHECK2
// expected-no-diagnostics
#ifndef HEADER
#define HEADER

template<typename tx>
tx ftemplate(int n) {
  int i;

  #pragma omp target teams distribute
  for (i = 0; i < 10; ++i)
  {
#pragma omp parallel
    ++i;
  }

  return i;
}

int bar(int n){
  int a = 0;

  a += ftemplate<char>(n);

  return a;
}

#endif
// CHECK1-LABEL: define {{[^@]+}}@{{__omp_offloading_[0-9a-z]+_[0-9a-z]+}}__Z9ftemplateIcET_i_l16
// CHECK1-SAME: () #[[ATTR0:[0-9]+]] {
// CHECK1-NEXT:  entry:
// CHECK1-NEXT:    [[DOTZERO_ADDR:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTTHREADID_TEMP_:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[TMP0:%.*]] = call i32 @__kmpc_target_init(%struct.ident_t* @[[GLOB1:[0-9]+]], i8 1, i1 true, i1 true)
// CHECK1-NEXT:    [[EXEC_USER_CODE:%.*]] = icmp eq i32 [[TMP0]], -1
// CHECK1-NEXT:    br i1 [[EXEC_USER_CODE]], label [[USER_CODE_ENTRY:%.*]], label [[WORKER_EXIT:%.*]]
// CHECK1:       user_code.entry:
// CHECK1-NEXT:    [[TMP1:%.*]] = call i32 @__kmpc_global_thread_num(%struct.ident_t* @[[GLOB1]])
// CHECK1-NEXT:    store i32 0, i32* [[DOTZERO_ADDR]], align 4
// CHECK1-NEXT:    store i32 [[TMP1]], i32* [[DOTTHREADID_TEMP_]], align 4
// CHECK1-NEXT:    call void @__omp_outlined__(i32* [[DOTTHREADID_TEMP_]], i32* [[DOTZERO_ADDR]]) #[[ATTR5:[0-9]+]]
// CHECK1-NEXT:    call void @__kmpc_target_deinit(%struct.ident_t* @[[GLOB1]], i8 1, i1 true)
// CHECK1-NEXT:    ret void
// CHECK1:       worker.exit:
// CHECK1-NEXT:    ret void
//
//
// CHECK1-LABEL: define {{[^@]+}}@__omp_outlined__
// CHECK1-SAME: (i32* noalias noundef [[DOTGLOBAL_TID_:%.*]], i32* noalias noundef [[DOTBOUND_TID_:%.*]]) #[[ATTR1:[0-9]+]] {
// CHECK1-NEXT:  entry:
// CHECK1-NEXT:    [[DOTGLOBAL_TID__ADDR:%.*]] = alloca i32*, align 8
// CHECK1-NEXT:    [[DOTBOUND_TID__ADDR:%.*]] = alloca i32*, align 8
// CHECK1-NEXT:    [[DOTOMP_IV:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[TMP:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTOMP_LB:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTOMP_UB:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTOMP_STRIDE:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTOMP_IS_LAST:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[CAPTURED_VARS_ADDRS:%.*]] = alloca [1 x i8*], align 8
// CHECK1-NEXT:    store i32* [[DOTGLOBAL_TID_]], i32** [[DOTGLOBAL_TID__ADDR]], align 8
// CHECK1-NEXT:    store i32* [[DOTBOUND_TID_]], i32** [[DOTBOUND_TID__ADDR]], align 8
// CHECK1-NEXT:    [[I:%.*]] = call align 8 i8* @__kmpc_alloc_shared(i64 4)
// CHECK1-NEXT:    [[I_ON_STACK:%.*]] = bitcast i8* [[I]] to i32*
// CHECK1-NEXT:    store i32 0, i32* [[DOTOMP_LB]], align 4
// CHECK1-NEXT:    store i32 9, i32* [[DOTOMP_UB]], align 4
// CHECK1-NEXT:    store i32 1, i32* [[DOTOMP_STRIDE]], align 4
// CHECK1-NEXT:    store i32 0, i32* [[DOTOMP_IS_LAST]], align 4
// CHECK1-NEXT:    [[TMP0:%.*]] = load i32*, i32** [[DOTGLOBAL_TID__ADDR]], align 8
// CHECK1-NEXT:    [[TMP1:%.*]] = load i32, i32* [[TMP0]], align 4
// CHECK1-NEXT:    call void @__kmpc_distribute_static_init_4(%struct.ident_t* @[[GLOB2:[0-9]+]], i32 [[TMP1]], i32 92, i32* [[DOTOMP_IS_LAST]], i32* [[DOTOMP_LB]], i32* [[DOTOMP_UB]], i32* [[DOTOMP_STRIDE]], i32 1, i32 1)
// CHECK1-NEXT:    [[TMP2:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK1-NEXT:    [[CMP:%.*]] = icmp sgt i32 [[TMP2]], 9
// CHECK1-NEXT:    br i1 [[CMP]], label [[COND_TRUE:%.*]], label [[COND_FALSE:%.*]]
// CHECK1:       cond.true:
// CHECK1-NEXT:    br label [[COND_END:%.*]]
// CHECK1:       cond.false:
// CHECK1-NEXT:    [[TMP3:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK1-NEXT:    br label [[COND_END]]
// CHECK1:       cond.end:
// CHECK1-NEXT:    [[COND:%.*]] = phi i32 [ 9, [[COND_TRUE]] ], [ [[TMP3]], [[COND_FALSE]] ]
// CHECK1-NEXT:    store i32 [[COND]], i32* [[DOTOMP_UB]], align 4
// CHECK1-NEXT:    [[TMP4:%.*]] = load i32, i32* [[DOTOMP_LB]], align 4
// CHECK1-NEXT:    store i32 [[TMP4]], i32* [[DOTOMP_IV]], align 4
// CHECK1-NEXT:    br label [[OMP_INNER_FOR_COND:%.*]]
// CHECK1:       omp.inner.for.cond:
// CHECK1-NEXT:    [[TMP5:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK1-NEXT:    [[TMP6:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK1-NEXT:    [[CMP1:%.*]] = icmp sle i32 [[TMP5]], [[TMP6]]
// CHECK1-NEXT:    br i1 [[CMP1]], label [[OMP_INNER_FOR_BODY:%.*]], label [[OMP_INNER_FOR_END:%.*]]
// CHECK1:       omp.inner.for.body:
// CHECK1-NEXT:    [[TMP7:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK1-NEXT:    [[MUL:%.*]] = mul nsw i32 [[TMP7]], 1
// CHECK1-NEXT:    [[ADD:%.*]] = add nsw i32 0, [[MUL]]
// CHECK1-NEXT:    store i32 [[ADD]], i32* [[I_ON_STACK]], align 4
// CHECK1-NEXT:    [[TMP8:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[CAPTURED_VARS_ADDRS]], i64 0, i64 0
// CHECK1-NEXT:    [[TMP9:%.*]] = bitcast i32* [[I_ON_STACK]] to i8*
// CHECK1-NEXT:    store i8* [[TMP9]], i8** [[TMP8]], align 8
// CHECK1-NEXT:    [[TMP10:%.*]] = bitcast [1 x i8*]* [[CAPTURED_VARS_ADDRS]] to i8**
// CHECK1-NEXT:    call void @__kmpc_parallel_51(%struct.ident_t* @[[GLOB1]], i32 [[TMP1]], i32 1, i32 -1, i32 -1, i8* bitcast (void (i32*, i32*, i32*)* @__omp_outlined__1 to i8*), i8* bitcast (void (i16, i32)* @__omp_outlined__1_wrapper to i8*), i8** [[TMP10]], i64 1)
// CHECK1-NEXT:    br label [[OMP_BODY_CONTINUE:%.*]]
// CHECK1:       omp.body.continue:
// CHECK1-NEXT:    br label [[OMP_INNER_FOR_INC:%.*]]
// CHECK1:       omp.inner.for.inc:
// CHECK1-NEXT:    [[TMP11:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK1-NEXT:    [[ADD2:%.*]] = add nsw i32 [[TMP11]], 1
// CHECK1-NEXT:    store i32 [[ADD2]], i32* [[DOTOMP_IV]], align 4
// CHECK1-NEXT:    br label [[OMP_INNER_FOR_COND]]
// CHECK1:       omp.inner.for.end:
// CHECK1-NEXT:    br label [[OMP_LOOP_EXIT:%.*]]
// CHECK1:       omp.loop.exit:
// CHECK1-NEXT:    call void @__kmpc_distribute_static_fini(%struct.ident_t* @[[GLOB2]], i32 [[TMP1]])
// CHECK1-NEXT:    call void @__kmpc_free_shared(i8* [[I]], i64 4)
// CHECK1-NEXT:    ret void
//
//
// CHECK1-LABEL: define {{[^@]+}}@__omp_outlined__1
// CHECK1-SAME: (i32* noalias noundef [[DOTGLOBAL_TID_:%.*]], i32* noalias noundef [[DOTBOUND_TID_:%.*]], i32* noundef nonnull align 4 dereferenceable(4) [[I:%.*]]) #[[ATTR1]] {
// CHECK1-NEXT:  entry:
// CHECK1-NEXT:    [[DOTGLOBAL_TID__ADDR:%.*]] = alloca i32*, align 8
// CHECK1-NEXT:    [[DOTBOUND_TID__ADDR:%.*]] = alloca i32*, align 8
// CHECK1-NEXT:    [[I_ADDR:%.*]] = alloca i32*, align 8
// CHECK1-NEXT:    store i32* [[DOTGLOBAL_TID_]], i32** [[DOTGLOBAL_TID__ADDR]], align 8
// CHECK1-NEXT:    store i32* [[DOTBOUND_TID_]], i32** [[DOTBOUND_TID__ADDR]], align 8
// CHECK1-NEXT:    store i32* [[I]], i32** [[I_ADDR]], align 8
// CHECK1-NEXT:    [[TMP0:%.*]] = load i32*, i32** [[I_ADDR]], align 8
// CHECK1-NEXT:    [[TMP1:%.*]] = load i32, i32* [[TMP0]], align 4
// CHECK1-NEXT:    [[INC:%.*]] = add nsw i32 [[TMP1]], 1
// CHECK1-NEXT:    store i32 [[INC]], i32* [[TMP0]], align 4
// CHECK1-NEXT:    ret void
//
//
// CHECK1-LABEL: define {{[^@]+}}@__omp_outlined__1_wrapper
// CHECK1-SAME: (i16 noundef zeroext [[TMP0:%.*]], i32 noundef [[TMP1:%.*]]) #[[ATTR3:[0-9]+]] {
// CHECK1-NEXT:  entry:
// CHECK1-NEXT:    [[DOTADDR:%.*]] = alloca i16, align 2
// CHECK1-NEXT:    [[DOTADDR1:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[DOTZERO_ADDR:%.*]] = alloca i32, align 4
// CHECK1-NEXT:    [[GLOBAL_ARGS:%.*]] = alloca i8**, align 8
// CHECK1-NEXT:    store i16 [[TMP0]], i16* [[DOTADDR]], align 2
// CHECK1-NEXT:    store i32 [[TMP1]], i32* [[DOTADDR1]], align 4
// CHECK1-NEXT:    store i32 0, i32* [[DOTZERO_ADDR]], align 4
// CHECK1-NEXT:    call void @__kmpc_get_shared_variables(i8*** [[GLOBAL_ARGS]])
// CHECK1-NEXT:    [[TMP2:%.*]] = load i8**, i8*** [[GLOBAL_ARGS]], align 8
// CHECK1-NEXT:    [[TMP3:%.*]] = getelementptr inbounds i8*, i8** [[TMP2]], i64 0
// CHECK1-NEXT:    [[TMP4:%.*]] = bitcast i8** [[TMP3]] to i32**
// CHECK1-NEXT:    [[TMP5:%.*]] = load i32*, i32** [[TMP4]], align 8
// CHECK1-NEXT:    call void @__omp_outlined__1(i32* [[DOTADDR1]], i32* [[DOTZERO_ADDR]], i32* [[TMP5]]) #[[ATTR5]]
// CHECK1-NEXT:    ret void
//
//
// CHECK2-LABEL: define {{[^@]+}}@{{__omp_offloading_[0-9a-z]+_[0-9a-z]+}}__Z9ftemplateIcET_i_l16
// CHECK2-SAME: () #[[ATTR0:[0-9]+]] {
// CHECK2-NEXT:  entry:
// CHECK2-NEXT:    [[DOTZERO_ADDR:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTTHREADID_TEMP_:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[TMP0:%.*]] = call i32 @__kmpc_target_init(%struct.ident_t* @[[GLOB1:[0-9]+]], i8 1, i1 true, i1 true)
// CHECK2-NEXT:    [[EXEC_USER_CODE:%.*]] = icmp eq i32 [[TMP0]], -1
// CHECK2-NEXT:    br i1 [[EXEC_USER_CODE]], label [[USER_CODE_ENTRY:%.*]], label [[WORKER_EXIT:%.*]]
// CHECK2:       user_code.entry:
// CHECK2-NEXT:    [[TMP1:%.*]] = call i32 @__kmpc_global_thread_num(%struct.ident_t* @[[GLOB1]])
// CHECK2-NEXT:    store i32 0, i32* [[DOTZERO_ADDR]], align 4
// CHECK2-NEXT:    store i32 [[TMP1]], i32* [[DOTTHREADID_TEMP_]], align 4
// CHECK2-NEXT:    call void @__omp_outlined__(i32* [[DOTTHREADID_TEMP_]], i32* [[DOTZERO_ADDR]]) #[[ATTR5:[0-9]+]]
// CHECK2-NEXT:    call void @__kmpc_target_deinit(%struct.ident_t* @[[GLOB1]], i8 1, i1 true)
// CHECK2-NEXT:    ret void
// CHECK2:       worker.exit:
// CHECK2-NEXT:    ret void
//
//
// CHECK2-LABEL: define {{[^@]+}}@__omp_outlined__
// CHECK2-SAME: (i32* noalias noundef [[DOTGLOBAL_TID_:%.*]], i32* noalias noundef [[DOTBOUND_TID_:%.*]]) #[[ATTR1:[0-9]+]] {
// CHECK2-NEXT:  entry:
// CHECK2-NEXT:    [[DOTGLOBAL_TID__ADDR:%.*]] = alloca i32*, align 4
// CHECK2-NEXT:    [[DOTBOUND_TID__ADDR:%.*]] = alloca i32*, align 4
// CHECK2-NEXT:    [[DOTOMP_IV:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[TMP:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTOMP_LB:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTOMP_UB:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTOMP_STRIDE:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTOMP_IS_LAST:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[CAPTURED_VARS_ADDRS:%.*]] = alloca [1 x i8*], align 4
// CHECK2-NEXT:    store i32* [[DOTGLOBAL_TID_]], i32** [[DOTGLOBAL_TID__ADDR]], align 4
// CHECK2-NEXT:    store i32* [[DOTBOUND_TID_]], i32** [[DOTBOUND_TID__ADDR]], align 4
// CHECK2-NEXT:    [[I:%.*]] = call align 8 i8* @__kmpc_alloc_shared(i32 4)
// CHECK2-NEXT:    [[I_ON_STACK:%.*]] = bitcast i8* [[I]] to i32*
// CHECK2-NEXT:    store i32 0, i32* [[DOTOMP_LB]], align 4
// CHECK2-NEXT:    store i32 9, i32* [[DOTOMP_UB]], align 4
// CHECK2-NEXT:    store i32 1, i32* [[DOTOMP_STRIDE]], align 4
// CHECK2-NEXT:    store i32 0, i32* [[DOTOMP_IS_LAST]], align 4
// CHECK2-NEXT:    [[TMP0:%.*]] = load i32*, i32** [[DOTGLOBAL_TID__ADDR]], align 4
// CHECK2-NEXT:    [[TMP1:%.*]] = load i32, i32* [[TMP0]], align 4
// CHECK2-NEXT:    call void @__kmpc_distribute_static_init_4(%struct.ident_t* @[[GLOB2:[0-9]+]], i32 [[TMP1]], i32 92, i32* [[DOTOMP_IS_LAST]], i32* [[DOTOMP_LB]], i32* [[DOTOMP_UB]], i32* [[DOTOMP_STRIDE]], i32 1, i32 1)
// CHECK2-NEXT:    [[TMP2:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK2-NEXT:    [[CMP:%.*]] = icmp sgt i32 [[TMP2]], 9
// CHECK2-NEXT:    br i1 [[CMP]], label [[COND_TRUE:%.*]], label [[COND_FALSE:%.*]]
// CHECK2:       cond.true:
// CHECK2-NEXT:    br label [[COND_END:%.*]]
// CHECK2:       cond.false:
// CHECK2-NEXT:    [[TMP3:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK2-NEXT:    br label [[COND_END]]
// CHECK2:       cond.end:
// CHECK2-NEXT:    [[COND:%.*]] = phi i32 [ 9, [[COND_TRUE]] ], [ [[TMP3]], [[COND_FALSE]] ]
// CHECK2-NEXT:    store i32 [[COND]], i32* [[DOTOMP_UB]], align 4
// CHECK2-NEXT:    [[TMP4:%.*]] = load i32, i32* [[DOTOMP_LB]], align 4
// CHECK2-NEXT:    store i32 [[TMP4]], i32* [[DOTOMP_IV]], align 4
// CHECK2-NEXT:    br label [[OMP_INNER_FOR_COND:%.*]]
// CHECK2:       omp.inner.for.cond:
// CHECK2-NEXT:    [[TMP5:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK2-NEXT:    [[TMP6:%.*]] = load i32, i32* [[DOTOMP_UB]], align 4
// CHECK2-NEXT:    [[CMP1:%.*]] = icmp sle i32 [[TMP5]], [[TMP6]]
// CHECK2-NEXT:    br i1 [[CMP1]], label [[OMP_INNER_FOR_BODY:%.*]], label [[OMP_INNER_FOR_END:%.*]]
// CHECK2:       omp.inner.for.body:
// CHECK2-NEXT:    [[TMP7:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK2-NEXT:    [[MUL:%.*]] = mul nsw i32 [[TMP7]], 1
// CHECK2-NEXT:    [[ADD:%.*]] = add nsw i32 0, [[MUL]]
// CHECK2-NEXT:    store i32 [[ADD]], i32* [[I_ON_STACK]], align 4
// CHECK2-NEXT:    [[TMP8:%.*]] = getelementptr inbounds [1 x i8*], [1 x i8*]* [[CAPTURED_VARS_ADDRS]], i32 0, i32 0
// CHECK2-NEXT:    [[TMP9:%.*]] = bitcast i32* [[I_ON_STACK]] to i8*
// CHECK2-NEXT:    store i8* [[TMP9]], i8** [[TMP8]], align 4
// CHECK2-NEXT:    [[TMP10:%.*]] = bitcast [1 x i8*]* [[CAPTURED_VARS_ADDRS]] to i8**
// CHECK2-NEXT:    call void @__kmpc_parallel_51(%struct.ident_t* @[[GLOB1]], i32 [[TMP1]], i32 1, i32 -1, i32 -1, i8* bitcast (void (i32*, i32*, i32*)* @__omp_outlined__1 to i8*), i8* bitcast (void (i16, i32)* @__omp_outlined__1_wrapper to i8*), i8** [[TMP10]], i32 1)
// CHECK2-NEXT:    br label [[OMP_BODY_CONTINUE:%.*]]
// CHECK2:       omp.body.continue:
// CHECK2-NEXT:    br label [[OMP_INNER_FOR_INC:%.*]]
// CHECK2:       omp.inner.for.inc:
// CHECK2-NEXT:    [[TMP11:%.*]] = load i32, i32* [[DOTOMP_IV]], align 4
// CHECK2-NEXT:    [[ADD2:%.*]] = add nsw i32 [[TMP11]], 1
// CHECK2-NEXT:    store i32 [[ADD2]], i32* [[DOTOMP_IV]], align 4
// CHECK2-NEXT:    br label [[OMP_INNER_FOR_COND]]
// CHECK2:       omp.inner.for.end:
// CHECK2-NEXT:    br label [[OMP_LOOP_EXIT:%.*]]
// CHECK2:       omp.loop.exit:
// CHECK2-NEXT:    call void @__kmpc_distribute_static_fini(%struct.ident_t* @[[GLOB2]], i32 [[TMP1]])
// CHECK2-NEXT:    call void @__kmpc_free_shared(i8* [[I]], i32 4)
// CHECK2-NEXT:    ret void
//
//
// CHECK2-LABEL: define {{[^@]+}}@__omp_outlined__1
// CHECK2-SAME: (i32* noalias noundef [[DOTGLOBAL_TID_:%.*]], i32* noalias noundef [[DOTBOUND_TID_:%.*]], i32* noundef nonnull align 4 dereferenceable(4) [[I:%.*]]) #[[ATTR1]] {
// CHECK2-NEXT:  entry:
// CHECK2-NEXT:    [[DOTGLOBAL_TID__ADDR:%.*]] = alloca i32*, align 4
// CHECK2-NEXT:    [[DOTBOUND_TID__ADDR:%.*]] = alloca i32*, align 4
// CHECK2-NEXT:    [[I_ADDR:%.*]] = alloca i32*, align 4
// CHECK2-NEXT:    store i32* [[DOTGLOBAL_TID_]], i32** [[DOTGLOBAL_TID__ADDR]], align 4
// CHECK2-NEXT:    store i32* [[DOTBOUND_TID_]], i32** [[DOTBOUND_TID__ADDR]], align 4
// CHECK2-NEXT:    store i32* [[I]], i32** [[I_ADDR]], align 4
// CHECK2-NEXT:    [[TMP0:%.*]] = load i32*, i32** [[I_ADDR]], align 4
// CHECK2-NEXT:    [[TMP1:%.*]] = load i32, i32* [[TMP0]], align 4
// CHECK2-NEXT:    [[INC:%.*]] = add nsw i32 [[TMP1]], 1
// CHECK2-NEXT:    store i32 [[INC]], i32* [[TMP0]], align 4
// CHECK2-NEXT:    ret void
//
//
// CHECK2-LABEL: define {{[^@]+}}@__omp_outlined__1_wrapper
// CHECK2-SAME: (i16 noundef zeroext [[TMP0:%.*]], i32 noundef [[TMP1:%.*]]) #[[ATTR3:[0-9]+]] {
// CHECK2-NEXT:  entry:
// CHECK2-NEXT:    [[DOTADDR:%.*]] = alloca i16, align 2
// CHECK2-NEXT:    [[DOTADDR1:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[DOTZERO_ADDR:%.*]] = alloca i32, align 4
// CHECK2-NEXT:    [[GLOBAL_ARGS:%.*]] = alloca i8**, align 4
// CHECK2-NEXT:    store i16 [[TMP0]], i16* [[DOTADDR]], align 2
// CHECK2-NEXT:    store i32 [[TMP1]], i32* [[DOTADDR1]], align 4
// CHECK2-NEXT:    store i32 0, i32* [[DOTZERO_ADDR]], align 4
// CHECK2-NEXT:    call void @__kmpc_get_shared_variables(i8*** [[GLOBAL_ARGS]])
// CHECK2-NEXT:    [[TMP2:%.*]] = load i8**, i8*** [[GLOBAL_ARGS]], align 4
// CHECK2-NEXT:    [[TMP3:%.*]] = getelementptr inbounds i8*, i8** [[TMP2]], i32 0
// CHECK2-NEXT:    [[TMP4:%.*]] = bitcast i8** [[TMP3]] to i32**
// CHECK2-NEXT:    [[TMP5:%.*]] = load i32*, i32** [[TMP4]], align 4
// CHECK2-NEXT:    call void @__omp_outlined__1(i32* [[DOTADDR1]], i32* [[DOTZERO_ADDR]], i32* [[TMP5]]) #[[ATTR5]]
// CHECK2-NEXT:    ret void
//
