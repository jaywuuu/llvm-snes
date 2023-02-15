// NOTE: Assertions have been autogenerated by utils/update_cc_test_checks.py
/// Check that the alignment builtins handle array-to-pointer decay
// RUN: %clang_cc1 -no-opaque-pointers -triple=x86_64-unknown-unknown -o - -emit-llvm %s | FileCheck %s

extern int func(char *c);

// CHECK-LABEL: @test_array(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[BUF:%.*]] = alloca [1024 x i8], align 16
// CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds [1024 x i8], [1024 x i8]* [[BUF]], i64 0, i64 44
// CHECK-NEXT:    [[INTPTR:%.*]] = ptrtoint i8* [[ARRAYIDX]] to i64
// CHECK-NEXT:    [[ALIGNED_INTPTR:%.*]] = and i64 [[INTPTR]], -16
// CHECK-NEXT:    [[DIFF:%.*]] = sub i64 [[ALIGNED_INTPTR]], [[INTPTR]]
// CHECK-NEXT:    [[ALIGNED_RESULT:%.*]] = getelementptr inbounds i8, i8* [[ARRAYIDX]], i64 [[DIFF]]
// CHECK-NEXT:    call void @llvm.assume(i1 true) [ "align"(i8* [[ALIGNED_RESULT]], i64 16) ]
// CHECK-NEXT:    [[CALL:%.*]] = call i32 @func(i8* noundef [[ALIGNED_RESULT]])
// CHECK-NEXT:    [[ARRAYIDX1:%.*]] = getelementptr inbounds [1024 x i8], [1024 x i8]* [[BUF]], i64 0, i64 22
// CHECK-NEXT:    [[INTPTR2:%.*]] = ptrtoint i8* [[ARRAYIDX1]] to i64
// CHECK-NEXT:    [[OVER_BOUNDARY:%.*]] = add i64 [[INTPTR2]], 31
// CHECK-NEXT:    [[ALIGNED_INTPTR4:%.*]] = and i64 [[OVER_BOUNDARY]], -32
// CHECK-NEXT:    [[DIFF5:%.*]] = sub i64 [[ALIGNED_INTPTR4]], [[INTPTR2]]
// CHECK-NEXT:    [[ALIGNED_RESULT6:%.*]] = getelementptr inbounds i8, i8* [[ARRAYIDX1]], i64 [[DIFF5]]
// CHECK-NEXT:    call void @llvm.assume(i1 true) [ "align"(i8* [[ALIGNED_RESULT6]], i64 32) ]
// CHECK-NEXT:    [[CALL7:%.*]] = call i32 @func(i8* noundef [[ALIGNED_RESULT6]])
// CHECK-NEXT:    [[ARRAYIDX8:%.*]] = getelementptr inbounds [1024 x i8], [1024 x i8]* [[BUF]], i64 0, i64 16
// CHECK-NEXT:    [[SRC_ADDR:%.*]] = ptrtoint i8* [[ARRAYIDX8]] to i64
// CHECK-NEXT:    [[SET_BITS:%.*]] = and i64 [[SRC_ADDR]], 63
// CHECK-NEXT:    [[IS_ALIGNED:%.*]] = icmp eq i64 [[SET_BITS]], 0
// CHECK-NEXT:    [[CONV:%.*]] = zext i1 [[IS_ALIGNED]] to i32
// CHECK-NEXT:    ret i32 [[CONV]]
//
int test_array(void) {
  char buf[1024];
  func(__builtin_align_down(&buf[44], 16));
  func(__builtin_align_up(&buf[22], 32));
  return __builtin_is_aligned(&buf[16], 64);
}

// CHECK-LABEL: @test_array_should_not_mask(
// CHECK-NEXT:  entry:
// CHECK-NEXT:    [[BUF:%.*]] = alloca [1024 x i8], align 32
// CHECK-NEXT:    [[ARRAYIDX:%.*]] = getelementptr inbounds [1024 x i8], [1024 x i8]* [[BUF]], i64 0, i64 64
// CHECK-NEXT:    [[INTPTR:%.*]] = ptrtoint i8* [[ARRAYIDX]] to i64
// CHECK-NEXT:    [[ALIGNED_INTPTR:%.*]] = and i64 [[INTPTR]], -16
// CHECK-NEXT:    [[DIFF:%.*]] = sub i64 [[ALIGNED_INTPTR]], [[INTPTR]]
// CHECK-NEXT:    [[ALIGNED_RESULT:%.*]] = getelementptr inbounds i8, i8* [[ARRAYIDX]], i64 [[DIFF]]
// CHECK-NEXT:    call void @llvm.assume(i1 true) [ "align"(i8* [[ALIGNED_RESULT]], i64 16) ]
// CHECK-NEXT:    [[CALL:%.*]] = call i32 @func(i8* noundef [[ALIGNED_RESULT]])
// CHECK-NEXT:    [[ARRAYIDX1:%.*]] = getelementptr inbounds [1024 x i8], [1024 x i8]* [[BUF]], i64 0, i64 32
// CHECK-NEXT:    [[INTPTR2:%.*]] = ptrtoint i8* [[ARRAYIDX1]] to i64
// CHECK-NEXT:    [[OVER_BOUNDARY:%.*]] = add i64 [[INTPTR2]], 31
// CHECK-NEXT:    [[ALIGNED_INTPTR4:%.*]] = and i64 [[OVER_BOUNDARY]], -32
// CHECK-NEXT:    [[DIFF5:%.*]] = sub i64 [[ALIGNED_INTPTR4]], [[INTPTR2]]
// CHECK-NEXT:    [[ALIGNED_RESULT6:%.*]] = getelementptr inbounds i8, i8* [[ARRAYIDX1]], i64 [[DIFF5]]
// CHECK-NEXT:    call void @llvm.assume(i1 true) [ "align"(i8* [[ALIGNED_RESULT6]], i64 32) ]
// CHECK-NEXT:    [[CALL7:%.*]] = call i32 @func(i8* noundef [[ALIGNED_RESULT6]])
// CHECK-NEXT:    ret i32 1
//
int test_array_should_not_mask(void) {
  _Alignas(32) char buf[1024];
  // TODO: The align_up and align_down calls should be folded to no-ops
  func(__builtin_align_down(&buf[64], 16));
  func(__builtin_align_up(&buf[32], 32));
  // This expression can be constant-evaluated:
  return __builtin_is_aligned(&buf[64], 32);
}
