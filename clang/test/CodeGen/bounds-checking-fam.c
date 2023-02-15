// REQUIRES: x86-registered-target
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=0 -fsanitize=array-bounds        %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-0
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=0 -fsanitize=array-bounds -x c++ %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-0,CXX,CXX-STRICT-0
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=1 -fsanitize=array-bounds        %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-1
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=1 -fsanitize=array-bounds -x c++ %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-1,CXX
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=2 -fsanitize=array-bounds        %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-2
// RUN: %clang_cc1 -emit-llvm -triple x86_64 -fstrict-flex-arrays=2 -fsanitize=array-bounds -x c++ %s -o - | FileCheck %s --check-prefixes=CHECK,CHECK-STRICT-2,CXX
// Before flexible array member was added to C99, many projects use a
// one-element array as the last member of a structure as an alternative.
// E.g. https://github.com/python/cpython/issues/84301
// Suppress such errors with -fstrict-flex-arrays=0.
struct One {
  int a[1];
};
struct Two {
  int a[2];
};
struct Three {
  int a[3];
};

// CHECK-LABEL: define {{.*}} @{{.*}}test_one{{.*}}(
int test_one(struct One *p, int i) {
  // CHECK-STRICT-0-NOT: @__ubsan
  // CHECK-STRICT-1-NOT: @__ubsan
  // CHECK-STRICT-2:     call void @__ubsan_handle_out_of_bounds_abort(
  return p->a[i] + (p->a)[i];
}

// CHECK-LABEL: define {{.*}} @{{.*}}test_two{{.*}}(
int test_two(struct Two *p, int i) {
  // CHECK-STRICT-0:     call void @__ubsan_handle_out_of_bounds_abort(
  // CHECK-STRICT-1:     call void @__ubsan_handle_out_of_bounds_abort(
  // CHECK-STRICT-2:     call void @__ubsan_handle_out_of_bounds_abort(
  return p->a[i] + (p->a)[i];
}

// CHECK-LABEL: define {{.*}} @{{.*}}test_three{{.*}}(
int test_three(struct Three *p, int i) {
  // CHECK-STRICT-0:     call void @__ubsan_handle_out_of_bounds_abort(
  // CHECK-STRICT-1:     call void @__ubsan_handle_out_of_bounds_abort(
  // CHECK-STRICT-2:     call void @__ubsan_handle_out_of_bounds_abort(
  return p->a[i] + (p->a)[i];
}

#define FLEXIBLE 1
struct Macro {
  int a[FLEXIBLE];
};

// CHECK-LABEL: define {{.*}} @{{.*}}test_macro{{.*}}(
int test_macro(struct Macro *p, int i) {
  // CHECK-STRICT-0-NOT: @__ubsan
  return p->a[i] + (p->a)[i];
}

#if defined __cplusplus

struct Base {
  int b;
};
struct NoStandardLayout : Base {
  int a[1];
};

// CXX-LABEL: define {{.*}} @{{.*}}test_nostandardlayout{{.*}}(
int test_nostandardlayout(NoStandardLayout *p, int i) {
  // CXX-STRICT-0-NOT: @__ubsan
  return p->a[i] + (p->a)[i];
}

template<int N> struct Template {
  int a[N];
};

// CXX-LABEL: define {{.*}} @{{.*}}test_template{{.*}}(
int test_template(Template<1> *p, int i) {
  // CXX-STRICT-0-NOT: @__ubsan
  return p->a[i] + (p->a)[i];
}

#endif
