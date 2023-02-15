// RUN: %clang_cc1 -no-opaque-pointers -std=c++17 -fsanitize=function -emit-llvm -triple x86_64-linux-gnu %s -o - | FileCheck %s

// Check that typeinfo recorded in function prolog doesn't have "Do" noexcept
// qualifier in its mangled name.
// CHECK: [[PROXY:@.*]] = private unnamed_addr constant i8* bitcast ({ i8*, i8* }* @_ZTIFvvE to i8*)
// CHECK: define{{.*}} void @_Z1fv() #{{.*}} !func_sanitize ![[FUNCSAN:.*]] {
void f() noexcept {}

// CHECK: define{{.*}} void @_Z1gPDoFvvE
void g(void (*p)() noexcept) {
  // Check that reference typeinfo at call site doesn't have "Do" noexcept
  // qualifier in its mangled name, either.
  // CHECK: icmp eq i8* %{{.*}}, bitcast ({ i8*, i8* }* @_ZTIFvvE to i8*), !nosanitize
  p();
}

// CHECK: ![[FUNCSAN]] = !{i32 846595819, i8** [[PROXY]]}
