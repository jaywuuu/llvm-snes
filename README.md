# llvm-snes
Let's give this another try with LLVM 15.0.0.7.  Attempting to add a SNES backend target to LLVM, then eventually add a driver using clang.

Recommended build steps:

```
mkdir build

cd build

cmake -DLLVM_ENABLE_PROJECTS='clang;clang-tools-extra' -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_BUILD_TYPE=Debug -DLLVM_OPTIMIZED_TABLEGEN=On ../llvm
```

Set -DLLVM_TARGETS_TO_BUILD to desired targets.
