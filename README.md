# llvm-snes
Let's give this another try with LLVM 15.0.0.7.  Attempting to add a SNES backend target to LLVM, then eventually add a driver using clang.

Build:
mkdir build
cd build
cmake -G Ninja -DLLVM_ENABLE_PROJECTS='clang;clang-tools-extra' -DCMAKE_BUILD_TYPE=Debug -DLLVM_OPTIMIZED_TABLEGEN=On ../llvm
