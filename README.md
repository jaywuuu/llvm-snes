# llvm-snes
Let's give this another try with LLVM 13.  Attempting to add a SNES backend target to LLVM.

Build:
mkdir build
cd build
cmake -G Ninja -DLLVM_ENABLE_PROJECTS='clang;clang-tools-extra' -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=f:/llvm-snes/out/ -DLLVM_OPTIMIZED_TABLEGEN=On ../llvm
