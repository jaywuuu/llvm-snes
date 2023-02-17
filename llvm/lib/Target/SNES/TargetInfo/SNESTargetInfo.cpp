//===-- SNESTargetInfo.cpp - SNES Target Implementation -----------------===//

#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheSNESTarget() {
  static Target TheSNESTarget;
  return TheSNESTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESTargetInfo() {
  RegisterTarget<Triple::snes, /*HasJIT=*/false> X(getTheSNESTarget(), "SNES",
                                                   "SNES", "SNES");
}
