//===-- SNESMachineFunctionInfo.cpp - SNES Machine Function Info --------===//

#include "SNESMachineFunctionInfo.h"

using namespace llvm;

void SNESMachineFunctionInfo::anchor() {}

MachineFunctionInfo *SNESMachineFunctionInfo::clone(
    BumpPtrAllocator &Allocator, MachineFunction &DestMF,
    const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
    const {
  return DestMF.cloneInfo<SNESMachineFunctionInfo>(*this);
}
