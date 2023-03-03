//===-- SNESFrameLowering.cpp - SNES Frame Information ------------------===//

#include "SNESFrameLowering.h"
#include "SNESInstrInfo.h"
#include "SNESMachineFunctionInfo.h"
#include "SNESSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

static cl::opt<bool>
    DisableLeafProc("disable-SNES-leaf-proc", cl::init(false),
                    cl::desc("Disable SNES leaf procedure optimization."),
                    cl::Hidden);

SNESFrameLowering::SNESFrameLowering(const SNESSubtarget &ST)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown,
                          ST.is64Bit() ? Align(16) : Align(8), 0,
                          ST.is64Bit() ? Align(16) : Align(8)) {}

void SNESFrameLowering::emitSPAdjustment(MachineFunction &MF,
                                         MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator MBBI,
                                         int NumBytes, unsigned ADDrr,
                                         unsigned ADDri) const {
}

void SNESFrameLowering::emitPrologue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
}

MachineBasicBlock::iterator SNESFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  return MBB.erase(I);
}

void SNESFrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
}

bool SNESFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // Reserve call frame if there are no variable sized objects on the stack.
  return !MF.getFrameInfo().hasVarSizedObjects();
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas or
// if frame pointer elimination is disabled.
bool SNESFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
}

StackOffset
SNESFrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                          Register &FrameReg) const {
  const SNESSubtarget &Subtarget = MF.getSubtarget<SNESSubtarget>();
  
  int64_t FrameOffset =
      MF.getFrameInfo().getObjectOffset(FI) + Subtarget.getStackPointerBias();

  return StackOffset::getFixed(FrameOffset +
                                MF.getFrameInfo().getStackSize());
}

static bool LLVM_ATTRIBUTE_UNUSED
verifyLeafProcRegUse(MachineRegisterInfo *MRI) {
  return true;
}

bool SNESFrameLowering::isLeafProc(MachineFunction &MF) const {
  return false;
}

void SNESFrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {
}

void SNESFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                             BitVector &SavedRegs,
                                             RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  if (!DisableLeafProc && isLeafProc(MF)) {
    SNESMachineFunctionInfo *MFI = MF.getInfo<SNESMachineFunctionInfo>();
    MFI->setLeafProc(true);

    remapRegsForLeafProc(MF);
  }
}
