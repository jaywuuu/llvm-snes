//===-- SNESRegisterInfo.cpp - SNES Register Information ----------------===//

#include "SNESRegisterInfo.h"
#include "SNES.h"
#include "SNESMachineFunctionInfo.h"
#include "SNESSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"

using namespace llvm;

#define GET_REGINFO_TARGET_DESC
#include "SNESGenRegisterInfo.inc"

SNESRegisterInfo::SNESRegisterInfo() : SNESGenRegisterInfo(0) {}

const MCPhysReg *SNESRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  return nullptr;
}

BitVector SNESRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  return BitVector{};
}

Register SNESRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return 0;
}

void SNESRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                          int SPAdj, unsigned FIOperandNum,
                          RegScavenger *RS) const {}