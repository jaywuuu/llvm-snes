//===-- SNESRegisterInfo.h - SNES Register Information Impl ---*- C++ -*-===//

#ifndef LLVM_LIB_TARGET_SNES_SNESREGISTERINFO_H
#define LLVM_LIB_TARGET_SNES_SNESREGISTERINFO_H

#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "SNESGenRegisterInfo.inc"

namespace llvm {
struct SNESRegisterInfo : public SNESGenRegisterInfo {
  SNESRegisterInfo();

  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;
  BitVector getReservedRegs(const MachineFunction &MF) const override;
  Register getFrameRegister(const MachineFunction &MF) const override;

  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, unsigned FIOperandNum,
                           RegScavenger *RS = nullptr) const override;
};

} // end namespace llvm

#endif
