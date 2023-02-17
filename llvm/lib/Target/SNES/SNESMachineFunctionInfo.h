//===- SNESMachineFunctionInfo.h - SNES Machine Function Info -*- C++ -*-===//

#ifndef LLVM_LIB_TARGET_SNES_SNESMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_SNES_SNESMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

class SNESMachineFunctionInfo : public MachineFunctionInfo {
  virtual void anchor();

private:
  Register GlobalBaseReg;

  /// VarArgsFrameOffset - Frame offset to start of varargs area.
  int VarArgsFrameOffset;

  /// SRetReturnReg - Holds the virtual register into which the sret
  /// argument is passed.
  Register SRetReturnReg;

  /// IsLeafProc - True if the function is a leaf procedure.
  bool IsLeafProc;

public:
  SNESMachineFunctionInfo()
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}
  explicit SNESMachineFunctionInfo(MachineFunction &MF)
      : GlobalBaseReg(0), VarArgsFrameOffset(0), SRetReturnReg(0),
        IsLeafProc(false) {}

  MachineFunctionInfo *
  clone(BumpPtrAllocator &Allocator, MachineFunction &DestMF,
        const DenseMap<MachineBasicBlock *, MachineBasicBlock *> &Src2DstMBB)
      const override;

  Register getGlobalBaseReg() const { return GlobalBaseReg; }
  void setGlobalBaseReg(Register Reg) { GlobalBaseReg = Reg; }

  int getVarArgsFrameOffset() const { return VarArgsFrameOffset; }
  void setVarArgsFrameOffset(int Offset) { VarArgsFrameOffset = Offset; }

  Register getSRetReturnReg() const { return SRetReturnReg; }
  void setSRetReturnReg(Register Reg) { SRetReturnReg = Reg; }

  void setLeafProc(bool rhs) { IsLeafProc = rhs; }
  bool isLeafProc() const { return IsLeafProc; }
};
} // namespace llvm

#endif
