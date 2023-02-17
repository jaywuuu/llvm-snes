//===-- SNESTargetObjectFile.h - SNES Object Info -------------*- C++ -*-===//

#ifndef LLVM_LIB_TARGET_SNES_SNESTARGETOBJECTFILE_H
#define LLVM_LIB_TARGET_SNES_SNESTARGETOBJECTFILE_H

#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"

namespace llvm {

class MCContext;
class TargetMachine;

class SNESELFTargetObjectFile : public TargetLoweringObjectFileELF {
public:
  SNESELFTargetObjectFile() = default;

  void Initialize(MCContext &Ctx, const TargetMachine &TM) override;

  const MCExpr *getTTypeGlobalReference(const GlobalValue *GV,
                                        unsigned Encoding,
                                        const TargetMachine &TM,
                                        MachineModuleInfo *MMI,
                                        MCStreamer &Streamer) const override;
};

} // end namespace llvm

#endif
