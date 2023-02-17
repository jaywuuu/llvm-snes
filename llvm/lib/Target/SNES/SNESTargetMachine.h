//===-- SNESTargetMachine.h - Define TargetMachine for SNES ---*- C++ -*-===//

#ifndef LLVM_LIB_TARGET_SNES_SNESTARGETMACHINE_H
#define LLVM_LIB_TARGET_SNES_SNESTARGETMACHINE_H

#include "SNESInstrInfo.h"
#include "SNESSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class SNESTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  SNESSubtarget Subtarget;
  bool is64Bit;
  mutable StringMap<std::unique_ptr<SNESSubtarget>> SubtargetMap;

public:
  SNESTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                    StringRef FS, const TargetOptions &Options,
                    Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                    CodeGenOpt::Level OL, bool JIT, bool is64bit);
  ~SNESTargetMachine() override;

  const SNESSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const SNESSubtarget *getSubtargetImpl(const Function &) const override;

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }
};

/// SNES 32-bit target machine
///
class SNESV8TargetMachine : public SNESTargetMachine {
  virtual void anchor();

public:
  SNESV8TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

/// SNES 64-bit target machine
///
class SNESV9TargetMachine : public SNESTargetMachine {
  virtual void anchor();

public:
  SNESV9TargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

class SNESelTargetMachine : public SNESTargetMachine {
  virtual void anchor();

public:
  SNESelTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                      StringRef FS, const TargetOptions &Options,
                      Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                      CodeGenOpt::Level OL, bool JIT);
};

} // end namespace llvm

#endif
