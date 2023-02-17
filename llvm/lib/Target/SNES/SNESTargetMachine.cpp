//===-- SNESTargetMachine.cpp - Define TargetMachine for SNES -----------===//

#include "SNESTargetMachine.h"
#include "SNES.h"
#include "SNESTargetObjectFile.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
using namespace llvm;

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESTarget() {
  // Register the target.
  RegisterTargetMachine<SNESV8TargetMachine> X(getTheSNESTarget());
}

static std::string computeDataLayout(const Triple &T, bool is64Bit) {
  // SNES is typically big endian, but some are little.
  std::string Ret = "E";
  Ret += "-m:e";

  // Some ABIs have 32bit pointers.
  if (!is64Bit)
    Ret += "-p:32:32";

  // Alignments for 64 bit integers.
  Ret += "-i64:64";

  // On SNESV9 128 floats are aligned to 128 bits, on others only to 64.
  // On SNESV9 registers can hold 64 or 32 bits, on others only 32.
  if (is64Bit)
    Ret += "-n32:64";
  else
    Ret += "-f128:64-n32";

  if (is64Bit)
    Ret += "-S128";
  else
    Ret += "-S64";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
  return RM.value_or(Reloc::Static);
}

// Code models. Some only make sense for 64-bit code.
//
// SunCC  Reloc   CodeModel  Constraints
// abs32  Static  Small      text+data+bss linked below 2^32 bytes
// abs44  Static  Medium     text+data+bss linked below 2^44 bytes
// abs64  Static  Large      text smaller than 2^31 bytes
// pic13  PIC_    Small      GOT < 2^13 bytes
// pic32  PIC_    Medium     GOT < 2^32 bytes
//
// All code models require that the text segment is smaller than 2GB.
static CodeModel::Model getEffectiveSNESCodeModel(Optional<CodeModel::Model> CM,
                                                  Reloc::Model RM, bool Is64Bit,
                                                  bool JIT) {
  if (CM) {
    if (*CM == CodeModel::Tiny)
      report_fatal_error("Target does not support the tiny CodeModel", false);
    if (*CM == CodeModel::Kernel)
      report_fatal_error("Target does not support the kernel CodeModel", false);
    return *CM;
  }
  if (Is64Bit) {
    if (JIT)
      return CodeModel::Large;
    return RM == Reloc::PIC_ ? CodeModel::Small : CodeModel::Medium;
  }
  return CodeModel::Small;
}

/// Create an ILP32 architecture model
SNESTargetMachine::SNESTargetMachine(
    const Target &T, const Triple &TT, StringRef CPU, StringRef FS,
    const TargetOptions &Options, Optional<Reloc::Model> RM,
    Optional<CodeModel::Model> CM, CodeGenOpt::Level OL, bool JIT, bool is64bit)
    : LLVMTargetMachine(T, computeDataLayout(TT, is64bit), TT, CPU, FS, Options,
                        getEffectiveRelocModel(RM),
                        getEffectiveSNESCodeModel(
                            CM, getEffectiveRelocModel(RM), is64bit, JIT),
                        OL),
      TLOF(std::make_unique<SNESELFTargetObjectFile>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this, is64bit),
      is64Bit(is64bit) {
  initAsmInfo();
}

SNESTargetMachine::~SNESTargetMachine() = default;

const SNESSubtarget *
SNESTargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  std::string CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString().str() : TargetCPU;
  std::string FS =
      FSAttr.isValid() ? FSAttr.getValueAsString().str() : TargetFS;

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function, so we can enable it as a subtarget feature.
  bool softFloat = F.getFnAttribute("use-soft-float").getValueAsBool();

  if (softFloat)
    FS += FS.empty() ? "+soft-float" : ",+soft-float";

  auto &I = SubtargetMap[CPU + FS];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<SNESSubtarget>(TargetTriple, CPU, FS, *this,
                                        this->is64Bit);
  }
  return I.get();
}

namespace {
/// SNES Code Generator Pass Configuration Options.
class SNESPassConfig : public TargetPassConfig {
public:
  SNESPassConfig(SNESTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  SNESTargetMachine &getSNESTargetMachine() const {
    return getTM<SNESTargetMachine>();
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  void addPreEmitPass() override;
};
} // namespace

TargetPassConfig *SNESTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new SNESPassConfig(*this, PM);
}

void SNESPassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());

  TargetPassConfig::addIRPasses();
}

bool SNESPassConfig::addInstSelector() {
  addPass(createSNESISelDag(getSNESTargetMachine()));
  return false;
}

void SNESPassConfig::addPreEmitPass() {}

void SNESV8TargetMachine::anchor() {}

SNESV8TargetMachine::SNESV8TargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : SNESTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, false) {}

void SNESV9TargetMachine::anchor() {}

SNESV9TargetMachine::SNESV9TargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : SNESTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, true) {}

void SNESelTargetMachine::anchor() {}

SNESelTargetMachine::SNESelTargetMachine(const Target &T, const Triple &TT,
                                         StringRef CPU, StringRef FS,
                                         const TargetOptions &Options,
                                         Optional<Reloc::Model> RM,
                                         Optional<CodeModel::Model> CM,
                                         CodeGenOpt::Level OL, bool JIT)
    : SNESTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL, JIT, false) {}
