//===-- SNESMCTargetDesc.cpp - SNES Target Descriptions -----------------===//
//===----------------------------------------------------------------------===//
//
// This file provides SNES specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "SNESMCTargetDesc.h"
#include "SNESInstPrinter.h"
#include "SNESMCAsmInfo.h"
#include "SNESTargetStreamer.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "SNESGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "SNESGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "SNESGenRegisterInfo.inc"

static MCAsmInfo *createSNESMCAsmInfo(const MCRegisterInfo &MRI,
                                      const Triple &TT,
                                      const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new SNESELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(SP::O6, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCAsmInfo *createSNESV9MCAsmInfo(const MCRegisterInfo &MRI,
                                        const Triple &TT,
                                        const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new SNESELFMCAsmInfo(TT);
  unsigned Reg = MRI.getDwarfRegNum(SP::O6, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, Reg, 2047);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCInstrInfo *createSNESMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitSNESMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createSNESMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitSNESMCRegisterInfo(X, SP::O7);
  return X;
}

static MCSubtargetInfo *createSNESMCSubtargetInfo(const Triple &TT,
                                                  StringRef CPU, StringRef FS) {
  if (CPU.empty())
    CPU = (TT.getArch() == Triple::snes) ? "v9" : "v8";
  return createSNESMCSubtargetInfoImpl(TT, CPU, /*TuneCPU*/ CPU, FS);
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new SNESTargetELFStreamer(S);
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new SNESTargetAsmStreamer(S, OS);
}

static MCInstPrinter *createSNESMCInstPrinter(const Triple &T,
                                              unsigned SyntaxVariant,
                                              const MCAsmInfo &MAI,
                                              const MCInstrInfo &MII,
                                              const MCRegisterInfo &MRI) {
  return new SNESInstPrinter(MAI, MII, MRI);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(getTheSNESTarget(), createSNESMCAsmInfo);

  for (Target *T : {&getTheSNESTarget()}) {
    // Register the MC instruction info.
    TargetRegistry::RegisterMCInstrInfo(*T, createSNESMCInstrInfo);

    // Register the MC register info.
    TargetRegistry::RegisterMCRegInfo(*T, createSNESMCRegisterInfo);

    // Register the MC subtarget info.
    TargetRegistry::RegisterMCSubtargetInfo(*T, createSNESMCSubtargetInfo);

    // Register the MC Code Emitter.
    TargetRegistry::RegisterMCCodeEmitter(*T, createSNESMCCodeEmitter);

    // Register the asm backend.
    TargetRegistry::RegisterMCAsmBackend(*T, createSNESAsmBackend);

    // Register the object target streamer.
    TargetRegistry::RegisterObjectTargetStreamer(*T,
                                                 createObjectTargetStreamer);

    // Register the asm streamer.
    TargetRegistry::RegisterAsmTargetStreamer(*T, createTargetAsmStreamer);

    // Register the MCInstPrinter
    TargetRegistry::RegisterMCInstPrinter(*T, createSNESMCInstPrinter);
  }
}
