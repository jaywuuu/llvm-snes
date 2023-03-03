//===-- SNESAsmPrinter.cpp - SNES LLVM assembly writer ------------------===//

#include "MCTargetDesc/SNESInstPrinter.h"
#include "MCTargetDesc/SNESMCExpr.h"
#include "MCTargetDesc/SNESTargetStreamer.h"
#include "SNES.h"
#include "SNESInstrInfo.h"
#include "SNESTargetMachine.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
class SNESAsmPrinter : public AsmPrinter {
  SNESTargetStreamer &getTargetStreamer() {
    return static_cast<SNESTargetStreamer &>(*OutStreamer->getTargetStreamer());
  }

public:
  explicit SNESAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)) {}

  StringRef getPassName() const override { return "SNES Assembly Printer"; }

  void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
  void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                       const char *Modifier = nullptr);

  void emitFunctionBodyStart() override;
  void emitInstruction(const MachineInstr *MI) override;

  static const char *getRegisterName(unsigned RegNo) {
    return SNESInstPrinter::getRegisterName(RegNo);
  }

  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &O) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                             const char *ExtraCode, raw_ostream &O) override;

  void LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                 const MCSubtargetInfo &STI);
};
} // end of anonymous namespace

void SNESAsmPrinter::LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                               const MCSubtargetInfo &STI) {
}

void SNESAsmPrinter::emitInstruction(const MachineInstr *MI) {
  SNES_MC::verifyInstructionPredicates(MI->getOpcode(),
                                       getSubtargetInfo().getFeatureBits());
}

void SNESAsmPrinter::emitFunctionBodyStart() {
}

void SNESAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                  raw_ostream &O) {
}

void SNESAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                     raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, O);
  O << "+";
  printOperand(MI, opNum + 1, O);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool SNESAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                     const char *ExtraCode, raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0)
      return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, O);
    case 'f':
    case 'r':
      break;
    }
  }

  printOperand(MI, OpNo, O);

  return false;
}

bool SNESAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                           unsigned OpNo, const char *ExtraCode,
                                           raw_ostream &O) {
  if (ExtraCode && ExtraCode[0])
    return true; // Unknown modifier

  O << '[';
  printMemOperand(MI, OpNo, O);
  O << ']';

  return false;
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESAsmPrinter() {
  RegisterAsmPrinter<SNESAsmPrinter> X(getTheSNESTarget());
}
