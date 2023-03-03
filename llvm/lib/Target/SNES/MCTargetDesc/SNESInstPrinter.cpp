//===-- SNESInstPrinter.cpp - Convert SNES MCInst to assembly syntax -----==//
//===----------------------------------------------------------------------===//
//
// This class prints an SNES MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "SNES.h"
#include "SNESInstPrinter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// The generated AsmMatcher SNESGenAsmWriter uses "SNES" as the target
// namespace. But SNES backend uses "SP" as its namespace.
namespace llvm {
namespace SNES {
using namespace SNES;
}
} // namespace llvm

#define GET_INSTRUCTION_NAME
#define PRINT_ALIAS_INSTR
#include "SNESGenAsmWriter.inc"

bool SNESInstPrinter::isV9(const MCSubtargetInfo &STI) const {
  return (STI.getFeatureBits()[SNES::FeatureV9]) != 0;
}

void SNESInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  OS << '%' << StringRef(getRegisterName(RegNo)).lower();
}

void SNESInstPrinter::printInst(const MCInst *MI, uint64_t Address,
                                StringRef Annot, const MCSubtargetInfo &STI,
                                raw_ostream &O) {
  if (!printAliasInstr(MI, Address, STI, O) && !printSNESAliasInstr(MI, STI, O))
    printInstruction(MI, Address, STI, O);
  printAnnotation(O, Annot);
}

bool SNESInstPrinter::printSNESAliasInstr(const MCInst *MI,
                                          const MCSubtargetInfo &STI,
                                          raw_ostream &O) {
  return false;
}

void SNESInstPrinter::printOperand(const MCInst *MI, int opNum,
                                   const MCSubtargetInfo &STI, raw_ostream &O) {
  const MCOperand &MO = MI->getOperand(opNum);

  if (MO.isReg()) {
    printRegName(O, MO.getReg());
    return;
  }

  if (MO.isImm()) {
    switch (MI->getOpcode()) {
    default:
      O << (int)MO.getImm();
      return;
    }
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void SNESInstPrinter::printMemOperand(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo &STI,
                                      raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, STI, O);
}

void SNESInstPrinter::printCCOperand(const MCInst *MI, int opNum,
                                     const MCSubtargetInfo &STI,
                                     raw_ostream &O) {
  int CC = (int)MI->getOperand(opNum).getImm();
  O << SNESCondCodeToString((SPCC::CondCodes)CC);
}

bool SNESInstPrinter::printGetPCX(const MCInst *MI, unsigned opNum,
                                  const MCSubtargetInfo &STI, raw_ostream &O) {
  llvm_unreachable("FIXME: Implement SNESInstPrinter::printGetPCX.");
  return true;
}

void SNESInstPrinter::printMembarTag(const MCInst *MI, int opNum,
                                     const MCSubtargetInfo &STI,
                                     raw_ostream &O) {
  static const char *const TagNames[] = {
      "#LoadLoad",  "#StoreLoad", "#LoadStore", "#StoreStore",
      "#Lookaside", "#MemIssue",  "#Sync"};

  unsigned Imm = MI->getOperand(opNum).getImm();

  if (Imm > 127) {
    O << Imm;
    return;
  }

  bool First = true;
  for (unsigned i = 0; i < sizeof(TagNames) / sizeof(char *); i++) {
    if (Imm & (1 << i)) {
      O << (First ? "" : " | ") << TagNames[i];
      First = false;
    }
  }
}
