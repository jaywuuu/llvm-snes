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
using namespace SP;
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
  switch (MI->getOpcode()) {
  default:
    return false;
  case SP::JMPLrr:
  case SP::JMPLri: {
    if (MI->getNumOperands() != 3)
      return false;
    if (!MI->getOperand(0).isReg())
      return false;
    switch (MI->getOperand(0).getReg()) {
    default:
      return false;
    case SP::G0: // jmp $addr | ret | retl
      if (MI->getOperand(2).isImm() && MI->getOperand(2).getImm() == 8) {
        switch (MI->getOperand(1).getReg()) {
        default:
          break;
        case SP::I7:
          O << "\tret";
          return true;
        case SP::O7:
          O << "\tretl";
          return true;
        }
      }
      O << "\tjmp ";
      printMemOperand(MI, 1, STI, O);
      return true;
    case SP::O7: // call $addr
      O << "\tcall ";
      printMemOperand(MI, 1, STI, O);
      return true;
    }
  }
  case SP::V9FCMPS:
  case SP::V9FCMPD:
  case SP::V9FCMPQ:
  case SP::V9FCMPES:
  case SP::V9FCMPED:
  case SP::V9FCMPEQ: {
    if (isV9(STI) || (MI->getNumOperands() != 3) ||
        (!MI->getOperand(0).isReg()) ||
        (MI->getOperand(0).getReg() != SP::FCC0))
      return false;
    // if V8, skip printing %fcc0.
    switch (MI->getOpcode()) {
    default:
    case SP::V9FCMPS:
      O << "\tfcmps ";
      break;
    case SP::V9FCMPD:
      O << "\tfcmpd ";
      break;
    case SP::V9FCMPQ:
      O << "\tfcmpq ";
      break;
    case SP::V9FCMPES:
      O << "\tfcmpes ";
      break;
    case SP::V9FCMPED:
      O << "\tfcmped ";
      break;
    case SP::V9FCMPEQ:
      O << "\tfcmpeq ";
      break;
    }
    printOperand(MI, 1, STI, O);
    O << ", ";
    printOperand(MI, 2, STI, O);
    return true;
  }
  }
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

    case SP::TICCri: // Fall through
    case SP::TICCrr: // Fall through
    case SP::TRAPri: // Fall through
    case SP::TRAPrr: // Fall through
    case SP::TXCCri: // Fall through
    case SP::TXCCrr: // Fall through
      // Only seven-bit values up to 127.
      O << ((int)MO.getImm() & 0x7f);
      return;
    }
  }

  assert(MO.isExpr() && "Unknown operand kind in printOperand");
  MO.getExpr()->print(O, &MAI);
}

void SNESInstPrinter::printMemOperand(const MCInst *MI, int opNum,
                                      const MCSubtargetInfo &STI,
                                      raw_ostream &O, const char *Modifier) {
  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    printOperand(MI, opNum, STI, O);
    O << ", ";
    printOperand(MI, opNum + 1, STI, O);
    return;
  }

  const MCOperand &Op1 = MI->getOperand(opNum);
  const MCOperand &Op2 = MI->getOperand(opNum + 1);

  bool PrintedFirstOperand = false;
  if (Op1.isReg() && Op1.getReg() != SP::G0) {
    printOperand(MI, opNum, STI, O);
    PrintedFirstOperand = true;
  }

  // Skip the second operand iff it adds nothing (literal 0 or %g0) and we've
  // already printed the first one
  const bool SkipSecondOperand =
      PrintedFirstOperand && ((Op2.isReg() && Op2.getReg() == SP::G0) ||
                              (Op2.isImm() && Op2.getImm() == 0));

  if (!SkipSecondOperand) {
    if (PrintedFirstOperand)
      O << '+';
    printOperand(MI, opNum + 1, STI, O);
  }
}

void SNESInstPrinter::printCCOperand(const MCInst *MI, int opNum,
                                     const MCSubtargetInfo &STI,
                                     raw_ostream &O) {
  int CC = (int)MI->getOperand(opNum).getImm();
  switch (MI->getOpcode()) {
  default:
    break;
  case SP::FBCOND:
  case SP::FBCONDA:
  case SP::BPFCC:
  case SP::BPFCCA:
  case SP::BPFCCNT:
  case SP::BPFCCANT:
  case SP::MOVFCCrr:
  case SP::V9MOVFCCrr:
  case SP::MOVFCCri:
  case SP::V9MOVFCCri:
  case SP::FMOVS_FCC:
  case SP::V9FMOVS_FCC:
  case SP::FMOVD_FCC:
  case SP::V9FMOVD_FCC:
  case SP::FMOVQ_FCC:
  case SP::V9FMOVQ_FCC:
    // Make sure CC is a fp conditional flag.
    CC = (CC < 16) ? (CC + 16) : CC;
    break;
  case SP::CBCOND:
  case SP::CBCONDA:
    // Make sure CC is a cp conditional flag.
    CC = (CC < 32) ? (CC + 32) : CC;
    break;
  }
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
