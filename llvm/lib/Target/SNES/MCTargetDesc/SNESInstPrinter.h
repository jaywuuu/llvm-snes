//===-- SNESInstPrinter.h - Convert SNES MCInst to assembly syntax ------===//
//===----------------------------------------------------------------------===//
//
// This class prints an SNES MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESINSTPRINTER_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESINSTPRINTER_H

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {

class SNESInstPrinter : public MCInstPrinter {
public:
  SNESInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                  const MCRegisterInfo &MRI)
      : MCInstPrinter(MAI, MII, MRI) {}

  void printRegName(raw_ostream &OS, unsigned RegNo) const override;
  void printInst(const MCInst *MI, uint64_t Address, StringRef Annot,
                 const MCSubtargetInfo &STI, raw_ostream &O) override;
  bool printSNESAliasInstr(const MCInst *MI, const MCSubtargetInfo &STI,
                           raw_ostream &OS);
  bool isV9(const MCSubtargetInfo &STI) const;

  // Autogenerated by tblgen.
  std::pair<const char *, uint64_t> getMnemonic(const MCInst *MI) override;
  void printInstruction(const MCInst *MI, uint64_t Address,
                        const MCSubtargetInfo &STI, raw_ostream &O);
  bool printAliasInstr(const MCInst *MI, uint64_t Address,
                       const MCSubtargetInfo &STI, raw_ostream &O);
  void printCustomAliasOperand(const MCInst *MI, uint64_t Address,
                               unsigned OpIdx, unsigned PrintMethodIdx,
                               const MCSubtargetInfo &STI, raw_ostream &O);
  static const char *getRegisterName(unsigned RegNo);

  void printOperand(const MCInst *MI, int opNum, const MCSubtargetInfo &STI,
                    raw_ostream &OS);
  void printMemOperand(const MCInst *MI, int opNum, const MCSubtargetInfo &STI,
                       raw_ostream &OS, const char *Modifier = nullptr);
  void printCCOperand(const MCInst *MI, int opNum, const MCSubtargetInfo &STI,
                      raw_ostream &OS);
  bool printGetPCX(const MCInst *MI, unsigned OpNo, const MCSubtargetInfo &STI,
                   raw_ostream &OS);
  void printMembarTag(const MCInst *MI, int opNum, const MCSubtargetInfo &STI,
                      raw_ostream &O);
};
} // end namespace llvm

#endif