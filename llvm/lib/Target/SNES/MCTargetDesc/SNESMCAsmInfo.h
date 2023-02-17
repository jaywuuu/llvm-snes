//===- SNESMCAsmInfo.h - SNES asm properties -----------------*- C++ -*--===//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the SNESMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCASMINFO_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class SNESELFMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit SNESELFMCAsmInfo(const Triple &TheTriple);

  const MCExpr *
  getExprForPersonalitySymbol(const MCSymbol *Sym, unsigned Encoding,
                              MCStreamer &Streamer) const override;
  const MCExpr *getExprForFDESymbol(const MCSymbol *Sym, unsigned Encoding,
                                    MCStreamer &Streamer) const override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCASMINFO_H
