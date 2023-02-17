//===- SNESMCAsmInfo.cpp - SNES asm properties --------------------------===//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the SNESMCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "SNESMCAsmInfo.h"
#include "SNESMCExpr.h"
#include "llvm/ADT/Triple.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"

using namespace llvm;

void SNESELFMCAsmInfo::anchor() {}

SNESELFMCAsmInfo::SNESELFMCAsmInfo(const Triple &TheTriple) {
  bool isV9 = (TheTriple.getArch() == Triple::snes);
  IsLittleEndian = (TheTriple.getArch() == Triple::snes);

  if (isV9) {
    CodePointerSize = CalleeSaveStackSlotSize = 8;
  }

  Data16bitsDirective = "\t.half\t";
  Data32bitsDirective = "\t.word\t";
  // .xword is only supported by V9.
  Data64bitsDirective = (isV9) ? "\t.xword\t" : nullptr;
  ZeroDirective = "\t.skip\t";
  CommentString = "!";
  SupportsDebugInformation = true;

  ExceptionsType = ExceptionHandling::DwarfCFI;

  UsesELFSectionDirectiveForBSS = true;
}

const MCExpr *SNESELFMCAsmInfo::getExprForPersonalitySymbol(
    const MCSymbol *Sym, unsigned Encoding, MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return SNESMCExpr::create(SNESMCExpr::VK_SNES_R_DISP32,
                              MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }

  return MCAsmInfo::getExprForPersonalitySymbol(Sym, Encoding, Streamer);
}

const MCExpr *
SNESELFMCAsmInfo::getExprForFDESymbol(const MCSymbol *Sym, unsigned Encoding,
                                      MCStreamer &Streamer) const {
  if (Encoding & dwarf::DW_EH_PE_pcrel) {
    MCContext &Ctx = Streamer.getContext();
    return SNESMCExpr::create(SNESMCExpr::VK_SNES_R_DISP32,
                              MCSymbolRefExpr::create(Sym, Ctx), Ctx);
  }
  return MCAsmInfo::getExprForFDESymbol(Sym, Encoding, Streamer);
}
