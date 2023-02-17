//===-- SNESELFObjectWriter.cpp - SNES ELF Writer -----------------------===//

#include "MCTargetDesc/SNESFixupKinds.h"
#include "MCTargetDesc/SNESMCExpr.h"
#include "MCTargetDesc/SNESMCTargetDesc.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

namespace {
class SNESELFObjectWriter : public MCELFObjectTargetWriter {
public:
  SNESELFObjectWriter(bool Is64Bit, uint8_t OSABI)
      : MCELFObjectTargetWriter(Is64Bit, OSABI, ELF::EM_SNES,
                                /*HasRelocationAddend*/ true) {}

  ~SNESELFObjectWriter() override = default;

protected:
  unsigned getRelocType(MCContext &Ctx, const MCValue &Target,
                        const MCFixup &Fixup, bool IsPCRel) const override;

  bool needsRelocateWithSymbol(const MCSymbol &Sym,
                               unsigned Type) const override;
};
} // namespace

unsigned SNESELFObjectWriter::getRelocType(MCContext &Ctx,
                                           const MCValue &Target,
                                           const MCFixup &Fixup,
                                           bool IsPCRel) const {
  MCFixupKind Kind = Fixup.getKind();
  if (Kind >= FirstLiteralRelocationKind)
    return Kind - FirstLiteralRelocationKind;

  if (const SNESMCExpr *SExpr = dyn_cast<SNESMCExpr>(Fixup.getValue())) {
    if (SExpr->getKind() == SNESMCExpr::VK_SNES_R_DISP32)
      return ELF::R_SNES_16;
  }

  if (IsPCRel) {
    switch (Fixup.getTargetKind()) {
    default:
      llvm_unreachable("Unimplemented fixup -> relocation");
    case FK_Data_1:
      return ELF::R_SNES_8;
    case FK_Data_2:
      return ELF::R_SNES_16;
    }
  }

  switch (Fixup.getTargetKind()) {
  default:
    llvm_unreachable("Unimplemented fixup -> relocation");
  case FK_NONE:
    return ELF::R_SNES_NONE;
  case FK_Data_1:
    return ELF::R_SNES_8;
  case FK_Data_2:
    return ELF::R_SNES_16;
  }

  return ELF::R_SNES_NONE;
}

bool SNESELFObjectWriter::needsRelocateWithSymbol(const MCSymbol &Sym,
                                                  unsigned Type) const {
  switch (Type) {
  default:
    return false;
  }
}

std::unique_ptr<MCObjectTargetWriter>
llvm::createSNESELFObjectWriter(bool Is64Bit, uint8_t OSABI) {
  return std::make_unique<SNESELFObjectWriter>(Is64Bit, OSABI);
}
