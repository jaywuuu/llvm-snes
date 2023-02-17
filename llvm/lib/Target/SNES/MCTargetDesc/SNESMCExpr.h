//====- SNESMCExpr.h - SNES specific MC expression classes --*- C++ -*-=====//
//===----------------------------------------------------------------------===//
//
// This file describes SNES-specific MCExprs, used for modifiers like
// "%hi" or "%lo" etc.,
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCEXPR_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCEXPR_H

#include "SNESFixupKinds.h"
#include "llvm/MC/MCExpr.h"

namespace llvm {

class StringRef;
class SNESMCExpr : public MCTargetExpr {
public:
  enum VariantKind {
    VK_SNES_None,
    VK_SNES_LO,
    VK_SNES_HI,
    VK_SNES_H44,
    VK_SNES_M44,
    VK_SNES_L44,
    VK_SNES_HH,
    VK_SNES_HM,
    VK_SNES_LM,
    VK_SNES_PC22,
    VK_SNES_PC10,
    VK_SNES_GOT22,
    VK_SNES_GOT10,
    VK_SNES_GOT13,
    VK_SNES_13,
    VK_SNES_WPLT30,
    VK_SNES_WDISP30,
    VK_SNES_R_DISP32,
    VK_SNES_TLS_GD_HI22,
    VK_SNES_TLS_GD_LO10,
    VK_SNES_TLS_GD_ADD,
    VK_SNES_TLS_GD_CALL,
    VK_SNES_TLS_LDM_HI22,
    VK_SNES_TLS_LDM_LO10,
    VK_SNES_TLS_LDM_ADD,
    VK_SNES_TLS_LDM_CALL,
    VK_SNES_TLS_LDO_HIX22,
    VK_SNES_TLS_LDO_LOX10,
    VK_SNES_TLS_LDO_ADD,
    VK_SNES_TLS_IE_HI22,
    VK_SNES_TLS_IE_LO10,
    VK_SNES_TLS_IE_LD,
    VK_SNES_TLS_IE_LDX,
    VK_SNES_TLS_IE_ADD,
    VK_SNES_TLS_LE_HIX22,
    VK_SNES_TLS_LE_LOX10,
    VK_SNES_HIX22,
    VK_SNES_LOX10,
    VK_SNES_GOTDATA_HIX22,
    VK_SNES_GOTDATA_LOX10,
    VK_SNES_GOTDATA_OP,
  };

private:
  const VariantKind Kind;
  const MCExpr *Expr;

  explicit SNESMCExpr(VariantKind Kind, const MCExpr *Expr)
      : Kind(Kind), Expr(Expr) {}

public:
  /// @name Construction
  /// @{

  static const SNESMCExpr *create(VariantKind Kind, const MCExpr *Expr,
                                  MCContext &Ctx);
  /// @}
  /// @name Accessors
  /// @{

  /// getOpcode - Get the kind of this expression.
  VariantKind getKind() const { return Kind; }

  /// getSubExpr - Get the child of this expression.
  const MCExpr *getSubExpr() const { return Expr; }

  /// getFixupKind - Get the fixup kind of this expression.
  SNES::Fixups getFixupKind() const { return getFixupKind(Kind); }

  /// @}
  void printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const override;
  bool evaluateAsRelocatableImpl(MCValue &Res, const MCAsmLayout *Layout,
                                 const MCFixup *Fixup) const override;
  void visitUsedExpr(MCStreamer &Streamer) const override;
  MCFragment *findAssociatedFragment() const override {
    return getSubExpr()->findAssociatedFragment();
  }

  void fixELFSymbolsInTLSFixups(MCAssembler &Asm) const override;

  static bool classof(const MCExpr *E) {
    return E->getKind() == MCExpr::Target;
  }

  static bool classof(const SNESMCExpr *) { return true; }

  static VariantKind parseVariantKind(StringRef name);
  static bool printVariantKind(raw_ostream &OS, VariantKind Kind);
  static SNES::Fixups getFixupKind(VariantKind Kind);
};

} // end namespace llvm.

#endif
