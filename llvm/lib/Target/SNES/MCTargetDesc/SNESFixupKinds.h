//===-- SNESFixupKinds.h - SNES Specific Fixup Entries --------*- C++ -*-===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESFIXUPKINDS_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESFIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace SNES {
enum Fixups {
  // fixup_SNES_call30 - 30-bit PC relative relocation for call
  fixup_SNES_call30 = FirstTargetFixupKind,

  /// fixup_SNES_br22 - 22-bit PC relative relocation for
  /// branches
  fixup_SNES_br22,

  /// fixup_SNES_br19 - 19-bit PC relative relocation for
  /// branches on icc/xcc
  fixup_SNES_br19,

  /// fixup_SNES_bpr  - 16-bit fixup for bpr
  fixup_SNES_br16_2,
  fixup_SNES_br16_14,

  /// fixup_SNES_13 - 13-bit fixup
  fixup_SNES_13,

  /// fixup_SNES_hi22  - 22-bit fixup corresponding to %hi(foo)
  /// for sethi
  fixup_SNES_hi22,

  /// fixup_SNES_lo10  - 10-bit fixup corresponding to %lo(foo)
  fixup_SNES_lo10,

  /// fixup_SNES_h44  - 22-bit fixup corresponding to %h44(foo)
  fixup_SNES_h44,

  /// fixup_SNES_m44  - 10-bit fixup corresponding to %m44(foo)
  fixup_SNES_m44,

  /// fixup_SNES_l44  - 12-bit fixup corresponding to %l44(foo)
  fixup_SNES_l44,

  /// fixup_SNES_hh  -  22-bit fixup corresponding to %hh(foo)
  fixup_SNES_hh,

  /// fixup_SNES_hm  -  10-bit fixup corresponding to %hm(foo)
  fixup_SNES_hm,

  /// fixup_SNES_lm  -  22-bit fixup corresponding to %lm(foo)
  fixup_SNES_lm,

  /// fixup_SNES_pc22 - 22-bit fixup corresponding to %pc22(foo)
  fixup_SNES_pc22,

  /// fixup_SNES_pc10 - 10-bit fixup corresponding to %pc10(foo)
  fixup_SNES_pc10,

  /// fixup_SNES_got22 - 22-bit fixup corresponding to %got22(foo)
  fixup_SNES_got22,

  /// fixup_SNES_got10 - 10-bit fixup corresponding to %got10(foo)
  fixup_SNES_got10,

  /// fixup_SNES_got13 - 13-bit fixup corresponding to %got13(foo)
  fixup_SNES_got13,

  /// fixup_SNES_wplt30
  fixup_SNES_wplt30,

  /// fixups for Thread Local Storage
  fixup_SNES_tls_gd_hi22,
  fixup_SNES_tls_gd_lo10,
  fixup_SNES_tls_gd_add,
  fixup_SNES_tls_gd_call,
  fixup_SNES_tls_ldm_hi22,
  fixup_SNES_tls_ldm_lo10,
  fixup_SNES_tls_ldm_add,
  fixup_SNES_tls_ldm_call,
  fixup_SNES_tls_ldo_hix22,
  fixup_SNES_tls_ldo_lox10,
  fixup_SNES_tls_ldo_add,
  fixup_SNES_tls_ie_hi22,
  fixup_SNES_tls_ie_lo10,
  fixup_SNES_tls_ie_ld,
  fixup_SNES_tls_ie_ldx,
  fixup_SNES_tls_ie_add,
  fixup_SNES_tls_le_hix22,
  fixup_SNES_tls_le_lox10,

  /// 22-bit fixup corresponding to %hix(foo)
  fixup_SNES_hix22,
  /// 13-bit fixup corresponding to %lox(foo)
  fixup_SNES_lox10,

  /// 22-bit fixup corresponding to %gdop_hix22(foo)
  fixup_SNES_gotdata_hix22,
  /// 13-bit fixup corresponding to %gdop_lox10(foo)
  fixup_SNES_gotdata_lox10,
  /// 32-bit fixup corresponding to %gdop(foo)
  fixup_SNES_gotdata_op,

  // Marker
  LastTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};
}
} // namespace llvm

#endif
