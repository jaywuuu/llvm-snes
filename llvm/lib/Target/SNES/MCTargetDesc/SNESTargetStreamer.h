//===-- SNESTargetStreamer.h - SNES Target Streamer ----------*- C++ -*--===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESTARGETSTREAMER_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESTARGETSTREAMER_H

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCStreamer.h"

namespace llvm {

class formatted_raw_ostream;

class SNESTargetStreamer : public MCTargetStreamer {
  virtual void anchor();

public:
  SNESTargetStreamer(MCStreamer &S);
  /// Emit ".register <reg>, #ignore".
  virtual void emitSNESRegisterIgnore(unsigned reg) = 0;
  /// Emit ".register <reg>, #scratch".
  virtual void emitSNESRegisterScratch(unsigned reg) = 0;
};

// This part is for ascii assembly output
class SNESTargetAsmStreamer : public SNESTargetStreamer {
  formatted_raw_ostream &OS;

public:
  SNESTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);
  void emitSNESRegisterIgnore(unsigned reg) override;
  void emitSNESRegisterScratch(unsigned reg) override;
};

// This part is for ELF object output
class SNESTargetELFStreamer : public SNESTargetStreamer {
public:
  SNESTargetELFStreamer(MCStreamer &S);
  MCELFStreamer &getStreamer();
  void emitSNESRegisterIgnore(unsigned reg) override {}
  void emitSNESRegisterScratch(unsigned reg) override {}
};
} // end namespace llvm

#endif
