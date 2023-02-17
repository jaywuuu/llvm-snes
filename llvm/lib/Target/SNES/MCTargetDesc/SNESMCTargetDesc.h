//===-- SNESMCTargetDesc.h - SNES Target Descriptions ---------*- C++ -*-===//
//===----------------------------------------------------------------------===//
//
// This file provides SNES specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCTARGETDESC_H
#define LLVM_LIB_TARGET_SNES_MCTARGETDESC_SNESMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {
class MCAsmBackend;
class MCCodeEmitter;
class MCContext;
class MCInstrInfo;
class MCObjectTargetWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class MCTargetOptions;
class Target;

MCCodeEmitter *createSNESMCCodeEmitter(const MCInstrInfo &MCII, MCContext &Ctx);
MCAsmBackend *createSNESAsmBackend(const Target &T, const MCSubtargetInfo &STI,
                                   const MCRegisterInfo &MRI,
                                   const MCTargetOptions &Options);
std::unique_ptr<MCObjectTargetWriter> createSNESELFObjectWriter(bool Is64Bit,
                                                                uint8_t OSABI);
} // namespace llvm

// Defines symbolic names for SNES registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "SNESGenRegisterInfo.inc"

// Defines symbolic names for the SNES instructions.
//
#define GET_INSTRINFO_ENUM
#define GET_INSTRINFO_MC_HELPER_DECLS
#include "SNESGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "SNESGenSubtargetInfo.inc"

#endif
