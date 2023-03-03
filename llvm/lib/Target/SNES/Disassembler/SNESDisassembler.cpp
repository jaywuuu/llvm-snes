//===- SNESDisassembler.cpp - Disassembler for SNES -----------*- C++ -*-===//
//===----------------------------------------------------------------------===//
//
// This file is part of the SNES Disassembler.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/SNESMCTargetDesc.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDecoderOps.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "SNES-disassembler"

typedef MCDisassembler::DecodeStatus DecodeStatus;

namespace {

/// A disassembler class for SNES.
class SNESDisassembler : public MCDisassembler {
public:
  SNESDisassembler(const MCSubtargetInfo &STI, MCContext &Ctx)
      : MCDisassembler(STI, Ctx) {}
  virtual ~SNESDisassembler() = default;

  DecodeStatus getInstruction(MCInst &Instr, uint64_t &Size,
                              ArrayRef<uint8_t> Bytes, uint64_t Address,
                              raw_ostream &CStream) const override;
};
} // namespace

static MCDisassembler *createSNESDisassembler(const Target &T,
                                              const MCSubtargetInfo &STI,
                                              MCContext &Ctx) {
  return new SNESDisassembler(STI, Ctx);
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESDisassembler() {
  // Register the disassembler.
  TargetRegistry::RegisterMCDisassembler(getTheSNESTarget(),
                                         createSNESDisassembler);
}

DecodeStatus SNESDisassembler::getInstruction(MCInst &Instr, uint64_t &Size,
                                              ArrayRef<uint8_t> Bytes,
                                              uint64_t Address,
                                              raw_ostream &CStream) const {
  DecodeStatus Result = MCDisassembler::Success;
  if (Result == MCDisassembler::Fail)
    return MCDisassembler::Fail;

  if (Result != MCDisassembler::Fail) {
    Size = 4;
    return Result;
  }

  return MCDisassembler::Fail;
}

