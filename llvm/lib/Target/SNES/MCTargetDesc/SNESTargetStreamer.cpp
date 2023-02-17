//===-- SNESTargetStreamer.cpp - SNES Target Streamer Methods -----------===//
//===----------------------------------------------------------------------===//
//
// This file provides SNES specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "SNESInstPrinter.h"
#include "SNESTargetStreamer.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
SNESTargetStreamer::SNESTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

void SNESTargetStreamer::anchor() {}

SNESTargetAsmStreamer::SNESTargetAsmStreamer(MCStreamer &S,
                                             formatted_raw_ostream &OS)
    : SNESTargetStreamer(S), OS(OS) {}

void SNESTargetAsmStreamer::emitSNESRegisterIgnore(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(SNESInstPrinter::getRegisterName(reg)).lower()
     << ", #ignore\n";
}

void SNESTargetAsmStreamer::emitSNESRegisterScratch(unsigned reg) {
  OS << "\t.register "
     << "%" << StringRef(SNESInstPrinter::getRegisterName(reg)).lower()
     << ", #scratch\n";
}

SNESTargetELFStreamer::SNESTargetELFStreamer(MCStreamer &S)
    : SNESTargetStreamer(S) {}

MCELFStreamer &SNESTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(Streamer);
}
