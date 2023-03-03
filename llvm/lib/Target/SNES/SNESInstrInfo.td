//===-- SNESInstrInfo.td - Target Description for SNES Target -----------===//

include "SNESInstrFormats.td"

def dummy : InstSNES<1, (outs), (ins), "Dummy", []>;