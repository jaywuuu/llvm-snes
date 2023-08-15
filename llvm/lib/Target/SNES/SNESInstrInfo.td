//===-- SNESInstrInfo.td - Target Description for SNES Target -----------===//

include "SNESInstrFormats.td"

def dummy_operand : Operand<i32> {
}

def dummy : InstSNES<1, (outs), (ins dummy_operand:$addr, IntRegs:$src),
                 "Dummy $src, [$addr]", [(store i32:$src, i32:$addr)]>;

//def dummy : SNESPseudoInst<(outs), (ins), "dummy", []>;

