//===-- SNESInstrFormats.td - SNES Instruction Formats ---*- tablegen -*-===//

class InstSNES<bits<2> opVal, dag outs, dag ins, string asmstr, list<dag> pattern>
   : Instruction {
  field bits<32> Inst;
  bits<2> op;

  let op = opVal;

  let Inst{31-30} = op;               // Top two bits are the 'op' field

  let Namespace = "SNES";
  let Size = 4;
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = asmstr;
  let Pattern = pattern;

  let DecoderNamespace = "SNES";
  field bits<32> SoftFail = 0;

}

