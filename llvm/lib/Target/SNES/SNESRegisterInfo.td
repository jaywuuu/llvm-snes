//===-- SNESRegisterInfo.td - SNES Register defs ---------*- tablegen -*-===//

class SNESReg<bits<16> Enc, string n> : Register<n> {
  let HWEncoding = Enc;
  let Namespace = "SNES";
}

def A : SNESReg<0, "A">, DwarfRegNum<[0]>;

def IntRegs : RegisterClass<"SNES", [i16, v2i8], 16, (add A)>;