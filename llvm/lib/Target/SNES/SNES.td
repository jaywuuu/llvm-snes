//===-- SNES.td - Describe the SNES Target Machine -------*- tablegen -*-===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// SNES Subtarget features.
//

def FeatureSoftMulDiv
  : SubtargetFeature<"soft-mul-div", "UseSoftMulDiv", "true",
                     "Use software emulation for integer multiply and divide">;

def FeatureNoFSMULD
  : SubtargetFeature<"no-fsmuld", "HasNoFSMULD", "true",
                     "Disable the fsmuld instruction.">;
def FeatureNoFMULS
  : SubtargetFeature<"no-fmuls", "HasNoFMULS", "true",
                     "Disable the fmuls instruction.">;

def FeatureV9
  : SubtargetFeature<"v9", "IsV9", "true",
                     "Enable SNES-V9 instructions">;
def FeatureV8Deprecated
  : SubtargetFeature<"deprecated-v8", "V8DeprecatedInsts", "true",
                     "Enable deprecated V8 instructions in V9 mode">;
def FeatureVIS
  : SubtargetFeature<"vis", "IsVIS", "true",
                     "Enable UltraSNES Visual Instruction Set extensions">;
def FeatureVIS2
  : SubtargetFeature<"vis2", "IsVIS2", "true",
                     "Enable Visual Instruction Set extensions II">;
def FeatureVIS3
  : SubtargetFeature<"vis3", "IsVIS3", "true",
                     "Enable Visual Instruction Set extensions III">;
def FeaturePWRPSR
  : SubtargetFeature<"leonpwrpsr", "HasPWRPSR", "true",
                     "Enable the PWRPSR instruction">;

def FeatureHardQuad
  : SubtargetFeature<"hard-quad-float", "HasHardQuad", "true",
                     "Enable quad-word floating point instructions">;

def UsePopc : SubtargetFeature<"popc", "UsePopc", "true",
                               "Use the popc (population count) instruction">;

def FeatureSoftFloat : SubtargetFeature<"soft-float", "UseSoftFloat", "true",
                              "Use software emulation for floating point">;


//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "SNESRegisterInfo.td"
include "SNESCallingConv.td"
include "SNESSchedule.td"
include "SNESInstrInfo.td"

def SNESInstrInfo : InstrInfo;

def SNESAsmParser : AsmParser {
  bit ShouldEmitMatchRegisterName = 0;
}

//===----------------------------------------------------------------------===//
// SNES processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic",         []>;
def : Proc<"v7",              [FeatureSoftMulDiv, FeatureNoFSMULD]>;
def : Proc<"v8",              []>;
def : Proc<"superSNES",      []>;
def : Proc<"SNESlite",       []>;
def : Proc<"f934",            []>;
def : Proc<"hyperSNES",      []>;
def : Proc<"SNESlite86x",    []>;
def : Proc<"SNESlet",        []>;
def : Proc<"tsc701",          []>;
def : Proc<"v9",              [FeatureV9]>;
def : Proc<"ultraSNES",      [FeatureV9, FeatureV8Deprecated, FeatureVIS]>;
def : Proc<"ultraSNES3",     [FeatureV9, FeatureV8Deprecated, FeatureVIS,
                               FeatureVIS2]>;
def : Proc<"niagara",         [FeatureV9, FeatureV8Deprecated, FeatureVIS,
                               FeatureVIS2]>;
def : Proc<"niagara2",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2]>;
def : Proc<"niagara3",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2]>;
def : Proc<"niagara4",        [FeatureV9, FeatureV8Deprecated, UsePopc,
                               FeatureVIS, FeatureVIS2, FeatureVIS3]>;

//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def SNESAsmWriter : AsmWriter {
  string AsmWriterClassName  = "InstPrinter";
  int PassSubtarget = 1;
  int Variant = 0;
}

def SNES : Target {
  // Pull in Instruction Info:
  let InstructionSet = SNESInstrInfo;
  let AssemblyParsers  = [SNESAsmParser];
  let AssemblyWriters = [SNESAsmWriter];
  let AllowRegisterRenaming = 1;
}
