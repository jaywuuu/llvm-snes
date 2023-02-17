//===-- SNESAsmParser.cpp - Parse SNES assembly to MCInst instructions --===//

#include "MCTargetDesc/SNESMCExpr.h"
#include "MCTargetDesc/SNESMCTargetDesc.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>

using namespace llvm;

// The generated AsmMatcher SNESGenAsmMatcher uses "SNES" as the target
// namespace. But SNES backend uses "SP" as its namespace.
namespace llvm {
namespace SNES {

using namespace SP;

} // end namespace SNES
} // end namespace llvm

namespace {

class SNESOperand;

class SNESAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;

  enum class TailRelocKind { Load_GOT, Add_TLS, Load_TLS, Call_TLS };

  /// @name Auto-generated Match Functions
  /// {

#define GET_ASSEMBLER_HEADER
#include "SNESGenAsmMatcher.inc"

  /// }

  // public interface of the MCTargetAsmParser.
  bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               OperandVector &Operands, MCStreamer &Out,
                               uint64_t &ErrorInfo,
                               bool MatchingInlineAsm) override;
  bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) override;
  OperandMatchResultTy tryParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                        SMLoc &EndLoc) override;
  bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                        SMLoc NameLoc, OperandVector &Operands) override;
  bool ParseDirective(AsmToken DirectiveID) override;

  unsigned validateTargetOperandClass(MCParsedAsmOperand &Op,
                                      unsigned Kind) override;

  // Custom parse functions for SNES specific operands.
  OperandMatchResultTy parseMEMOperand(OperandVector &Operands);

  OperandMatchResultTy parseMembarTag(OperandVector &Operands);

  template <TailRelocKind Kind>
  OperandMatchResultTy parseTailRelocSym(OperandVector &Operands);

  template <unsigned N>
  OperandMatchResultTy parseShiftAmtImm(OperandVector &Operands);

  OperandMatchResultTy parseCallTarget(OperandVector &Operands);

  OperandMatchResultTy parseOperand(OperandVector &Operands, StringRef Name);

  OperandMatchResultTy
  parseSNESAsmOperand(std::unique_ptr<SNESOperand> &Operand,
                      bool isCall = false);

  OperandMatchResultTy parseBranchModifiers(OperandVector &Operands);

  // Helper function for dealing with %lo / %hi in PIC mode.
  const SNESMCExpr *adjustPICRelocation(SNESMCExpr::VariantKind VK,
                                        const MCExpr *subExpr);

  // returns true if Tok is matched to a register and returns register in RegNo.
  bool matchRegisterName(const AsmToken &Tok, unsigned &RegNo,
                         unsigned &RegKind);

  bool matchSNESAsmModifiers(const MCExpr *&EVal, SMLoc &EndLoc);

  bool is64Bit() const {
    return false;
  }

  bool expandSET(MCInst &Inst, SMLoc IDLoc,
                 SmallVectorImpl<MCInst> &Instructions);

  SMLoc getLoc() const { return getParser().getTok().getLoc(); }

public:
  SNESAsmParser(const MCSubtargetInfo &sti, MCAsmParser &parser,
                const MCInstrInfo &MII, const MCTargetOptions &Options)
      : MCTargetAsmParser(Options, sti, MII), Parser(parser) {
    Parser.addAliasForDirective(".half", ".2byte");
    Parser.addAliasForDirective(".uahalf", ".2byte");
    Parser.addAliasForDirective(".word", ".4byte");
    Parser.addAliasForDirective(".uaword", ".4byte");
    Parser.addAliasForDirective(".nword", is64Bit() ? ".8byte" : ".4byte");
    if (is64Bit())
      Parser.addAliasForDirective(".xword", ".8byte");

    // Initialize the set of available features.
    setAvailableFeatures(ComputeAvailableFeatures(getSTI().getFeatureBits()));
  }
};

} // end anonymous namespace

static const MCPhysReg IntRegs[32] = {
    SNES::G0, SNES::G1, SNES::G2, SNES::G3, SNES::G4, SNES::G5, SNES::G6,
    SNES::G7, SNES::O0, SNES::O1, SNES::O2, SNES::O3, SNES::O4, SNES::O5,
    SNES::O6, SNES::O7, SNES::L0, SNES::L1, SNES::L2, SNES::L3, SNES::L4,
    SNES::L5, SNES::L6, SNES::L7, SNES::I0, SNES::I1, SNES::I2, SNES::I3,
    SNES::I4, SNES::I5, SNES::I6, SNES::I7};

static const MCPhysReg FloatRegs[32] = {
    SNES::F0,  SNES::F1,  SNES::F2,  SNES::F3,  SNES::F4,  SNES::F5,  SNES::F6,
    SNES::F7,  SNES::F8,  SNES::F9,  SNES::F10, SNES::F11, SNES::F12, SNES::F13,
    SNES::F14, SNES::F15, SNES::F16, SNES::F17, SNES::F18, SNES::F19, SNES::F20,
    SNES::F21, SNES::F22, SNES::F23, SNES::F24, SNES::F25, SNES::F26, SNES::F27,
    SNES::F28, SNES::F29, SNES::F30, SNES::F31};

static const MCPhysReg DoubleRegs[32] = {
    SNES::D0,  SNES::D1,  SNES::D2,  SNES::D3,  SNES::D4,  SNES::D5,  SNES::D6,
    SNES::D7,  SNES::D8,  SNES::D9,  SNES::D10, SNES::D11, SNES::D12, SNES::D13,
    SNES::D14, SNES::D15, SNES::D16, SNES::D17, SNES::D18, SNES::D19, SNES::D20,
    SNES::D21, SNES::D22, SNES::D23, SNES::D24, SNES::D25, SNES::D26, SNES::D27,
    SNES::D28, SNES::D29, SNES::D30, SNES::D31};

static const MCPhysReg QuadFPRegs[32] = {
    SNES::Q0,  SNES::Q1,  SNES::Q2,  SNES::Q3, SNES::Q4,  SNES::Q5,
    SNES::Q6,  SNES::Q7,  SNES::Q8,  SNES::Q9, SNES::Q10, SNES::Q11,
    SNES::Q12, SNES::Q13, SNES::Q14, SNES::Q15};

static const MCPhysReg ASRRegs[32] = {
    SP::Y,     SP::ASR1,  SP::ASR2,  SP::ASR3,  SP::ASR4,  SP::ASR5,  SP::ASR6,
    SP::ASR7,  SP::ASR8,  SP::ASR9,  SP::ASR10, SP::ASR11, SP::ASR12, SP::ASR13,
    SP::ASR14, SP::ASR15, SP::ASR16, SP::ASR17, SP::ASR18, SP::ASR19, SP::ASR20,
    SP::ASR21, SP::ASR22, SP::ASR23, SP::ASR24, SP::ASR25, SP::ASR26, SP::ASR27,
    SP::ASR28, SP::ASR29, SP::ASR30, SP::ASR31};

static const MCPhysReg IntPairRegs[] = {
    SNES::G0_G1, SNES::G2_G3, SNES::G4_G5, SNES::G6_G7,
    SNES::O0_O1, SNES::O2_O3, SNES::O4_O5, SNES::O6_O7,
    SNES::L0_L1, SNES::L2_L3, SNES::L4_L5, SNES::L6_L7,
    SNES::I0_I1, SNES::I2_I3, SNES::I4_I5, SNES::I6_I7};

static const MCPhysReg CoprocRegs[32] = {
    SNES::C0,  SNES::C1,  SNES::C2,  SNES::C3,  SNES::C4,  SNES::C5,  SNES::C6,
    SNES::C7,  SNES::C8,  SNES::C9,  SNES::C10, SNES::C11, SNES::C12, SNES::C13,
    SNES::C14, SNES::C15, SNES::C16, SNES::C17, SNES::C18, SNES::C19, SNES::C20,
    SNES::C21, SNES::C22, SNES::C23, SNES::C24, SNES::C25, SNES::C26, SNES::C27,
    SNES::C28, SNES::C29, SNES::C30, SNES::C31};

static const MCPhysReg CoprocPairRegs[] = {
    SNES::C0_C1,   SNES::C2_C3,   SNES::C4_C5,   SNES::C6_C7,
    SNES::C8_C9,   SNES::C10_C11, SNES::C12_C13, SNES::C14_C15,
    SNES::C16_C17, SNES::C18_C19, SNES::C20_C21, SNES::C22_C23,
    SNES::C24_C25, SNES::C26_C27, SNES::C28_C29, SNES::C30_C31};

namespace {

/// SNESOperand - Instances of this class represent a parsed SNES machine
/// instruction.
class SNESOperand : public MCParsedAsmOperand {
public:
  enum RegisterKind {
    rk_None,
    rk_IntReg,
    rk_IntPairReg,
    rk_FloatReg,
    rk_DoubleReg,
    rk_QuadReg,
    rk_CoprocReg,
    rk_CoprocPairReg,
    rk_Special,
  };

private:
  enum KindTy {
    k_Token,
    k_Register,
    k_Immediate,
    k_MemoryReg,
    k_MemoryImm
  } Kind;

  SMLoc StartLoc, EndLoc;

  struct Token {
    const char *Data;
    unsigned Length;
  };

  struct RegOp {
    unsigned RegNum;
    RegisterKind Kind;
  };

  struct ImmOp {
    const MCExpr *Val;
  };

  struct MemOp {
    unsigned Base;
    unsigned OffsetReg;
    const MCExpr *Off;
  };

  union {
    struct Token Tok;
    struct RegOp Reg;
    struct ImmOp Imm;
    struct MemOp Mem;
  };

public:
  SNESOperand(KindTy K) : Kind(K) {}

  bool isToken() const override { return Kind == k_Token; }
  bool isReg() const override { return Kind == k_Register; }
  bool isImm() const override { return Kind == k_Immediate; }
  bool isMem() const override { return isMEMrr() || isMEMri(); }
  bool isMEMrr() const { return Kind == k_MemoryReg; }
  bool isMEMri() const { return Kind == k_MemoryImm; }
  bool isMembarTag() const { return Kind == k_Immediate; }
  bool isTailRelocSym() const { return Kind == k_Immediate; }

  bool isCallTarget() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return CE->getValue() % 4 == 0;

    return true;
  }

  bool isShiftAmtImm5() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<5>(CE->getValue());

    return false;
  }

  bool isShiftAmtImm6() const {
    if (!isImm())
      return false;

    if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Imm.Val))
      return isUInt<6>(CE->getValue());

    return false;
  }

  bool isIntReg() const {
    return (Kind == k_Register && Reg.Kind == rk_IntReg);
  }

  bool isFloatReg() const {
    return (Kind == k_Register && Reg.Kind == rk_FloatReg);
  }

  bool isFloatOrDoubleReg() const {
    return (Kind == k_Register &&
            (Reg.Kind == rk_FloatReg || Reg.Kind == rk_DoubleReg));
  }

  bool isCoprocReg() const {
    return (Kind == k_Register && Reg.Kind == rk_CoprocReg);
  }

  StringRef getToken() const {
    assert(Kind == k_Token && "Invalid access!");
    return StringRef(Tok.Data, Tok.Length);
  }

  unsigned getReg() const override {
    assert((Kind == k_Register) && "Invalid access!");
    return Reg.RegNum;
  }

  const MCExpr *getImm() const {
    assert((Kind == k_Immediate) && "Invalid access!");
    return Imm.Val;
  }

  unsigned getMemBase() const {
    assert((Kind == k_MemoryReg || Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Base;
  }

  unsigned getMemOffsetReg() const {
    assert((Kind == k_MemoryReg) && "Invalid access!");
    return Mem.OffsetReg;
  }

  const MCExpr *getMemOff() const {
    assert((Kind == k_MemoryImm) && "Invalid access!");
    return Mem.Off;
  }

  /// getStartLoc - Get the location of the first token of this operand.
  SMLoc getStartLoc() const override { return StartLoc; }
  /// getEndLoc - Get the location of the last token of this operand.
  SMLoc getEndLoc() const override { return EndLoc; }

  void print(raw_ostream &OS) const override {
    switch (Kind) {
    case k_Token:
      OS << "Token: " << getToken() << "\n";
      break;
    case k_Register:
      OS << "Reg: #" << getReg() << "\n";
      break;
    case k_Immediate:
      OS << "Imm: " << getImm() << "\n";
      break;
    case k_MemoryReg:
      OS << "Mem: " << getMemBase() << "+" << getMemOffsetReg() << "\n";
      break;
    case k_MemoryImm:
      assert(getMemOff() != nullptr);
      OS << "Mem: " << getMemBase() << "+" << *getMemOff() << "\n";
      break;
    }
  }

  void addRegOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    Inst.addOperand(MCOperand::createReg(getReg()));
  }

  void addImmOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addShiftAmtImm5Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }
  void addShiftAmtImm6Operands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addExpr(MCInst &Inst, const MCExpr *Expr) const {
    // Add as immediate when possible.  Null MCExpr = 0.
    if (!Expr)
      Inst.addOperand(MCOperand::createImm(0));
    else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
      Inst.addOperand(MCOperand::createImm(CE->getValue()));
    else
      Inst.addOperand(MCOperand::createExpr(Expr));
  }

  void addMEMrrOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    assert(getMemOffsetReg() != 0 && "Invalid offset");
    Inst.addOperand(MCOperand::createReg(getMemOffsetReg()));
  }

  void addMEMriOperands(MCInst &Inst, unsigned N) const {
    assert(N == 2 && "Invalid number of operands!");

    Inst.addOperand(MCOperand::createReg(getMemBase()));

    const MCExpr *Expr = getMemOff();
    addExpr(Inst, Expr);
  }

  void addMembarTagOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    const MCExpr *Expr = getImm();
    addExpr(Inst, Expr);
  }

  void addCallTargetOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  void addTailRelocSymOperands(MCInst &Inst, unsigned N) const {
    assert(N == 1 && "Invalid number of operands!");
    addExpr(Inst, getImm());
  }

  static std::unique_ptr<SNESOperand> CreateToken(StringRef Str, SMLoc S) {
    auto Op = std::make_unique<SNESOperand>(k_Token);
    Op->Tok.Data = Str.data();
    Op->Tok.Length = Str.size();
    Op->StartLoc = S;
    Op->EndLoc = S;
    return Op;
  }

  static std::unique_ptr<SNESOperand> CreateReg(unsigned RegNum, unsigned Kind,
                                                SMLoc S, SMLoc E) {
    auto Op = std::make_unique<SNESOperand>(k_Register);
    Op->Reg.RegNum = RegNum;
    Op->Reg.Kind = (SNESOperand::RegisterKind)Kind;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<SNESOperand> CreateImm(const MCExpr *Val, SMLoc S,
                                                SMLoc E) {
    auto Op = std::make_unique<SNESOperand>(k_Immediate);
    Op->Imm.Val = Val;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static bool MorphToIntPairReg(SNESOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_IntReg);
    unsigned regIdx = 32;
    if (Reg >= SNES::G0 && Reg <= SNES::G7)
      regIdx = Reg - SNES::G0;
    else if (Reg >= SNES::O0 && Reg <= SNES::O7)
      regIdx = Reg - SNES::O0 + 8;
    else if (Reg >= SNES::L0 && Reg <= SNES::L7)
      regIdx = Reg - SNES::L0 + 16;
    else if (Reg >= SNES::I0 && Reg <= SNES::I7)
      regIdx = Reg - SNES::I0 + 24;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = IntPairRegs[regIdx / 2];
    Op.Reg.Kind = rk_IntPairReg;
    return true;
  }

  static bool MorphToDoubleReg(SNESOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_FloatReg);
    unsigned regIdx = Reg - SNES::F0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = DoubleRegs[regIdx / 2];
    Op.Reg.Kind = rk_DoubleReg;
    return true;
  }

  static bool MorphToQuadReg(SNESOperand &Op) {
    unsigned Reg = Op.getReg();
    unsigned regIdx = 0;
    switch (Op.Reg.Kind) {
    default:
      llvm_unreachable("Unexpected register kind!");
    case rk_FloatReg:
      regIdx = Reg - SNES::F0;
      if (regIdx % 4 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 4];
      break;
    case rk_DoubleReg:
      regIdx = Reg - SNES::D0;
      if (regIdx % 2 || regIdx > 31)
        return false;
      Reg = QuadFPRegs[regIdx / 2];
      break;
    }
    Op.Reg.RegNum = Reg;
    Op.Reg.Kind = rk_QuadReg;
    return true;
  }

  static bool MorphToCoprocPairReg(SNESOperand &Op) {
    unsigned Reg = Op.getReg();
    assert(Op.Reg.Kind == rk_CoprocReg);
    unsigned regIdx = 32;
    if (Reg >= SNES::C0 && Reg <= SNES::C31)
      regIdx = Reg - SNES::C0;
    if (regIdx % 2 || regIdx > 31)
      return false;
    Op.Reg.RegNum = CoprocPairRegs[regIdx / 2];
    Op.Reg.Kind = rk_CoprocPairReg;
    return true;
  }

  static std::unique_ptr<SNESOperand>
  MorphToMEMrr(unsigned Base, std::unique_ptr<SNESOperand> Op) {
    unsigned offsetReg = Op->getReg();
    Op->Kind = k_MemoryReg;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = offsetReg;
    Op->Mem.Off = nullptr;
    return Op;
  }

  static std::unique_ptr<SNESOperand> CreateMEMr(unsigned Base, SMLoc S,
                                                 SMLoc E) {
    auto Op = std::make_unique<SNESOperand>(k_MemoryReg);
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = SNES::G0; // always 0
    Op->Mem.Off = nullptr;
    Op->StartLoc = S;
    Op->EndLoc = E;
    return Op;
  }

  static std::unique_ptr<SNESOperand>
  MorphToMEMri(unsigned Base, std::unique_ptr<SNESOperand> Op) {
    const MCExpr *Imm = Op->getImm();
    Op->Kind = k_MemoryImm;
    Op->Mem.Base = Base;
    Op->Mem.OffsetReg = 0;
    Op->Mem.Off = Imm;
    return Op;
  }
};

} // end anonymous namespace

bool SNESAsmParser::expandSET(MCInst &Inst, SMLoc IDLoc,
                              SmallVectorImpl<MCInst> &Instructions) {
  MCOperand MCRegOp = Inst.getOperand(0);
  MCOperand MCValOp = Inst.getOperand(1);
  assert(MCRegOp.isReg());
  assert(MCValOp.isImm() || MCValOp.isExpr());

  // the imm operand can be either an expression or an immediate.
  bool IsImm = Inst.getOperand(1).isImm();
  int64_t RawImmValue = IsImm ? MCValOp.getImm() : 0;

  // Allow either a signed or unsigned 32-bit immediate.
  if (RawImmValue < -2147483648LL || RawImmValue > 4294967295LL) {
    return Error(IDLoc,
                 "set: argument must be between -2147483648 and 4294967295");
  }

  // If the value was expressed as a large unsigned number, that's ok.
  // We want to see if it "looks like" a small signed number.
  int32_t ImmValue = RawImmValue;
  // For 'set' you can't use 'or' with a negative operand on V9 because
  // that would splat the sign bit across the upper half of the destination
  // register, whereas 'set' is defined to zero the high 32 bits.
  bool IsEffectivelyImm13 =
      IsImm && ((is64Bit() ? 0 : -4096) <= ImmValue && ImmValue < 4096);
  const MCExpr *ValExpr;
  if (IsImm)
    ValExpr = MCConstantExpr::create(ImmValue, getContext());
  else
    ValExpr = MCValOp.getExpr();

  MCOperand PrevReg = MCOperand::createReg(SNES::G0);

  // If not just a signed imm13 value, then either we use a 'sethi' with a
  // following 'or', or a 'sethi' by itself if there are no more 1 bits.
  // In either case, start with the 'sethi'.
  if (!IsEffectivelyImm13) {
    MCInst TmpInst;
    const MCExpr *Expr = adjustPICRelocation(SNESMCExpr::VK_SNES_HI, ValExpr);
    TmpInst.setLoc(IDLoc);
    TmpInst.setOpcode(SP::SETHIi);
    TmpInst.addOperand(MCRegOp);
    TmpInst.addOperand(MCOperand::createExpr(Expr));
    Instructions.push_back(TmpInst);
    PrevReg = MCRegOp;
  }

  // The low bits require touching in 3 cases:
  // * A non-immediate value will always require both instructions.
  // * An effectively imm13 value needs only an 'or' instruction.
  // * Otherwise, an immediate that is not effectively imm13 requires the
  //   'or' only if bits remain after clearing the 22 bits that 'sethi' set.
  // If the low bits are known zeros, there's nothing to do.
  // In the second case, and only in that case, must we NOT clear
  // bits of the immediate value via the %lo() assembler function.
  // Note also, the 'or' instruction doesn't mind a large value in the case
  // where the operand to 'set' was 0xFFFFFzzz - it does exactly what you mean.
  if (!IsImm || IsEffectivelyImm13 || (ImmValue & 0x3ff)) {
    MCInst TmpInst;
    const MCExpr *Expr;
    if (IsEffectivelyImm13)
      Expr = ValExpr;
    else
      Expr = adjustPICRelocation(SNESMCExpr::VK_SNES_LO, ValExpr);
    TmpInst.setLoc(IDLoc);
    TmpInst.setOpcode(SP::ORri);
    TmpInst.addOperand(MCRegOp);
    TmpInst.addOperand(PrevReg);
    TmpInst.addOperand(MCOperand::createExpr(Expr));
    Instructions.push_back(TmpInst);
  }
  return false;
}

bool SNESAsmParser::MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                                            OperandVector &Operands,
                                            MCStreamer &Out,
                                            uint64_t &ErrorInfo,
                                            bool MatchingInlineAsm) {
  MCInst Inst;
  SmallVector<MCInst, 8> Instructions;
  unsigned MatchResult =
      MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm);
  switch (MatchResult) {
  case Match_Success: {
    switch (Inst.getOpcode()) {
    default:
      Inst.setLoc(IDLoc);
      Instructions.push_back(Inst);
      break;
    case SP::SET:
      if (expandSET(Inst, IDLoc, Instructions))
        return true;
      break;
    }

    for (const MCInst &I : Instructions) {
      Out.emitInstruction(I, getSTI());
    }
    return false;
  }

  case Match_MissingFeature:
    return Error(IDLoc,
                 "instruction requires a CPU feature not currently enabled");

  case Match_InvalidOperand: {
    SMLoc ErrorLoc = IDLoc;
    if (ErrorInfo != ~0ULL) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((SNESOperand &)*Operands[ErrorInfo]).getStartLoc();
      if (ErrorLoc == SMLoc())
        ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }
  case Match_MnemonicFail:
    return Error(IDLoc, "invalid instruction mnemonic");
  }
  llvm_unreachable("Implement any new match types added!");
}

bool SNESAsmParser::ParseRegister(unsigned &RegNo, SMLoc &StartLoc,
                                  SMLoc &EndLoc) {
  if (tryParseRegister(RegNo, StartLoc, EndLoc) != MatchOperand_Success)
    return Error(StartLoc, "invalid register name");
  return false;
}

OperandMatchResultTy SNESAsmParser::tryParseRegister(unsigned &RegNo,
                                                     SMLoc &StartLoc,
                                                     SMLoc &EndLoc) {
  const AsmToken &Tok = Parser.getTok();
  StartLoc = Tok.getLoc();
  EndLoc = Tok.getEndLoc();
  RegNo = 0;
  if (getLexer().getKind() != AsmToken::Percent)
    return MatchOperand_NoMatch;
  Parser.Lex();
  unsigned regKind = SNESOperand::rk_None;
  if (matchRegisterName(Tok, RegNo, regKind)) {
    Parser.Lex();
    return MatchOperand_Success;
  }

  getLexer().UnLex(Tok);
  return MatchOperand_NoMatch;
}

static void applyMnemonicAliases(StringRef &Mnemonic,
                                 const FeatureBitset &Features,
                                 unsigned VariantID);

bool SNESAsmParser::ParseInstruction(ParseInstructionInfo &Info, StringRef Name,
                                     SMLoc NameLoc, OperandVector &Operands) {

  // First operand in MCInst is instruction mnemonic.
  Operands.push_back(SNESOperand::CreateToken(Name, NameLoc));

  // apply mnemonic aliases, if any, so that we can parse operands correctly.
  applyMnemonicAliases(Name, getAvailableFeatures(), 0);

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    // Read the first operand.
    if (getLexer().is(AsmToken::Comma)) {
      if (parseBranchModifiers(Operands) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
    if (parseOperand(Operands, Name) != MatchOperand_Success) {
      SMLoc Loc = getLexer().getLoc();
      return Error(Loc, "unexpected token");
    }

    while (getLexer().is(AsmToken::Comma) || getLexer().is(AsmToken::Plus)) {
      if (getLexer().is(AsmToken::Plus)) {
        // Plus tokens are significant in software_traps (p83, SNESv8.pdf). We
        // must capture them.
        Operands.push_back(
            SNESOperand::CreateToken("+", Parser.getTok().getLoc()));
      }
      Parser.Lex(); // Eat the comma or plus.
      // Parse and remember the operand.
      if (parseOperand(Operands, Name) != MatchOperand_Success) {
        SMLoc Loc = getLexer().getLoc();
        return Error(Loc, "unexpected token");
      }
    }
  }
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    SMLoc Loc = getLexer().getLoc();
    return Error(Loc, "unexpected token");
  }
  Parser.Lex(); // Consume the EndOfStatement.
  return false;
}

bool SNESAsmParser::ParseDirective(AsmToken DirectiveID) {
  StringRef IDVal = DirectiveID.getString();

  if (IDVal == ".register") {
    // For now, ignore .register directive.
    Parser.eatToEndOfStatement();
    return false;
  }
  if (IDVal == ".proc") {
    // For compatibility, ignore this directive.
    // (It's supposed to be an "optimization" in the Sun assembler)
    Parser.eatToEndOfStatement();
    return false;
  }

  // Let the MC layer to handle other directives.
  return true;
}

OperandMatchResultTy SNESAsmParser::parseMEMOperand(OperandVector &Operands) {
  SMLoc S, E;

  std::unique_ptr<SNESOperand> LHS;
  if (parseSNESAsmOperand(LHS) != MatchOperand_Success)
    return MatchOperand_NoMatch;

  // Single immediate operand
  if (LHS->isImm()) {
    Operands.push_back(SNESOperand::MorphToMEMri(SNES::G0, std::move(LHS)));
    return MatchOperand_Success;
  }

  if (!LHS->isIntReg()) {
    Error(LHS->getStartLoc(), "invalid register kind for this operand");
    return MatchOperand_ParseFail;
  }

  AsmToken Tok = getLexer().getTok();
  // The plus token may be followed by a register or an immediate value, the
  // minus one is always interpreted as sign for the immediate value
  if (Tok.is(AsmToken::Plus) || Tok.is(AsmToken::Minus)) {
    (void)Parser.parseOptionalToken(AsmToken::Plus);

    std::unique_ptr<SNESOperand> RHS;
    if (parseSNESAsmOperand(RHS) != MatchOperand_Success)
      return MatchOperand_NoMatch;

    if (RHS->isReg() && !RHS->isIntReg()) {
      Error(RHS->getStartLoc(), "invalid register kind for this operand");
      return MatchOperand_ParseFail;
    }

    Operands.push_back(
        RHS->isImm()
            ? SNESOperand::MorphToMEMri(LHS->getReg(), std::move(RHS))
            : SNESOperand::MorphToMEMrr(LHS->getReg(), std::move(RHS)));

    return MatchOperand_Success;
  }

  Operands.push_back(SNESOperand::CreateMEMr(LHS->getReg(), S, E));
  return MatchOperand_Success;
}

template <unsigned N>
OperandMatchResultTy SNESAsmParser::parseShiftAmtImm(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  // This is a register, not an immediate
  if (getLexer().getKind() == AsmToken::Percent)
    return MatchOperand_NoMatch;

  const MCExpr *Expr;
  if (getParser().parseExpression(Expr))
    return MatchOperand_ParseFail;

  const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr);
  if (!CE) {
    Error(S, "constant expression expected");
    return MatchOperand_ParseFail;
  }

  if (!isUInt<N>(CE->getValue())) {
    Error(S, "immediate shift value out of range");
    return MatchOperand_ParseFail;
  }

  Operands.push_back(SNESOperand::CreateImm(Expr, S, E));
  return MatchOperand_Success;
}

template <SNESAsmParser::TailRelocKind Kind>
OperandMatchResultTy SNESAsmParser::parseTailRelocSym(OperandVector &Operands) {
  SMLoc S = getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  auto MatchesKind = [](SNESMCExpr::VariantKind VK) -> bool {
    switch (Kind) {
    case TailRelocKind::Load_GOT:
      // Non-TLS relocations on ld (or ldx).
      // ld [%rr + %rr], %rr, %rel(sym)
      return VK == SNESMCExpr::VK_SNES_GOTDATA_OP;
    case TailRelocKind::Add_TLS:
      // TLS relocations on add.
      // add %rr, %rr, %rr, %rel(sym)
      switch (VK) {
      case SNESMCExpr::VK_SNES_TLS_GD_ADD:
      case SNESMCExpr::VK_SNES_TLS_IE_ADD:
      case SNESMCExpr::VK_SNES_TLS_LDM_ADD:
      case SNESMCExpr::VK_SNES_TLS_LDO_ADD:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Load_TLS:
      // TLS relocations on ld (or ldx).
      // ld[x] %addr, %rr, %rel(sym)
      switch (VK) {
      case SNESMCExpr::VK_SNES_TLS_IE_LD:
      case SNESMCExpr::VK_SNES_TLS_IE_LDX:
        return true;
      default:
        return false;
      }
    case TailRelocKind::Call_TLS:
      // TLS relocations on call.
      // call sym, %rel(sym)
      switch (VK) {
      case SNESMCExpr::VK_SNES_TLS_GD_CALL:
      case SNESMCExpr::VK_SNES_TLS_LDM_CALL:
        return true;
      default:
        return false;
      }
    }
    llvm_unreachable("Unhandled SNESAsmParser::TailRelocKind enum");
  };

  if (getLexer().getKind() != AsmToken::Percent) {
    Error(getLoc(), "expected '%' for operand modifier");
    return MatchOperand_ParseFail;
  }

  const AsmToken Tok = Parser.getTok();
  getParser().Lex(); // Eat '%'

  if (getLexer().getKind() != AsmToken::Identifier) {
    Error(getLoc(), "expected valid identifier for operand modifier");
    return MatchOperand_ParseFail;
  }

  StringRef Name = getParser().getTok().getIdentifier();
  SNESMCExpr::VariantKind VK = SNESMCExpr::parseVariantKind(Name);
  if (VK == SNESMCExpr::VK_SNES_None) {
    Error(getLoc(), "invalid operand modifier");
    return MatchOperand_ParseFail;
  }

  if (!MatchesKind(VK)) {
    // Did not match the specified set of relocation types, put '%' back.
    getLexer().UnLex(Tok);
    return MatchOperand_NoMatch;
  }

  Parser.Lex(); // Eat the identifier.
  if (getLexer().getKind() != AsmToken::LParen) {
    Error(getLoc(), "expected '('");
    return MatchOperand_ParseFail;
  }

  getParser().Lex(); // Eat '('
  const MCExpr *SubExpr;
  if (getParser().parseParenExpression(SubExpr, E)) {
    return MatchOperand_ParseFail;
  }

  const MCExpr *Val = adjustPICRelocation(VK, SubExpr);
  Operands.push_back(SNESOperand::CreateImm(Val, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy SNESAsmParser::parseMembarTag(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  const MCExpr *EVal;
  int64_t ImmVal = 0;

  std::unique_ptr<SNESOperand> Mask;
  if (parseSNESAsmOperand(Mask) == MatchOperand_Success) {
    if (!Mask->isImm() || !Mask->getImm()->evaluateAsAbsolute(ImmVal) ||
        ImmVal < 0 || ImmVal > 127) {
      Error(S, "invalid membar mask number");
      return MatchOperand_ParseFail;
    }
  }

  while (getLexer().getKind() == AsmToken::Hash) {
    SMLoc TagStart = getLexer().getLoc();
    Parser.Lex(); // Eat the '#'.
    unsigned MaskVal = StringSwitch<unsigned>(Parser.getTok().getString())
                           .Case("LoadLoad", 0x1)
                           .Case("StoreLoad", 0x2)
                           .Case("LoadStore", 0x4)
                           .Case("StoreStore", 0x8)
                           .Case("Lookaside", 0x10)
                           .Case("MemIssue", 0x20)
                           .Case("Sync", 0x40)
                           .Default(0);

    Parser.Lex(); // Eat the identifier token.

    if (!MaskVal) {
      Error(TagStart, "unknown membar tag");
      return MatchOperand_ParseFail;
    }

    ImmVal |= MaskVal;

    if (getLexer().getKind() == AsmToken::Pipe)
      Parser.Lex(); // Eat the '|'.
  }

  EVal = MCConstantExpr::create(ImmVal, getContext());
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  Operands.push_back(SNESOperand::CreateImm(EVal, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy SNESAsmParser::parseCallTarget(OperandVector &Operands) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(S.getPointer() - 1);

  switch (getLexer().getKind()) {
  default:
    return MatchOperand_NoMatch;
  case AsmToken::LParen:
  case AsmToken::Integer:
  case AsmToken::Identifier:
  case AsmToken::Dot:
    break;
  }

  const MCExpr *DestValue;
  if (getParser().parseExpression(DestValue))
    return MatchOperand_NoMatch;

  bool IsPic = getContext().getObjectFileInfo()->isPositionIndependent();
  SNESMCExpr::VariantKind Kind =
      IsPic ? SNESMCExpr::VK_SNES_WPLT30 : SNESMCExpr::VK_SNES_WDISP30;

  const MCExpr *DestExpr = SNESMCExpr::create(Kind, DestValue, getContext());
  Operands.push_back(SNESOperand::CreateImm(DestExpr, S, E));
  return MatchOperand_Success;
}

OperandMatchResultTy SNESAsmParser::parseOperand(OperandVector &Operands,
                                                 StringRef Mnemonic) {

  OperandMatchResultTy ResTy = MatchOperandParserImpl(Operands, Mnemonic);

  // If there wasn't a custom match, try the generic matcher below. Otherwise,
  // there was a match, but an error occurred, in which case, just return that
  // the operand parsing failed.
  if (ResTy == MatchOperand_Success || ResTy == MatchOperand_ParseFail)
    return ResTy;

  if (getLexer().is(AsmToken::LBrac)) {
    // Memory operand
    Operands.push_back(SNESOperand::CreateToken("[", Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the [

    if (Mnemonic == "cas" || Mnemonic == "casx" || Mnemonic == "casa") {
      SMLoc S = Parser.getTok().getLoc();
      if (getLexer().getKind() != AsmToken::Percent)
        return MatchOperand_NoMatch;
      Parser.Lex(); // eat %

      unsigned RegNo, RegKind;
      if (!matchRegisterName(Parser.getTok(), RegNo, RegKind))
        return MatchOperand_NoMatch;

      Parser.Lex(); // Eat the identifier token.
      SMLoc E =
          SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Operands.push_back(SNESOperand::CreateReg(RegNo, RegKind, S, E));
      ResTy = MatchOperand_Success;
    } else {
      ResTy = parseMEMOperand(Operands);
    }

    if (ResTy != MatchOperand_Success)
      return ResTy;

    if (!getLexer().is(AsmToken::RBrac))
      return MatchOperand_ParseFail;

    Operands.push_back(SNESOperand::CreateToken("]", Parser.getTok().getLoc()));
    Parser.Lex(); // Eat the ]

    // Parse an optional address-space identifier after the address.
    if (getLexer().is(AsmToken::Integer)) {
      std::unique_ptr<SNESOperand> Op;
      ResTy = parseSNESAsmOperand(Op, false);
      if (ResTy != MatchOperand_Success || !Op)
        return MatchOperand_ParseFail;
      Operands.push_back(std::move(Op));
    }
    return MatchOperand_Success;
  }

  std::unique_ptr<SNESOperand> Op;

  ResTy = parseSNESAsmOperand(Op, (Mnemonic == "call"));
  if (ResTy != MatchOperand_Success || !Op)
    return MatchOperand_ParseFail;

  // Push the parsed operand into the list of operands
  Operands.push_back(std::move(Op));

  return MatchOperand_Success;
}

OperandMatchResultTy
SNESAsmParser::parseSNESAsmOperand(std::unique_ptr<SNESOperand> &Op,
                                   bool isCall) {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
  const MCExpr *EVal;

  Op = nullptr;
  switch (getLexer().getKind()) {
  default:
    break;

  case AsmToken::Percent:
    Parser.Lex(); // Eat the '%'.
    unsigned RegNo;
    unsigned RegKind;
    if (matchRegisterName(Parser.getTok(), RegNo, RegKind)) {
      StringRef name = Parser.getTok().getString();
      Parser.Lex(); // Eat the identifier token.
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      switch (RegNo) {
      default:
        Op = SNESOperand::CreateReg(RegNo, RegKind, S, E);
        break;
      case SNES::PSR:
        Op = SNESOperand::CreateToken("%psr", S);
        break;
      case SNES::FSR:
        Op = SNESOperand::CreateToken("%fsr", S);
        break;
      case SNES::FQ:
        Op = SNESOperand::CreateToken("%fq", S);
        break;
      case SNES::CPSR:
        Op = SNESOperand::CreateToken("%csr", S);
        break;
      case SNES::CPQ:
        Op = SNESOperand::CreateToken("%cq", S);
        break;
      case SNES::WIM:
        Op = SNESOperand::CreateToken("%wim", S);
        break;
      case SNES::TBR:
        Op = SNESOperand::CreateToken("%tbr", S);
        break;
      case SNES::PC:
        Op = SNESOperand::CreateToken("%pc", S);
        break;
      case SNES::ICC:
        if (name == "xcc")
          Op = SNESOperand::CreateToken("%xcc", S);
        else
          Op = SNESOperand::CreateToken("%icc", S);
        break;
      }
      break;
    }
    if (matchSNESAsmModifiers(EVal, E)) {
      E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);
      Op = SNESOperand::CreateImm(EVal, S, E);
    }
    break;

  case AsmToken::Plus:
  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::LParen:
  case AsmToken::Dot:
  case AsmToken::Identifier:
    if (getParser().parseExpression(EVal, E))
      break;

    int64_t Res;
    if (!EVal->evaluateAsAbsolute(Res)) {
      SNESMCExpr::VariantKind Kind = SNESMCExpr::VK_SNES_13;

      if (getContext().getObjectFileInfo()->isPositionIndependent()) {
        if (isCall)
          Kind = SNESMCExpr::VK_SNES_WPLT30;
        else
          Kind = SNESMCExpr::VK_SNES_GOT13;
      }
      EVal = SNESMCExpr::create(Kind, EVal, getContext());
    }
    Op = SNESOperand::CreateImm(EVal, S, E);
    break;
  }
  return (Op) ? MatchOperand_Success : MatchOperand_ParseFail;
}

OperandMatchResultTy
SNESAsmParser::parseBranchModifiers(OperandVector &Operands) {
  // parse (,a|,pn|,pt)+

  while (getLexer().is(AsmToken::Comma)) {
    Parser.Lex(); // Eat the comma

    if (!getLexer().is(AsmToken::Identifier))
      return MatchOperand_ParseFail;
    StringRef modName = Parser.getTok().getString();
    if (modName == "a" || modName == "pn" || modName == "pt") {
      Operands.push_back(
          SNESOperand::CreateToken(modName, Parser.getTok().getLoc()));
      Parser.Lex(); // eat the identifier.
    }
  }
  return MatchOperand_Success;
}

bool SNESAsmParser::matchRegisterName(const AsmToken &Tok, unsigned &RegNo,
                                      unsigned &RegKind) {
  int64_t intVal = 0;
  RegNo = 0;
  RegKind = SNESOperand::rk_None;
  if (Tok.is(AsmToken::Identifier)) {
    StringRef name = Tok.getString();

    // %fp
    if (name.equals("fp")) {
      RegNo = SNES::I6;
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }
    // %sp
    if (name.equals("sp")) {
      RegNo = SNES::O6;
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }

    if (name.equals("y")) {
      RegNo = SNES::Y;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.substr(0, 3).equals_insensitive("asr") &&
        !name.substr(3).getAsInteger(10, intVal) && intVal > 0 && intVal < 32) {
      RegNo = ASRRegs[intVal];
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    // %fprs is an alias of %asr6.
    if (name.equals("fprs")) {
      RegNo = ASRRegs[6];
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("icc")) {
      RegNo = SNES::ICC;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("psr")) {
      RegNo = SNES::PSR;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("fsr")) {
      RegNo = SNES::FSR;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("fq")) {
      RegNo = SNES::FQ;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("csr")) {
      RegNo = SNES::CPSR;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("cq")) {
      RegNo = SNES::CPQ;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("wim")) {
      RegNo = SNES::WIM;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("tbr")) {
      RegNo = SNES::TBR;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    if (name.equals("xcc")) {
      // FIXME:: check 64bit.
      RegNo = SNES::ICC;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    // %fcc0 - %fcc3
    if (name.substr(0, 3).equals_insensitive("fcc") &&
        !name.substr(3).getAsInteger(10, intVal) && intVal < 4) {
      // FIXME: check 64bit and  handle %fcc1 - %fcc3
      RegNo = SNES::FCC0 + intVal;
      RegKind = SNESOperand::rk_Special;
      return true;
    }

    // %g0 - %g7
    if (name.substr(0, 1).equals_insensitive("g") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
      RegNo = IntRegs[intVal];
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }
    // %o0 - %o7
    if (name.substr(0, 1).equals_insensitive("o") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
      RegNo = IntRegs[8 + intVal];
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }
    if (name.substr(0, 1).equals_insensitive("l") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
      RegNo = IntRegs[16 + intVal];
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }
    if (name.substr(0, 1).equals_insensitive("i") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 8) {
      RegNo = IntRegs[24 + intVal];
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }
    // %f0 - %f31
    if (name.substr(0, 1).equals_insensitive("f") &&
        !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 32) {
      RegNo = FloatRegs[intVal];
      RegKind = SNESOperand::rk_FloatReg;
      return true;
    }
    // %f32 - %f62
    if (name.substr(0, 1).equals_insensitive("f") &&
        !name.substr(1, 2).getAsInteger(10, intVal) && intVal >= 32 &&
        intVal <= 62 && (intVal % 2 == 0)) {
      // FIXME: Check V9
      RegNo = DoubleRegs[intVal / 2];
      RegKind = SNESOperand::rk_DoubleReg;
      return true;
    }

    // %r0 - %r31
    if (name.substr(0, 1).equals_insensitive("r") &&
        !name.substr(1, 2).getAsInteger(10, intVal) && intVal < 31) {
      RegNo = IntRegs[intVal];
      RegKind = SNESOperand::rk_IntReg;
      return true;
    }

    // %c0 - %c31
    if (name.substr(0, 1).equals_insensitive("c") &&
        !name.substr(1).getAsInteger(10, intVal) && intVal < 32) {
      RegNo = CoprocRegs[intVal];
      RegKind = SNESOperand::rk_CoprocReg;
      return true;
    }

    if (name.equals("tpc")) {
      RegNo = SNES::TPC;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tnpc")) {
      RegNo = SNES::TNPC;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tstate")) {
      RegNo = SNES::TSTATE;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tt")) {
      RegNo = SNES::TT;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tick")) {
      RegNo = SNES::TICK;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tba")) {
      RegNo = SNES::TBA;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("pstate")) {
      RegNo = SNES::PSTATE;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("tl")) {
      RegNo = SNES::TL;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("pil")) {
      RegNo = SNES::PIL;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("cwp")) {
      RegNo = SNES::CWP;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("cansave")) {
      RegNo = SNES::CANSAVE;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("canrestore")) {
      RegNo = SNES::CANRESTORE;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("cleanwin")) {
      RegNo = SNES::CLEANWIN;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("otherwin")) {
      RegNo = SNES::OTHERWIN;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("wstate")) {
      RegNo = SNES::WSTATE;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
    if (name.equals("pc")) {
      RegNo = SNES::PC;
      RegKind = SNESOperand::rk_Special;
      return true;
    }
  }
  return false;
}

// Determine if an expression contains a reference to the symbol
// "_GLOBAL_OFFSET_TABLE_".
static bool hasGOTReference(const MCExpr *Expr) {
  switch (Expr->getKind()) {
  case MCExpr::Target:
    if (const SNESMCExpr *SE = dyn_cast<SNESMCExpr>(Expr))
      return hasGOTReference(SE->getSubExpr());
    break;

  case MCExpr::Constant:
    break;

  case MCExpr::Binary: {
    const MCBinaryExpr *BE = cast<MCBinaryExpr>(Expr);
    return hasGOTReference(BE->getLHS()) || hasGOTReference(BE->getRHS());
  }

  case MCExpr::SymbolRef: {
    const MCSymbolRefExpr &SymRef = *cast<MCSymbolRefExpr>(Expr);
    return (SymRef.getSymbol().getName() == "_GLOBAL_OFFSET_TABLE_");
  }

  case MCExpr::Unary:
    return hasGOTReference(cast<MCUnaryExpr>(Expr)->getSubExpr());
  }
  return false;
}

const SNESMCExpr *SNESAsmParser::adjustPICRelocation(SNESMCExpr::VariantKind VK,
                                                     const MCExpr *subExpr) {
  // When in PIC mode, "%lo(...)" and "%hi(...)" behave differently.
  // If the expression refers contains _GLOBAL_OFFSET_TABLE, it is
  // actually a %pc10 or %pc22 relocation. Otherwise, they are interpreted
  // as %got10 or %got22 relocation.

  if (getContext().getObjectFileInfo()->isPositionIndependent()) {
    switch (VK) {
    default:
      break;
    case SNESMCExpr::VK_SNES_LO:
      VK = (hasGOTReference(subExpr) ? SNESMCExpr::VK_SNES_PC10
                                     : SNESMCExpr::VK_SNES_GOT10);
      break;
    case SNESMCExpr::VK_SNES_HI:
      VK = (hasGOTReference(subExpr) ? SNESMCExpr::VK_SNES_PC22
                                     : SNESMCExpr::VK_SNES_GOT22);
      break;
    }
  }

  return SNESMCExpr::create(VK, subExpr, getContext());
}

bool SNESAsmParser::matchSNESAsmModifiers(const MCExpr *&EVal, SMLoc &EndLoc) {
  AsmToken Tok = Parser.getTok();
  if (!Tok.is(AsmToken::Identifier))
    return false;

  StringRef name = Tok.getString();

  SNESMCExpr::VariantKind VK = SNESMCExpr::parseVariantKind(name);
  switch (VK) {
  case SNESMCExpr::VK_SNES_None:
    Error(getLoc(), "invalid operand modifier");
    return false;

  case SNESMCExpr::VK_SNES_GOTDATA_OP:
  case SNESMCExpr::VK_SNES_TLS_GD_ADD:
  case SNESMCExpr::VK_SNES_TLS_GD_CALL:
  case SNESMCExpr::VK_SNES_TLS_IE_ADD:
  case SNESMCExpr::VK_SNES_TLS_IE_LD:
  case SNESMCExpr::VK_SNES_TLS_IE_LDX:
  case SNESMCExpr::VK_SNES_TLS_LDM_ADD:
  case SNESMCExpr::VK_SNES_TLS_LDM_CALL:
  case SNESMCExpr::VK_SNES_TLS_LDO_ADD:
    // These are special-cased at tablegen level.
    return false;

  default:
    break;
  }

  Parser.Lex(); // Eat the identifier.
  if (Parser.getTok().getKind() != AsmToken::LParen)
    return false;

  Parser.Lex(); // Eat the LParen token.
  const MCExpr *subExpr;
  if (Parser.parseParenExpression(subExpr, EndLoc))
    return false;

  EVal = adjustPICRelocation(VK, subExpr);
  return true;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESAsmParser() {
  RegisterMCAsmParser<SNESAsmParser> A(getTheSNESTarget());
}

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "SNESGenAsmMatcher.inc"

unsigned SNESAsmParser::validateTargetOperandClass(MCParsedAsmOperand &GOp,
                                                   unsigned Kind) {
  SNESOperand &Op = (SNESOperand &)GOp;
  if (Op.isFloatOrDoubleReg()) {
    switch (Kind) {
    default:
      break;
    case MCK_DFPRegs:
      if (!Op.isFloatReg() || SNESOperand::MorphToDoubleReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    case MCK_QFPRegs:
      if (SNESOperand::MorphToQuadReg(Op))
        return MCTargetAsmParser::Match_Success;
      break;
    }
  }
  if (Op.isIntReg() && Kind == MCK_IntPair) {
    if (SNESOperand::MorphToIntPairReg(Op))
      return MCTargetAsmParser::Match_Success;
  }
  if (Op.isCoprocReg() && Kind == MCK_CoprocPair) {
    if (SNESOperand::MorphToCoprocPairReg(Op))
      return MCTargetAsmParser::Match_Success;
  }
  return Match_InvalidOperand;
}
