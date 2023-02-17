//===-- SNESAsmPrinter.cpp - SNES LLVM assembly writer ------------------===//

#include "MCTargetDesc/SNESInstPrinter.h"
#include "MCTargetDesc/SNESMCExpr.h"
#include "MCTargetDesc/SNESTargetStreamer.h"
#include "SNES.h"
#include "SNESInstrInfo.h"
#include "SNESTargetMachine.h"
#include "TargetInfo/SNESTargetInfo.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
class SNESAsmPrinter : public AsmPrinter {
  SNESTargetStreamer &getTargetStreamer() {
    return static_cast<SNESTargetStreamer &>(*OutStreamer->getTargetStreamer());
  }

public:
  explicit SNESAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)) {}

  StringRef getPassName() const override { return "SNES Assembly Printer"; }

  void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
  void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                       const char *Modifier = nullptr);

  void emitFunctionBodyStart() override;
  void emitInstruction(const MachineInstr *MI) override;

  static const char *getRegisterName(unsigned RegNo) {
    return SNESInstPrinter::getRegisterName(RegNo);
  }

  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                       const char *ExtraCode, raw_ostream &O) override;
  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                             const char *ExtraCode, raw_ostream &O) override;

  void LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                 const MCSubtargetInfo &STI);
};
} // end of anonymous namespace

static MCOperand createSNESMCOperand(SNESMCExpr::VariantKind Kind,
                                     MCSymbol *Sym, MCContext &OutContext) {
  const MCSymbolRefExpr *MCSym = MCSymbolRefExpr::create(Sym, OutContext);
  const SNESMCExpr *expr = SNESMCExpr::create(Kind, MCSym, OutContext);
  return MCOperand::createExpr(expr);
}
static MCOperand createPCXCallOP(MCSymbol *Label, MCContext &OutContext) {
  return createSNESMCOperand(SNESMCExpr::VK_SNES_WDISP30, Label, OutContext);
}

static MCOperand createPCXRelExprOp(SNESMCExpr::VariantKind Kind,
                                    MCSymbol *GOTLabel, MCSymbol *StartLabel,
                                    MCSymbol *CurLabel, MCContext &OutContext) {
  const MCSymbolRefExpr *GOT = MCSymbolRefExpr::create(GOTLabel, OutContext);
  const MCSymbolRefExpr *Start =
      MCSymbolRefExpr::create(StartLabel, OutContext);
  const MCSymbolRefExpr *Cur = MCSymbolRefExpr::create(CurLabel, OutContext);

  const MCBinaryExpr *Sub = MCBinaryExpr::createSub(Cur, Start, OutContext);
  const MCBinaryExpr *Add = MCBinaryExpr::createAdd(GOT, Sub, OutContext);
  const SNESMCExpr *expr = SNESMCExpr::create(Kind, Add, OutContext);
  return MCOperand::createExpr(expr);
}

static void EmitCall(MCStreamer &OutStreamer, MCOperand &Callee,
                     const MCSubtargetInfo &STI) {
  MCInst CallInst;
  CallInst.setOpcode(SP::CALL);
  CallInst.addOperand(Callee);
  OutStreamer.emitInstruction(CallInst, STI);
}

static void EmitSETHI(MCStreamer &OutStreamer, MCOperand &Imm, MCOperand &RD,
                      const MCSubtargetInfo &STI) {
  MCInst SETHIInst;
  SETHIInst.setOpcode(SP::SETHIi);
  SETHIInst.addOperand(RD);
  SETHIInst.addOperand(Imm);
  OutStreamer.emitInstruction(SETHIInst, STI);
}

static void EmitBinary(MCStreamer &OutStreamer, unsigned Opcode, MCOperand &RS1,
                       MCOperand &Src2, MCOperand &RD,
                       const MCSubtargetInfo &STI) {
  MCInst Inst;
  Inst.setOpcode(Opcode);
  Inst.addOperand(RD);
  Inst.addOperand(RS1);
  Inst.addOperand(Src2);
  OutStreamer.emitInstruction(Inst, STI);
}

static void EmitOR(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &Imm,
                   MCOperand &RD, const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::ORri, RS1, Imm, RD, STI);
}

static void EmitADD(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &RS2,
                    MCOperand &RD, const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::ADDrr, RS1, RS2, RD, STI);
}

static void EmitSHL(MCStreamer &OutStreamer, MCOperand &RS1, MCOperand &Imm,
                    MCOperand &RD, const MCSubtargetInfo &STI) {
  EmitBinary(OutStreamer, SP::SLLri, RS1, Imm, RD, STI);
}

static void EmitHiLo(MCStreamer &OutStreamer, MCSymbol *GOTSym,
                     SNESMCExpr::VariantKind HiKind,
                     SNESMCExpr::VariantKind LoKind, MCOperand &RD,
                     MCContext &OutContext, const MCSubtargetInfo &STI) {

  MCOperand hi = createSNESMCOperand(HiKind, GOTSym, OutContext);
  MCOperand lo = createSNESMCOperand(LoKind, GOTSym, OutContext);
  EmitSETHI(OutStreamer, hi, RD, STI);
  EmitOR(OutStreamer, RD, lo, RD, STI);
}

void SNESAsmPrinter::LowerGETPCXAndEmitMCInsts(const MachineInstr *MI,
                                               const MCSubtargetInfo &STI) {
  MCSymbol *GOTLabel =
      OutContext.getOrCreateSymbol(Twine("_GLOBAL_OFFSET_TABLE_"));

  const MachineOperand &MO = MI->getOperand(0);
  assert(MO.getReg() != SP::O7 && "%o7 is assigned as destination for getpcx!");

  MCOperand MCRegOP = MCOperand::createReg(MO.getReg());

  if (!isPositionIndependent()) {
    // Just load the address of GOT to MCRegOP.
    switch (TM.getCodeModel()) {
    default:
      llvm_unreachable("Unsupported absolute code model");
    case CodeModel::Small:
      EmitHiLo(*OutStreamer, GOTLabel, SNESMCExpr::VK_SNES_HI,
               SNESMCExpr::VK_SNES_LO, MCRegOP, OutContext, STI);
      break;
    case CodeModel::Medium: {
      EmitHiLo(*OutStreamer, GOTLabel, SNESMCExpr::VK_SNES_H44,
               SNESMCExpr::VK_SNES_M44, MCRegOP, OutContext, STI);
      MCOperand imm =
          MCOperand::createExpr(MCConstantExpr::create(12, OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      MCOperand lo =
          createSNESMCOperand(SNESMCExpr::VK_SNES_L44, GOTLabel, OutContext);
      EmitOR(*OutStreamer, MCRegOP, lo, MCRegOP, STI);
      break;
    }
    case CodeModel::Large: {
      EmitHiLo(*OutStreamer, GOTLabel, SNESMCExpr::VK_SNES_HH,
               SNESMCExpr::VK_SNES_HM, MCRegOP, OutContext, STI);
      MCOperand imm =
          MCOperand::createExpr(MCConstantExpr::create(32, OutContext));
      EmitSHL(*OutStreamer, MCRegOP, imm, MCRegOP, STI);
      // Use register %o7 to load the lower 32 bits.
      MCOperand RegO7 = MCOperand::createReg(SP::O7);
      EmitHiLo(*OutStreamer, GOTLabel, SNESMCExpr::VK_SNES_HI,
               SNESMCExpr::VK_SNES_LO, RegO7, OutContext, STI);
      EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
    }
    }
    return;
  }

  MCSymbol *StartLabel = OutContext.createTempSymbol();
  MCSymbol *EndLabel = OutContext.createTempSymbol();
  MCSymbol *SethiLabel = OutContext.createTempSymbol();

  MCOperand RegO7 = MCOperand::createReg(SP::O7);

  // <StartLabel>:
  //   call <EndLabel>
  // <SethiLabel>:
  //     sethi %hi(_GLOBAL_OFFSET_TABLE_+(<SethiLabel>-<StartLabel>)), <MO>
  // <EndLabel>:
  //   or  <MO>, %lo(_GLOBAL_OFFSET_TABLE_+(<EndLabel>-<StartLabel>))), <MO>
  //   add <MO>, %o7, <MO>

  OutStreamer->emitLabel(StartLabel);
  MCOperand Callee = createPCXCallOP(EndLabel, OutContext);
  EmitCall(*OutStreamer, Callee, STI);
  OutStreamer->emitLabel(SethiLabel);
  MCOperand hiImm = createPCXRelExprOp(SNESMCExpr::VK_SNES_PC22, GOTLabel,
                                       StartLabel, SethiLabel, OutContext);
  EmitSETHI(*OutStreamer, hiImm, MCRegOP, STI);
  OutStreamer->emitLabel(EndLabel);
  MCOperand loImm = createPCXRelExprOp(SNESMCExpr::VK_SNES_PC10, GOTLabel,
                                       StartLabel, EndLabel, OutContext);
  EmitOR(*OutStreamer, MCRegOP, loImm, MCRegOP, STI);
  EmitADD(*OutStreamer, MCRegOP, RegO7, MCRegOP, STI);
}

void SNESAsmPrinter::emitInstruction(const MachineInstr *MI) {
  SNES_MC::verifyInstructionPredicates(MI->getOpcode(),
                                       getSubtargetInfo().getFeatureBits());

  switch (MI->getOpcode()) {
  default:
    break;
  case TargetOpcode::DBG_VALUE:
    // FIXME: Debug Value.
    return;
  case SP::GETPCX:
    LowerGETPCXAndEmitMCInsts(MI, getSubtargetInfo());
    return;
  }
  MachineBasicBlock::const_instr_iterator I = MI->getIterator();
  MachineBasicBlock::const_instr_iterator E = MI->getParent()->instr_end();
  do {
    MCInst TmpInst;
    LowerSNESMachineInstrToMCInst(&*I, TmpInst, *this);
    EmitToStreamer(*OutStreamer, TmpInst);
  } while ((++I != E) && I->isInsideBundle()); // Delay slot check.
}

void SNESAsmPrinter::emitFunctionBodyStart() {
  if (!MF->getSubtarget<SNESSubtarget>().is64Bit())
    return;

  const MachineRegisterInfo &MRI = MF->getRegInfo();
  const unsigned globalRegs[] = {SP::G2, SP::G3, SP::G6, SP::G7, 0};
  for (unsigned i = 0; globalRegs[i] != 0; ++i) {
    unsigned reg = globalRegs[i];
    if (MRI.use_empty(reg))
      continue;

    if (reg == SP::G6 || reg == SP::G7)
      getTargetStreamer().emitSNESRegisterIgnore(reg);
    else
      getTargetStreamer().emitSNESRegisterScratch(reg);
  }
}

void SNESAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                  raw_ostream &O) {
  const DataLayout &DL = getDataLayout();
  const MachineOperand &MO = MI->getOperand(opNum);
  SNESMCExpr::VariantKind TF = (SNESMCExpr::VariantKind)MO.getTargetFlags();

#ifndef NDEBUG
  // Verify the target flags.
  if (MO.isGlobal() || MO.isSymbol() || MO.isCPI()) {
    if (MI->getOpcode() == SP::CALL)
      assert(TF == SNESMCExpr::VK_SNES_None &&
             "Cannot handle target flags on call address");
    else if (MI->getOpcode() == SP::SETHIi || MI->getOpcode() == SP::SETHIXi)
      assert((TF == SNESMCExpr::VK_SNES_HI || TF == SNESMCExpr::VK_SNES_H44 ||
              TF == SNESMCExpr::VK_SNES_HH || TF == SNESMCExpr::VK_SNES_LM ||
              TF == SNESMCExpr::VK_SNES_TLS_GD_HI22 ||
              TF == SNESMCExpr::VK_SNES_TLS_LDM_HI22 ||
              TF == SNESMCExpr::VK_SNES_TLS_LDO_HIX22 ||
              TF == SNESMCExpr::VK_SNES_TLS_IE_HI22 ||
              TF == SNESMCExpr::VK_SNES_TLS_LE_HIX22) &&
             "Invalid target flags for address operand on sethi");
    else if (MI->getOpcode() == SP::TLS_CALL)
      assert((TF == SNESMCExpr::VK_SNES_None ||
              TF == SNESMCExpr::VK_SNES_TLS_GD_CALL ||
              TF == SNESMCExpr::VK_SNES_TLS_LDM_CALL) &&
             "Cannot handle target flags on tls call address");
    else if (MI->getOpcode() == SP::TLS_ADDrr)
      assert((TF == SNESMCExpr::VK_SNES_TLS_GD_ADD ||
              TF == SNESMCExpr::VK_SNES_TLS_LDM_ADD ||
              TF == SNESMCExpr::VK_SNES_TLS_LDO_ADD ||
              TF == SNESMCExpr::VK_SNES_TLS_IE_ADD) &&
             "Cannot handle target flags on add for TLS");
    else if (MI->getOpcode() == SP::TLS_LDrr)
      assert(TF == SNESMCExpr::VK_SNES_TLS_IE_LD &&
             "Cannot handle target flags on ld for TLS");
    else if (MI->getOpcode() == SP::TLS_LDXrr)
      assert(TF == SNESMCExpr::VK_SNES_TLS_IE_LDX &&
             "Cannot handle target flags on ldx for TLS");
    else if (MI->getOpcode() == SP::XORri || MI->getOpcode() == SP::XORXri)
      assert((TF == SNESMCExpr::VK_SNES_TLS_LDO_LOX10 ||
              TF == SNESMCExpr::VK_SNES_TLS_LE_LOX10) &&
             "Cannot handle target flags on xor for TLS");
    else
      assert((TF == SNESMCExpr::VK_SNES_LO || TF == SNESMCExpr::VK_SNES_M44 ||
              TF == SNESMCExpr::VK_SNES_L44 || TF == SNESMCExpr::VK_SNES_HM ||
              TF == SNESMCExpr::VK_SNES_TLS_GD_LO10 ||
              TF == SNESMCExpr::VK_SNES_TLS_LDM_LO10 ||
              TF == SNESMCExpr::VK_SNES_TLS_IE_LO10) &&
             "Invalid target flags for small address operand");
  }
#endif

  bool CloseParen = SNESMCExpr::printVariantKind(O, TF);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << "%" << StringRef(getRegisterName(MO.getReg())).lower();
    break;

  case MachineOperand::MO_Immediate:
    O << MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MO.getMBB()->getSymbol()->print(O, MAI);
    return;
  case MachineOperand::MO_GlobalAddress:
    PrintSymbolOperand(MO, O);
    break;
  case MachineOperand::MO_BlockAddress:
    O << GetBlockAddressSymbol(MO.getBlockAddress())->getName();
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << MO.getSymbolName();
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << DL.getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << "_"
      << MO.getIndex();
    break;
  case MachineOperand::MO_Metadata:
    MO.getMetadata()->printAsOperand(O, MMI->getModule());
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }
  if (CloseParen)
    O << ")";
}

void SNESAsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                     raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, O);

  // If this is an ADD operand, emit it like normal operands.
  if (Modifier && !strcmp(Modifier, "arith")) {
    O << ", ";
    printOperand(MI, opNum + 1, O);
    return;
  }

  if (MI->getOperand(opNum + 1).isReg() &&
      MI->getOperand(opNum + 1).getReg() == SP::G0)
    return; // don't print "+%g0"
  if (MI->getOperand(opNum + 1).isImm() &&
      MI->getOperand(opNum + 1).getImm() == 0)
    return; // don't print "+0"

  O << "+";
  printOperand(MI, opNum + 1, O);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool SNESAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                     const char *ExtraCode, raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0)
      return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default:
      // See if this is a generic print operand
      return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, O);
    case 'f':
    case 'r':
      break;
    }
  }

  printOperand(MI, OpNo, O);

  return false;
}

bool SNESAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                           unsigned OpNo, const char *ExtraCode,
                                           raw_ostream &O) {
  if (ExtraCode && ExtraCode[0])
    return true; // Unknown modifier

  O << '[';
  printMemOperand(MI, OpNo, O);
  O << ']';

  return false;
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeSNESAsmPrinter() {
  RegisterAsmPrinter<SNESAsmPrinter> X(getTheSNESTarget());
}
