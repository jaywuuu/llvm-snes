//===-- SNESFrameLowering.cpp - SNES Frame Information ------------------===//

#include "SNESFrameLowering.h"
#include "SNESInstrInfo.h"
#include "SNESMachineFunctionInfo.h"
#include "SNESSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

static cl::opt<bool>
    DisableLeafProc("disable-SNES-leaf-proc", cl::init(false),
                    cl::desc("Disable SNES leaf procedure optimization."),
                    cl::Hidden);

SNESFrameLowering::SNESFrameLowering(const SNESSubtarget &ST)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown,
                          ST.is64Bit() ? Align(16) : Align(8), 0,
                          ST.is64Bit() ? Align(16) : Align(8)) {}

void SNESFrameLowering::emitSPAdjustment(MachineFunction &MF,
                                         MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator MBBI,
                                         int NumBytes, unsigned ADDrr,
                                         unsigned ADDri) const {

  DebugLoc dl;
  const SNESInstrInfo &TII =
      *static_cast<const SNESInstrInfo *>(MF.getSubtarget().getInstrInfo());

  if (NumBytes >= -4096 && NumBytes < 4096) {
    BuildMI(MBB, MBBI, dl, TII.get(ADDri), SP::O6)
        .addReg(SP::O6)
        .addImm(NumBytes);
    return;
  }

  // Emit this the hard way.  This clobbers G1 which we always know is
  // available here.
  if (NumBytes >= 0) {
    // Emit nonnegative numbers with sethi + or.
    // sethi %hi(NumBytes), %g1
    // or %g1, %lo(NumBytes), %g1
    // add %sp, %g1, %sp
    BuildMI(MBB, MBBI, dl, TII.get(SP::SETHIi), SP::G1).addImm(HI22(NumBytes));
    BuildMI(MBB, MBBI, dl, TII.get(SP::ORri), SP::G1)
        .addReg(SP::G1)
        .addImm(LO10(NumBytes));
    BuildMI(MBB, MBBI, dl, TII.get(ADDrr), SP::O6)
        .addReg(SP::O6)
        .addReg(SP::G1);
    return;
  }

  // Emit negative numbers with sethi + xor.
  // sethi %hix(NumBytes), %g1
  // xor %g1, %lox(NumBytes), %g1
  // add %sp, %g1, %sp
  BuildMI(MBB, MBBI, dl, TII.get(SP::SETHIi), SP::G1).addImm(HIX22(NumBytes));
  BuildMI(MBB, MBBI, dl, TII.get(SP::XORri), SP::G1)
      .addReg(SP::G1)
      .addImm(LOX10(NumBytes));
  BuildMI(MBB, MBBI, dl, TII.get(ADDrr), SP::O6).addReg(SP::O6).addReg(SP::G1);
}

void SNESFrameLowering::emitPrologue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  SNESMachineFunctionInfo *FuncInfo = MF.getInfo<SNESMachineFunctionInfo>();

  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const SNESSubtarget &Subtarget = MF.getSubtarget<SNESSubtarget>();
  const SNESInstrInfo &TII =
      *static_cast<const SNESInstrInfo *>(Subtarget.getInstrInfo());
  const SNESRegisterInfo &RegInfo =
      *static_cast<const SNESRegisterInfo *>(Subtarget.getRegisterInfo());
  MachineBasicBlock::iterator MBBI = MBB.begin();
  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc dl;
  bool NeedsStackRealignment = RegInfo.shouldRealignStack(MF);

  if (NeedsStackRealignment && !RegInfo.canRealignStack(MF))
    report_fatal_error("Function \"" + Twine(MF.getName()) +
                       "\" required "
                       "stack re-alignment, but LLVM couldn't handle it "
                       "(probably because it has a dynamic alloca).");

  // Get the number of bytes to allocate from the FrameInfo
  int NumBytes = (int)MFI.getStackSize();

  unsigned SAVEri = SP::SAVEri;
  unsigned SAVErr = SP::SAVErr;
  if (FuncInfo->isLeafProc()) {
    if (NumBytes == 0)
      return;
    SAVEri = SP::ADDri;
    SAVErr = SP::ADDrr;
  }

  // The SNES ABI is a bit odd in that it requires a reserved 92-byte
  // (128 in v9) area in the user's stack, starting at %sp. Thus, the
  // first part of the stack that can actually be used is located at
  // %sp + 92.
  //
  // We therefore need to add that offset to the total stack size
  // after all the stack objects are placed by
  // PrologEpilogInserter calculateFrameObjectOffsets. However, since the stack
  // needs to be aligned *after* the extra size is added, we need to disable
  // calculateFrameObjectOffsets's built-in stack alignment, by having
  // targetHandlesStackFrameRounding return true.

  // Add the extra call frame stack size, if needed. (This is the same
  // code as in PrologEpilogInserter, but also gets disabled by
  // targetHandlesStackFrameRounding)
  if (MFI.adjustsStack() && hasReservedCallFrame(MF))
    NumBytes += MFI.getMaxCallFrameSize();

  // Adds the SNES subtarget-specific spill area to the stack
  // size. Also ensures target-required alignment.
  NumBytes = Subtarget.getAdjustedFrameSize(NumBytes);

  // Finally, ensure that the size is sufficiently aligned for the
  // data on the stack.
  NumBytes = alignTo(NumBytes, MFI.getMaxAlign());

  // Update stack size with corrected value.
  MFI.setStackSize(NumBytes);

  emitSPAdjustment(MF, MBB, MBBI, -NumBytes, SAVErr, SAVEri);

  unsigned regFP = RegInfo.getDwarfRegNum(SP::I6, true);

  // Emit ".cfi_def_cfa_register 30".
  unsigned CFIIndex =
      MF.addFrameInst(MCCFIInstruction::createDefCfaRegister(nullptr, regFP));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  // Emit ".cfi_window_save".
  CFIIndex = MF.addFrameInst(MCCFIInstruction::createWindowSave(nullptr));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  unsigned regInRA = RegInfo.getDwarfRegNum(SP::I7, true);
  unsigned regOutRA = RegInfo.getDwarfRegNum(SP::O7, true);
  // Emit ".cfi_register 15, 31".
  CFIIndex = MF.addFrameInst(
      MCCFIInstruction::createRegister(nullptr, regOutRA, regInRA));
  BuildMI(MBB, MBBI, dl, TII.get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  if (NeedsStackRealignment) {
    int64_t Bias = Subtarget.getStackPointerBias();
    unsigned regUnbiased;
    if (Bias) {
      // This clobbers G1 which we always know is available here.
      regUnbiased = SP::G1;
      // add %o6, BIAS, %g1
      BuildMI(MBB, MBBI, dl, TII.get(SP::ADDri), regUnbiased)
          .addReg(SP::O6)
          .addImm(Bias);
    } else
      regUnbiased = SP::O6;

    // andn %regUnbiased, MaxAlign-1, %regUnbiased
    Align MaxAlign = MFI.getMaxAlign();
    BuildMI(MBB, MBBI, dl, TII.get(SP::ANDNri), regUnbiased)
        .addReg(regUnbiased)
        .addImm(MaxAlign.value() - 1U);

    if (Bias) {
      // add %g1, -BIAS, %o6
      BuildMI(MBB, MBBI, dl, TII.get(SP::ADDri), SP::O6)
          .addReg(regUnbiased)
          .addImm(-Bias);
    }
  }
}

MachineBasicBlock::iterator SNESFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  if (!hasReservedCallFrame(MF)) {
    MachineInstr &MI = *I;
    int Size = MI.getOperand(0).getImm();
    if (MI.getOpcode() == SP::ADJCALLSTACKDOWN)
      Size = -Size;

    if (Size)
      emitSPAdjustment(MF, MBB, I, Size, SP::ADDrr, SP::ADDri);
  }
  return MBB.erase(I);
}

void SNESFrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  SNESMachineFunctionInfo *FuncInfo = MF.getInfo<SNESMachineFunctionInfo>();
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const SNESInstrInfo &TII =
      *static_cast<const SNESInstrInfo *>(MF.getSubtarget().getInstrInfo());
  DebugLoc dl = MBBI->getDebugLoc();
  assert((MBBI->getOpcode() == SP::RETL || MBBI->getOpcode() == SP::TAIL_CALL ||
          MBBI->getOpcode() == SP::TAIL_CALLri) &&
         "Can only put epilog before 'retl' or 'tail_call' instruction!");
  if (!FuncInfo->isLeafProc()) {
    BuildMI(MBB, MBBI, dl, TII.get(SP::RESTORErr), SP::G0)
        .addReg(SP::G0)
        .addReg(SP::G0);
    return;
  }
  MachineFrameInfo &MFI = MF.getFrameInfo();

  int NumBytes = (int)MFI.getStackSize();
  if (NumBytes != 0)
    emitSPAdjustment(MF, MBB, MBBI, NumBytes, SP::ADDrr, SP::ADDri);

  // Preserve return address in %o7
  if (MBBI->getOpcode() == SP::TAIL_CALL) {
    MBB.addLiveIn(SP::O7);
    BuildMI(MBB, MBBI, dl, TII.get(SP::ORrr), SP::G1)
        .addReg(SP::G0)
        .addReg(SP::O7);
    BuildMI(MBB, MBBI, dl, TII.get(SP::ORrr), SP::O7)
        .addReg(SP::G0)
        .addReg(SP::G1);
  }
}

bool SNESFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // Reserve call frame if there are no variable sized objects on the stack.
  return !MF.getFrameInfo().hasVarSizedObjects();
}

// hasFP - Return true if the specified function should have a dedicated frame
// pointer register.  This is true if the function has variable sized allocas or
// if frame pointer elimination is disabled.
bool SNESFrameLowering::hasFP(const MachineFunction &MF) const {
  const TargetRegisterInfo *RegInfo = MF.getSubtarget().getRegisterInfo();

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         RegInfo->hasStackRealignment(MF) || MFI.hasVarSizedObjects() ||
         MFI.isFrameAddressTaken();
}

StackOffset
SNESFrameLowering::getFrameIndexReference(const MachineFunction &MF, int FI,
                                          Register &FrameReg) const {
  const SNESSubtarget &Subtarget = MF.getSubtarget<SNESSubtarget>();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const SNESRegisterInfo *RegInfo = Subtarget.getRegisterInfo();
  const SNESMachineFunctionInfo *FuncInfo =
      MF.getInfo<SNESMachineFunctionInfo>();
  bool isFixed = MFI.isFixedObjectIndex(FI);

  // Addressable stack objects are accessed using neg. offsets from
  // %fp, or positive offsets from %sp.
  bool UseFP;

  // SNES uses FP-based references in general, even when "hasFP" is
  // false. That function is rather a misnomer, because %fp is
  // actually always available, unless isLeafProc.
  if (FuncInfo->isLeafProc()) {
    // If there's a leaf proc, all offsets need to be %sp-based,
    // because we haven't caused %fp to actually point to our frame.
    UseFP = false;
  } else if (isFixed) {
    // Otherwise, argument access should always use %fp.
    UseFP = true;
  } else if (RegInfo->hasStackRealignment(MF)) {
    // If there is dynamic stack realignment, all local object
    // references need to be via %sp, to take account of the
    // re-alignment.
    UseFP = false;
  } else {
    // Finally, default to using %fp.
    UseFP = true;
  }

  int64_t FrameOffset =
      MF.getFrameInfo().getObjectOffset(FI) + Subtarget.getStackPointerBias();

  if (UseFP) {
    FrameReg = RegInfo->getFrameRegister(MF);
    return StackOffset::getFixed(FrameOffset);
  } else {
    FrameReg = SP::O6; // %sp
    return StackOffset::getFixed(FrameOffset +
                                 MF.getFrameInfo().getStackSize());
  }
}

static bool LLVM_ATTRIBUTE_UNUSED
verifyLeafProcRegUse(MachineRegisterInfo *MRI) {

  for (unsigned reg = SP::I0; reg <= SP::I7; ++reg)
    if (MRI->isPhysRegUsed(reg))
      return false;

  for (unsigned reg = SP::L0; reg <= SP::L7; ++reg)
    if (MRI->isPhysRegUsed(reg))
      return false;

  return true;
}

bool SNESFrameLowering::isLeafProc(MachineFunction &MF) const {

  MachineRegisterInfo &MRI = MF.getRegInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();

  return !(MFI.hasCalls()               // has calls
           || MRI.isPhysRegUsed(SP::L0) // Too many registers needed
           || MRI.isPhysRegUsed(SP::O6) // %sp is used
           || hasFP(MF)                 // need %fp
           || MF.hasInlineAsm());       // has inline assembly
}

void SNESFrameLowering::remapRegsForLeafProc(MachineFunction &MF) const {
  MachineRegisterInfo &MRI = MF.getRegInfo();
  // Remap %i[0-7] to %o[0-7].
  for (unsigned reg = SP::I0; reg <= SP::I7; ++reg) {
    if (!MRI.isPhysRegUsed(reg))
      continue;

    unsigned mapped_reg = reg - SP::I0 + SP::O0;

    // Replace I register with O register.
    MRI.replaceRegWith(reg, mapped_reg);

    // Also replace register pair super-registers.
    if ((reg - SP::I0) % 2 == 0) {
      unsigned preg = (reg - SP::I0) / 2 + SP::I0_I1;
      unsigned mapped_preg = preg - SP::I0_I1 + SP::O0_O1;
      MRI.replaceRegWith(preg, mapped_preg);
    }
  }

  // Rewrite MBB's Live-ins.
  for (MachineBasicBlock &MBB : MF) {
    for (unsigned reg = SP::I0_I1; reg <= SP::I6_I7; ++reg) {
      if (!MBB.isLiveIn(reg))
        continue;
      MBB.removeLiveIn(reg);
      MBB.addLiveIn(reg - SP::I0_I1 + SP::O0_O1);
    }
    for (unsigned reg = SP::I0; reg <= SP::I7; ++reg) {
      if (!MBB.isLiveIn(reg))
        continue;
      MBB.removeLiveIn(reg);
      MBB.addLiveIn(reg - SP::I0 + SP::O0);
    }
  }

  assert(verifyLeafProcRegUse(&MRI));
#ifdef EXPENSIVE_CHECKS
  MF.verify(0, "After LeafProc Remapping");
#endif
}

void SNESFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                             BitVector &SavedRegs,
                                             RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  if (!DisableLeafProc && isLeafProc(MF)) {
    SNESMachineFunctionInfo *MFI = MF.getInfo<SNESMachineFunctionInfo>();
    MFI->setLeafProc(true);

    remapRegsForLeafProc(MF);
  }
}
