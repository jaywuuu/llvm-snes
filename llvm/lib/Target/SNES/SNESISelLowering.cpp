//===-- SNESISelLowering.cpp - SNES DAG Lowering Implementation ---------===//


#include "SNESISelLowering.h"
#include "MCTargetDesc/SNESMCExpr.h"
#include "SNESMachineFunctionInfo.h"
#include "SNESRegisterInfo.h"
#include "SNESTargetMachine.h"
#include "SNESTargetObjectFile.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
using namespace llvm;

bool SNESTargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool isVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
  return true;
}

SDValue
SNESTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                 bool IsVarArg,
                                 const SmallVectorImpl<ISD::OutputArg> &Outs,
                                 const SmallVectorImpl<SDValue> &OutVals,
                                 const SDLoc &DL, SelectionDAG &DAG) const {
  return LowerReturn_32(Chain, CallConv, IsVarArg, Outs, OutVals, DL, DAG);
}

SDValue
SNESTargetLowering::LowerReturn_32(SDValue Chain, CallingConv::ID CallConv,
                                    bool IsVarArg,
                                    const SmallVectorImpl<ISD::OutputArg> &Outs,
                                    const SmallVectorImpl<SDValue> &OutVals,
                                    const SDLoc &DL, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();

  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);
  // Make room for the return address offset.
  RetOps.push_back(SDValue());

  // Copy the result values into the output registers.
  for (unsigned i = 0, realRVLocIdx = 0;
       i != RVLocs.size();
       ++i, ++realRVLocIdx) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    SDValue Arg = OutVals[realRVLocIdx];

    if (VA.needsCustom()) {
      assert(VA.getLocVT() == MVT::v2i32);
      // Legalize ret v2i32 -> ret 2 x i32 (Basically: do what would
      // happen by default if this wasn't a legal type)

      SDValue Part0 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
                                  Arg,
                                  DAG.getConstant(0, DL, getVectorIdxTy(DAG.getDataLayout())));
      SDValue Part1 = DAG.getNode(ISD::EXTRACT_VECTOR_ELT, DL, MVT::i32,
                                  Arg,
                                  DAG.getConstant(1, DL, getVectorIdxTy(DAG.getDataLayout())));

      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part0, Flag);
      Flag = Chain.getValue(1);
      RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
      VA = RVLocs[++i]; // skip ahead to next loc
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Part1,
                               Flag);
    } else
      Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Arg, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned RetAddrOffset = 8; // Call Inst + Delay Slot
  RetOps[0] = Chain;  // Update chain.
  RetOps[1] = DAG.getConstant(RetAddrOffset, DL, MVT::i32);

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(SPISD::RET_FLAG, DL, MVT::Other, RetOps);
}

// Lower return values for the 64-bit ABI.
// Return values are passed the exactly the same way as function arguments.
SDValue
SNESTargetLowering::LowerReturn_64(SDValue Chain, CallingConv::ID CallConv,
                                    bool IsVarArg,
                                    const SmallVectorImpl<ISD::OutputArg> &Outs,
                                    const SmallVectorImpl<SDValue> &OutVals,
                                    const SDLoc &DL, SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // The second operand on the return instruction is the return address offset.
  // The return address is always %i7+8 with the 64-bit ABI.
  RetOps.push_back(DAG.getConstant(8, DL, MVT::i32));

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");
    SDValue OutVal = OutVals[i];

    // Integer return values must be sign or zero extended by the callee.
    switch (VA.getLocInfo()) {
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      OutVal = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    case CCValAssign::ZExt:
      OutVal = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    case CCValAssign::AExt:
      OutVal = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), OutVal);
      break;
    default:
      llvm_unreachable("Unknown loc info!");
    }

    // The custom bit on an i32 return value indicates that it should be passed
    // in the high bits of the register.
    if (VA.getValVT() == MVT::i32 && VA.needsCustom()) {
      OutVal = DAG.getNode(ISD::SHL, DL, MVT::i64, OutVal,
                           DAG.getConstant(32, DL, MVT::i32));

      // The next value may go in the low bits of the same register.
      // Handle both at once.
      if (i+1 < RVLocs.size() && RVLocs[i+1].getLocReg() == VA.getLocReg()) {
        SDValue NV = DAG.getNode(ISD::ZERO_EXTEND, DL, MVT::i64, OutVals[i+1]);
        OutVal = DAG.getNode(ISD::OR, DL, MVT::i64, OutVal, NV);
        // Skip the next value, it's already done.
        ++i;
      }
    }

    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), OutVal, Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain;  // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(SPISD::RET_FLAG, DL, MVT::Other, RetOps);
}

SDValue SNESTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  if (Subtarget->is64Bit())
    return LowerFormalArguments_64(Chain, CallConv, IsVarArg, Ins,
                                   DL, DAG, InVals);
  return LowerFormalArguments_32(Chain, CallConv, IsVarArg, Ins,
                                 DL, DAG, InVals);
}

/// LowerFormalArguments32 - V8 uses a very simple ABI, where all values are
/// passed in either one or two GPRs, including FP values.  TODO: we should
/// pass FP values in FP registers for fastcc functions.
SDValue SNESTargetLowering::LowerFormalArguments_32(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  return SDValue{};
}

// Lower formal arguments for the 64 bit ABI.
SDValue SNESTargetLowering::LowerFormalArguments_64(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  return Chain;
}

SDValue
SNESTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                               SmallVectorImpl<SDValue> &InVals) const {
  if (Subtarget->is64Bit())
    return LowerCall_64(CLI, InVals);
  return LowerCall_32(CLI, InVals);
}

static bool hasReturnsTwiceAttr(SelectionDAG &DAG, SDValue Callee,
                                const CallBase *Call) {
  if (Call)
    return Call->hasFnAttr(Attribute::ReturnsTwice);

  const Function *CalleeFn = nullptr;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    CalleeFn = dyn_cast<Function>(G->getGlobal());
  } else if (ExternalSymbolSDNode *E =
             dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const Function &Fn = DAG.getMachineFunction().getFunction();
    const Module *M = Fn.getParent();
    const char *CalleeName = E->getSymbol();
    CalleeFn = M->getFunction(CalleeName);
  }

  if (!CalleeFn)
    return false;
  return CalleeFn->hasFnAttribute(Attribute::ReturnsTwice);
}

/// IsEligibleForTailCallOptimization - Check whether the call is eligible
/// for tail call optimization.
bool SNESTargetLowering::IsEligibleForTailCallOptimization(
    CCState &CCInfo, CallLoweringInfo &CLI, MachineFunction &MF) const {

  auto &Outs = CLI.Outs;
  auto &Caller = MF.getFunction();

  // Do not tail call opt functions with "disable-tail-calls" attribute.
  if (Caller.getFnAttribute("disable-tail-calls").getValueAsString() == "true")
    return false;

  // Do not tail call opt if the stack is used to pass parameters.
  if (CCInfo.getNextStackOffset() != 0)
    return false;

  // Do not tail call opt if either the callee or caller returns
  // a struct and the other does not.
  if (!Outs.empty() && Caller.hasStructRetAttr() != Outs[0].Flags.isSRet())
    return false;

  // Byval parameters hand the function a pointer directly into the stack area
  // we want to reuse during a tail call.
  for (auto &Arg : Outs)
    if (Arg.Flags.isByVal())
      return false;

  return true;
}

// Lower a call for the 32-bit ABI.
SDValue
SNESTargetLowering::LowerCall_32(TargetLowering::CallLoweringInfo &CLI,
                                  SmallVectorImpl<SDValue> &InVals) const {
  SDValue Chain;
  return Chain;
}

// FIXME? Maybe this could be a TableGen attribute on some registers and
// this table could be generated automatically from RegInfo.
Register SNESTargetLowering::getRegisterByName(const char* RegName, LLT VT,
                                                const MachineFunction &MF) const {
  Register Reg = StringSwitch<Register>(RegName)
    .Case("A", SNES::A)
    .Default(0);

  if (Reg)
    return Reg;

  report_fatal_error("Invalid register name global variable");
}

// Lower a call for the 64-bit ABI.
SDValue
SNESTargetLowering::LowerCall_64(TargetLowering::CallLoweringInfo &CLI,
                                  SmallVectorImpl<SDValue> &InVals) const {
  return SDValue{};
}

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

TargetLowering::AtomicExpansionKind SNESTargetLowering::shouldExpandAtomicRMWInIR(AtomicRMWInst *AI) const {
  if (AI->getOperation() == AtomicRMWInst::Xchg &&
      AI->getType()->getPrimitiveSizeInBits() == 32)
    return AtomicExpansionKind::None; // Uses xchg instruction

  return AtomicExpansionKind::CmpXChg;
}

SNESTargetLowering::SNESTargetLowering(const TargetMachine &TM,
                                         const SNESSubtarget &STI)
    : TargetLowering(TM), Subtarget(&STI) {
}

bool SNESTargetLowering::useSoftFloat() const {
  return Subtarget->useSoftFloat();
}

const char *SNESTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch ((SPISD::NodeType)Opcode) {
  case SPISD::FIRST_NUMBER:    break;
  case SPISD::CMPICC:          return "SPISD::CMPICC";
  case SPISD::CMPFCC:          return "SPISD::CMPFCC";
  case SPISD::BRICC:           return "SPISD::BRICC";
  case SPISD::BRXCC:           return "SPISD::BRXCC";
  case SPISD::BRFCC:           return "SPISD::BRFCC";
  case SPISD::SELECT_ICC:      return "SPISD::SELECT_ICC";
  case SPISD::SELECT_XCC:      return "SPISD::SELECT_XCC";
  case SPISD::SELECT_FCC:      return "SPISD::SELECT_FCC";
  case SPISD::Hi:              return "SPISD::Hi";
  case SPISD::Lo:              return "SPISD::Lo";
  case SPISD::FTOI:            return "SPISD::FTOI";
  case SPISD::ITOF:            return "SPISD::ITOF";
  case SPISD::FTOX:            return "SPISD::FTOX";
  case SPISD::XTOF:            return "SPISD::XTOF";
  case SPISD::CALL:            return "SPISD::CALL";
  case SPISD::RET_FLAG:        return "SPISD::RET_FLAG";
  case SPISD::GLOBAL_BASE_REG: return "SPISD::GLOBAL_BASE_REG";
  case SPISD::FLUSHW:          return "SPISD::FLUSHW";
  case SPISD::TLS_ADD:         return "SPISD::TLS_ADD";
  case SPISD::TLS_LD:          return "SPISD::TLS_LD";
  case SPISD::TLS_CALL:        return "SPISD::TLS_CALL";
  case SPISD::TAIL_CALL:       return "SPISD::TAIL_CALL";
  case SPISD::LOAD_GDOP:       return "SPISD::LOAD_GDOP";
  }
  return nullptr;
}

EVT SNESTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &,
                                            EVT VT) const {
  if (!VT.isVector())
    return MVT::i32;
  return VT.changeVectorElementTypeToInteger();
}

/// isMaskedValueZeroForTargetNode - Return true if 'Op & Mask' is known to
/// be zero. Op is expected to be a target specific node. Used by DAG
/// combiner.
void SNESTargetLowering::computeKnownBitsForTargetNode
                                (const SDValue Op,
                                 KnownBits &Known,
                                 const APInt &DemandedElts,
                                 const SelectionDAG &DAG,
                                 unsigned Depth) const {
  KnownBits Known2;
  Known.resetAll();

  switch (Op.getOpcode()) {
  default: break;
  case SPISD::SELECT_ICC:
  case SPISD::SELECT_XCC:
  case SPISD::SELECT_FCC:
    Known = DAG.computeKnownBits(Op.getOperand(1), Depth + 1);
    Known2 = DAG.computeKnownBits(Op.getOperand(0), Depth + 1);

    // Only known if known in both the LHS and RHS.
    Known = KnownBits::commonBits(Known, Known2);
    break;
  }
}

// Look at LHS/RHS/CC and see if they are a lowered setcc instruction.  If so
// set LHS/RHS and SPCC to the LHS/RHS of the setcc and SPCC to the condition.
static void LookThroughSetCC(SDValue &LHS, SDValue &RHS,
                             ISD::CondCode CC, unsigned &SPCC) {
  if (isNullConstant(RHS) &&
      CC == ISD::SETNE &&
      (((LHS.getOpcode() == SPISD::SELECT_ICC ||
         LHS.getOpcode() == SPISD::SELECT_XCC) &&
        LHS.getOperand(3).getOpcode() == SPISD::CMPICC) ||
       (LHS.getOpcode() == SPISD::SELECT_FCC &&
        LHS.getOperand(3).getOpcode() == SPISD::CMPFCC)) &&
      isOneConstant(LHS.getOperand(0)) &&
      isNullConstant(LHS.getOperand(1))) {
    SDValue CMPCC = LHS.getOperand(3);
    SPCC = cast<ConstantSDNode>(LHS.getOperand(2))->getZExtValue();
    LHS = CMPCC.getOperand(0);
    RHS = CMPCC.getOperand(1);
  }
}

// Convert to a target node and set target flags.
SDValue SNESTargetLowering::withTargetFlags(SDValue Op, unsigned TF,
                                             SelectionDAG &DAG) const {
  if (const GlobalAddressSDNode *GA = dyn_cast<GlobalAddressSDNode>(Op))
    return DAG.getTargetGlobalAddress(GA->getGlobal(),
                                      SDLoc(GA),
                                      GA->getValueType(0),
                                      GA->getOffset(), TF);

  if (const ConstantPoolSDNode *CP = dyn_cast<ConstantPoolSDNode>(Op))
    return DAG.getTargetConstantPool(CP->getConstVal(), CP->getValueType(0),
                                     CP->getAlign(), CP->getOffset(), TF);

  if (const BlockAddressSDNode *BA = dyn_cast<BlockAddressSDNode>(Op))
    return DAG.getTargetBlockAddress(BA->getBlockAddress(),
                                     Op.getValueType(),
                                     0,
                                     TF);

  if (const ExternalSymbolSDNode *ES = dyn_cast<ExternalSymbolSDNode>(Op))
    return DAG.getTargetExternalSymbol(ES->getSymbol(),
                                       ES->getValueType(0), TF);

  llvm_unreachable("Unhandled address SDNode");
}

// Split Op into high and low parts according to HiTF and LoTF.
// Return an ADD node combining the parts.
SDValue SNESTargetLowering::makeHiLoPair(SDValue Op,
                                          unsigned HiTF, unsigned LoTF,
                                          SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = Op.getValueType();
  SDValue Hi = DAG.getNode(SPISD::Hi, DL, VT, withTargetFlags(Op, HiTF, DAG));
  SDValue Lo = DAG.getNode(SPISD::Lo, DL, VT, withTargetFlags(Op, LoTF, DAG));
  return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
}

// Build SDNodes for producing an address from a GlobalAddress, ConstantPool,
// or ExternalSymbol SDNode.
SDValue SNESTargetLowering::makeAddress(SDValue Op, SelectionDAG &DAG) const {
  SDLoc DL(Op);
  EVT VT = getPointerTy(DAG.getDataLayout());

  // Handle PIC mode first. SNES needs a got load for every variable!
  if (isPositionIndependent()) {
    const Module *M = DAG.getMachineFunction().getFunction().getParent();
    PICLevel::Level picLevel = M->getPICLevel();
    SDValue Idx;

    if (picLevel == PICLevel::SmallPIC) {
      // This is the pic13 code model, the GOT is known to be smaller than 8KiB.
      Idx = DAG.getNode(SPISD::Lo, DL, Op.getValueType(),
                        withTargetFlags(Op, SNESMCExpr::VK_SNES_GOT13, DAG));
    } else {
      // This is the pic32 code model, the GOT is known to be smaller than 4GB.
      Idx = makeHiLoPair(Op, SNESMCExpr::VK_SNES_GOT22,
                         SNESMCExpr::VK_SNES_GOT10, DAG);
    }

    SDValue GlobalBase = DAG.getNode(SPISD::GLOBAL_BASE_REG, DL, VT);
    SDValue AbsAddr = DAG.getNode(ISD::ADD, DL, VT, GlobalBase, Idx);
    // GLOBAL_BASE_REG codegen'ed with call. Inform MFI that this
    // function has calls.
    MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
    MFI.setHasCalls(true);
    return DAG.getLoad(VT, DL, DAG.getEntryNode(), AbsAddr,
                       MachinePointerInfo::getGOT(DAG.getMachineFunction()));
  }

  // This is one of the absolute code models.
  switch(getTargetMachine().getCodeModel()) {
  default:
    llvm_unreachable("Unsupported absolute code model");
  case CodeModel::Small:
    // abs32.
    return makeHiLoPair(Op, SNESMCExpr::VK_SNES_HI,
                        SNESMCExpr::VK_SNES_LO, DAG);
  case CodeModel::Medium: {
    // abs44.
    SDValue H44 = makeHiLoPair(Op, SNESMCExpr::VK_SNES_H44,
                               SNESMCExpr::VK_SNES_M44, DAG);
    H44 = DAG.getNode(ISD::SHL, DL, VT, H44, DAG.getConstant(12, DL, MVT::i32));
    SDValue L44 = withTargetFlags(Op, SNESMCExpr::VK_SNES_L44, DAG);
    L44 = DAG.getNode(SPISD::Lo, DL, VT, L44);
    return DAG.getNode(ISD::ADD, DL, VT, H44, L44);
  }
  case CodeModel::Large: {
    // abs64.
    SDValue Hi = makeHiLoPair(Op, SNESMCExpr::VK_SNES_HH,
                              SNESMCExpr::VK_SNES_HM, DAG);
    Hi = DAG.getNode(ISD::SHL, DL, VT, Hi, DAG.getConstant(32, DL, MVT::i32));
    SDValue Lo = makeHiLoPair(Op, SNESMCExpr::VK_SNES_HI,
                              SNESMCExpr::VK_SNES_LO, DAG);
    return DAG.getNode(ISD::ADD, DL, VT, Hi, Lo);
  }
  }
}

SDValue SNESTargetLowering::LowerGlobalAddress(SDValue Op,
                                                SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SNESTargetLowering::LowerConstantPool(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SNESTargetLowering::LowerBlockAddress(SDValue Op,
                                               SelectionDAG &DAG) const {
  return makeAddress(Op, DAG);
}

SDValue SNESTargetLowering::LowerGlobalTLSAddress(SDValue Op,
                                                   SelectionDAG &DAG) const {
  return SDValue{};
}

SDValue SNESTargetLowering::LowerF128_LibCallArg(SDValue Chain,
                                                  ArgListTy &Args, SDValue Arg,
                                                  const SDLoc &DL,
                                                  SelectionDAG &DAG) const {
  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  EVT ArgVT = Arg.getValueType();
  Type *ArgTy = ArgVT.getTypeForEVT(*DAG.getContext());

  ArgListEntry Entry;
  Entry.Node = Arg;
  Entry.Ty   = ArgTy;

  if (ArgTy->isFP128Ty()) {
    // Create a stack object and pass the pointer to the library function.
    int FI = MFI.CreateStackObject(16, Align(8), false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    Chain = DAG.getStore(Chain, DL, Entry.Node, FIPtr, MachinePointerInfo(),
                         Align(8));

    Entry.Node = FIPtr;
    Entry.Ty   = PointerType::getUnqual(ArgTy);
  }
  Args.push_back(Entry);
  return Chain;
}

SDValue
SNESTargetLowering::LowerF128Op(SDValue Op, SelectionDAG &DAG,
                                 const char *LibFuncName,
                                 unsigned numArgs) const {

  ArgListTy Args;

  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();
  auto PtrVT = getPointerTy(DAG.getDataLayout());

  SDValue Callee = DAG.getExternalSymbol(LibFuncName, PtrVT);
  Type *RetTy = Op.getValueType().getTypeForEVT(*DAG.getContext());
  Type *RetTyABI = RetTy;
  SDValue Chain = DAG.getEntryNode();
  SDValue RetPtr;

  if (RetTy->isFP128Ty()) {
    // Create a Stack Object to receive the return value of type f128.
    ArgListEntry Entry;
    int RetFI = MFI.CreateStackObject(16, Align(8), false);
    RetPtr = DAG.getFrameIndex(RetFI, PtrVT);
    Entry.Node = RetPtr;
    Entry.Ty   = PointerType::getUnqual(RetTy);
    if (!Subtarget->is64Bit()) {
      Entry.IsSRet = true;
      Entry.IndirectType = RetTy;
    }
    Entry.IsReturned = false;
    Args.push_back(Entry);
    RetTyABI = Type::getVoidTy(*DAG.getContext());
  }

  assert(Op->getNumOperands() >= numArgs && "Not enough operands!");
  for (unsigned i = 0, e = numArgs; i != e; ++i) {
    Chain = LowerF128_LibCallArg(Chain, Args, Op.getOperand(i), SDLoc(Op), DAG);
  }
  TargetLowering::CallLoweringInfo CLI(DAG);
  CLI.setDebugLoc(SDLoc(Op)).setChain(Chain)
    .setCallee(CallingConv::C, RetTyABI, Callee, std::move(Args));

  std::pair<SDValue, SDValue> CallInfo = LowerCallTo(CLI);

  // chain is in second result.
  if (RetTyABI == RetTy)
    return CallInfo.first;

  assert (RetTy->isFP128Ty() && "Unexpected return type!");

  Chain = CallInfo.second;

  // Load RetPtr to get the return value.
  return DAG.getLoad(Op.getValueType(), SDLoc(Op), Chain, RetPtr,
                     MachinePointerInfo(), Align(8));
}

SDValue SNESTargetLowering::LowerF128Compare(SDValue LHS, SDValue RHS,
                                              unsigned &SPCC, const SDLoc &DL,
                                              SelectionDAG &DAG) const {

  const char *LibCall = nullptr;
  bool is64Bit = Subtarget->is64Bit();
  switch(SPCC) {
  default: llvm_unreachable("Unhandled conditional code!");
  case SPCC::FCC_E  : LibCall = is64Bit? "_Qp_feq" : "_Q_feq"; break;
  case SPCC::FCC_NE : LibCall = is64Bit? "_Qp_fne" : "_Q_fne"; break;
  case SPCC::FCC_L  : LibCall = is64Bit? "_Qp_flt" : "_Q_flt"; break;
  case SPCC::FCC_G  : LibCall = is64Bit? "_Qp_fgt" : "_Q_fgt"; break;
  case SPCC::FCC_LE : LibCall = is64Bit? "_Qp_fle" : "_Q_fle"; break;
  case SPCC::FCC_GE : LibCall = is64Bit? "_Qp_fge" : "_Q_fge"; break;
  case SPCC::FCC_UL :
  case SPCC::FCC_ULE:
  case SPCC::FCC_UG :
  case SPCC::FCC_UGE:
  case SPCC::FCC_U  :
  case SPCC::FCC_O  :
  case SPCC::FCC_LG :
  case SPCC::FCC_UE : LibCall = is64Bit? "_Qp_cmp" : "_Q_cmp"; break;
  }

  auto PtrVT = getPointerTy(DAG.getDataLayout());
  SDValue Callee = DAG.getExternalSymbol(LibCall, PtrVT);
  Type *RetTy = Type::getInt32Ty(*DAG.getContext());
  ArgListTy Args;
  SDValue Chain = DAG.getEntryNode();
  Chain = LowerF128_LibCallArg(Chain, Args, LHS, DL, DAG);
  Chain = LowerF128_LibCallArg(Chain, Args, RHS, DL, DAG);

  TargetLowering::CallLoweringInfo CLI(DAG);
  CLI.setDebugLoc(DL).setChain(Chain)
    .setCallee(CallingConv::C, RetTy, Callee, std::move(Args));

  std::pair<SDValue, SDValue> CallInfo = LowerCallTo(CLI);

  // result is in first, and chain is in second result.
  SDValue Result =  CallInfo.first;

  switch(SPCC) {
  default: {
    SDValue RHS = DAG.getConstant(0, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_UL : {
    SDValue Mask   = DAG.getConstant(1, DL, Result.getValueType());
    Result = DAG.getNode(ISD::AND, DL, Result.getValueType(), Result, Mask);
    SDValue RHS    = DAG.getConstant(0, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_ULE: {
    SDValue RHS = DAG.getConstant(2, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_UG :  {
    SDValue RHS = DAG.getConstant(1, DL, Result.getValueType());
    SPCC = SPCC::ICC_G;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_UGE: {
    SDValue RHS = DAG.getConstant(1, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }

  case SPCC::FCC_U  :  {
    SDValue RHS = DAG.getConstant(3, DL, Result.getValueType());
    SPCC = SPCC::ICC_E;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_O  :  {
    SDValue RHS = DAG.getConstant(3, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_LG :  {
    SDValue Mask   = DAG.getConstant(3, DL, Result.getValueType());
    Result = DAG.getNode(ISD::AND, DL, Result.getValueType(), Result, Mask);
    SDValue RHS    = DAG.getConstant(0, DL, Result.getValueType());
    SPCC = SPCC::ICC_NE;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  case SPCC::FCC_UE : {
    SDValue Mask   = DAG.getConstant(3, DL, Result.getValueType());
    Result = DAG.getNode(ISD::AND, DL, Result.getValueType(), Result, Mask);
    SDValue RHS    = DAG.getConstant(0, DL, Result.getValueType());
    SPCC = SPCC::ICC_E;
    return DAG.getNode(SPISD::CMPICC, DL, MVT::Glue, Result, RHS);
  }
  }
}

SDValue SNESTargetLowering::LowerINTRINSIC_WO_CHAIN(SDValue Op,
                                                     SelectionDAG &DAG) const {
  return SDValue{};
}

SDValue SNESTargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  return SDValue{};
}

SDValue SNESTargetLowering::bitcastConstantFPToInt(ConstantFPSDNode *C,
                                                    const SDLoc &DL,
                                                    SelectionDAG &DAG) const {
  APInt V = C->getValueAPF().bitcastToAPInt();
  SDValue Lo = DAG.getConstant(V.zextOrTrunc(32), DL, MVT::i32);
  SDValue Hi = DAG.getConstant(V.lshr(32).zextOrTrunc(32), DL, MVT::i32);
  if (DAG.getDataLayout().isLittleEndian())
    std::swap(Lo, Hi);
  return DAG.getBuildVector(MVT::v2i32, DL, {Hi, Lo});
}

SDValue SNESTargetLowering::PerformBITCASTCombine(SDNode *N,
                                                   DAGCombinerInfo &DCI) const {
  SDLoc dl(N);
  SDValue Src = N->getOperand(0);

  if (isa<ConstantFPSDNode>(Src) && N->getSimpleValueType(0) == MVT::v2i32 &&
      Src.getSimpleValueType() == MVT::f64)
    return bitcastConstantFPToInt(cast<ConstantFPSDNode>(Src), dl, DCI.DAG);

  return SDValue();
}

SDValue SNESTargetLowering::PerformDAGCombine(SDNode *N,
                                               DAGCombinerInfo &DCI) const {
  switch (N->getOpcode()) {
  default:
    break;
  case ISD::BITCAST:
    return PerformBITCASTCombine(N, DCI);
  }
  return SDValue();
}

MachineBasicBlock *
SNESTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const {
  return nullptr;
}

MachineBasicBlock *
SNESTargetLowering::expandSelectCC(MachineInstr &MI, MachineBasicBlock *BB,
                                    unsigned BROpcode) const {
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                         SNES Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
SNESTargetLowering::ConstraintType
SNESTargetLowering::getConstraintType(StringRef Constraint) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    default:  break;
    case 'r':
    case 'f':
    case 'e':
      return C_RegisterClass;
    case 'I': // SIMM13
      return C_Immediate;
    }
  }

  return TargetLowering::getConstraintType(Constraint);
}

TargetLowering::ConstraintWeight SNESTargetLowering::
getSingleConstraintMatchWeight(AsmOperandInfo &info,
                               const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
  // If we don't have a value, we can't do a match,
  // but allow it at the lowest weight.
  if (!CallOperandVal)
    return CW_Default;

  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'I': // SIMM13
    if (ConstantInt *C = dyn_cast<ConstantInt>(info.CallOperandVal)) {
      if (isInt<13>(C->getSExtValue()))
        weight = CW_Constant;
    }
    break;
  }
  return weight;
}

/// LowerAsmOperandForConstraint - Lower the specified operand into the Ops
/// vector.  If it is invalid, don't add anything to Ops.
void SNESTargetLowering::
LowerAsmOperandForConstraint(SDValue Op,
                             std::string &Constraint,
                             std::vector<SDValue> &Ops,
                             SelectionDAG &DAG) const {
  SDValue Result;

  // Only support length 1 constraints for now.
  if (Constraint.length() > 1)
    return;

  char ConstraintLetter = Constraint[0];
  switch (ConstraintLetter) {
  default: break;
  case 'I':
    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op)) {
      if (isInt<13>(C->getSExtValue())) {
        Result = DAG.getTargetConstant(C->getSExtValue(), SDLoc(Op),
                                       Op.getValueType());
        break;
      }
      return;
    }
  }

  if (Result.getNode()) {
    Ops.push_back(Result);
    return;
  }
  TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

std::pair<unsigned, const TargetRegisterClass *>
SNESTargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                  StringRef Constraint,
                                                  MVT VT) const {
  auto ResultPair =
      TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);

  return ResultPair;
}

bool
SNESTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The SNES target isn't yet aware of offsets.
  return false;
}

void SNESTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue>& Results,
                                             SelectionDAG &DAG) const {
}

// Override to enable LOAD_STACK_GUARD lowering on Linux.
bool SNESTargetLowering::useLoadStackGuardNode() const {
  if (!Subtarget->isTargetLinux())
    return TargetLowering::useLoadStackGuardNode();
  return true;
}

// Override to disable global variable loading on Linux.
void SNESTargetLowering::insertSSPDeclarations(Module &M) const {
  if (!Subtarget->isTargetLinux())
    return TargetLowering::insertSSPDeclarations(M);
}
