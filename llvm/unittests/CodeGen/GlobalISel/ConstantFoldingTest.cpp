//===- ConstantFoldingTest.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "GISelMITest.h"
#include "llvm/CodeGen/GlobalISel/CSEMIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"
#include "llvm/CodeGen/GlobalISel/Utils.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "gtest/gtest.h"

using namespace llvm;

namespace {

TEST_F(AArch64GISelMITest, FoldWithBuilder) {
  setUp();
  if (!TM)
    return;
  // Try to use the FoldableInstructionsBuilder to build binary ops.
  CSEMIRBuilder CFB(B.getState());
  LLT s32 = LLT::scalar(32);
  int64_t Cst;
  auto MIBCAdd =
      CFB.buildAdd(s32, CFB.buildConstant(s32, 0), CFB.buildConstant(s32, 1));
  // This should be a constant now.
  bool match = mi_match(MIBCAdd.getReg(0), *MRI, m_ICst(Cst));
  EXPECT_TRUE(match);
  EXPECT_EQ(Cst, 1);
  auto MIBCAdd1 =
      CFB.buildInstr(TargetOpcode::G_ADD, {s32},
                     {CFB.buildConstant(s32, 0), CFB.buildConstant(s32, 1)});
  // This should be a constant now.
  match = mi_match(MIBCAdd1.getReg(0), *MRI, m_ICst(Cst));
  EXPECT_TRUE(match);
  EXPECT_EQ(Cst, 1);

  // Try one of the other constructors of MachineIRBuilder to make sure it's
  // compatible.
  CSEMIRBuilder CFB1(*MF);
  CFB1.setInsertPt(*EntryMBB, EntryMBB->end());
  auto MIBCSub =
      CFB1.buildInstr(TargetOpcode::G_SUB, {s32},
                      {CFB1.buildConstant(s32, 1), CFB1.buildConstant(s32, 1)});
  // This should be a constant now.
  match = mi_match(MIBCSub.getReg(0), *MRI, m_ICst(Cst));
  EXPECT_TRUE(match);
  EXPECT_EQ(Cst, 0);

  auto MIBCSext1 =
      CFB1.buildInstr(TargetOpcode::G_SEXT_INREG, {s32},
                      {CFB1.buildConstant(s32, 0x01), uint64_t(8)});
  // This should be a constant now.
  match = mi_match(MIBCSext1.getReg(0), *MRI, m_ICst(Cst));
  EXPECT_TRUE(match);
  EXPECT_EQ(1, Cst);

  auto MIBCSext2 =
      CFB1.buildInstr(TargetOpcode::G_SEXT_INREG, {s32},
                      {CFB1.buildConstant(s32, 0x80), uint64_t(8)});
  // This should be a constant now.
  match = mi_match(MIBCSext2.getReg(0), *MRI, m_ICst(Cst));
  EXPECT_TRUE(match);
  EXPECT_EQ(-0x80, Cst);
}

TEST_F(AArch64GISelMITest, FoldBinOp) {
  setUp();
  if (!TM)
    return;

  LLT s32{LLT::scalar(32)};
  auto MIBCst1 = B.buildConstant(s32, 16);
  auto MIBCst2 = B.buildConstant(s32, 9);
  auto MIBFCst1 = B.buildFConstant(s32, 1.0000001);
  auto MIBFCst2 = B.buildFConstant(s32, 2.0);

  // Test G_ADD folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGAddInt =
      ConstantFoldBinOp(TargetOpcode::G_ADD, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAddInt.has_value());
  EXPECT_EQ(25ULL, FoldGAddInt.value().getLimitedValue());
  Optional<APInt> FoldGAddMix =
      ConstantFoldBinOp(TargetOpcode::G_ADD, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAddMix.has_value());
  EXPECT_EQ(1073741840ULL, FoldGAddMix.value().getLimitedValue());

  // Test G_AND folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGAndInt =
      ConstantFoldBinOp(TargetOpcode::G_AND, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAndInt.has_value());
  EXPECT_EQ(0ULL, FoldGAndInt.value().getLimitedValue());
  Optional<APInt> FoldGAndMix =
      ConstantFoldBinOp(TargetOpcode::G_AND, MIBCst2.getReg(0),
                        MIBFCst1.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAndMix.has_value());
  EXPECT_EQ(1ULL, FoldGAndMix.value().getLimitedValue());

  // Test G_ASHR folding Integer + Mixed cases
  Optional<APInt> FoldGAShrInt =
      ConstantFoldBinOp(TargetOpcode::G_ASHR, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAShrInt.has_value());
  EXPECT_EQ(0ULL, FoldGAShrInt.value().getLimitedValue());
  Optional<APInt> FoldGAShrMix =
      ConstantFoldBinOp(TargetOpcode::G_ASHR, MIBFCst2.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGAShrMix.has_value());
  EXPECT_EQ(2097152ULL, FoldGAShrMix.value().getLimitedValue());

  // Test G_LSHR folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGLShrInt =
      ConstantFoldBinOp(TargetOpcode::G_LSHR, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGLShrInt.has_value());
  EXPECT_EQ(0ULL, FoldGLShrInt.value().getLimitedValue());
  Optional<APInt> FoldGLShrMix =
      ConstantFoldBinOp(TargetOpcode::G_LSHR, MIBFCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGLShrMix.has_value());
  EXPECT_EQ(2080768ULL, FoldGLShrMix.value().getLimitedValue());

  // Test G_MUL folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGMulInt =
      ConstantFoldBinOp(TargetOpcode::G_MUL, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGMulInt.has_value());
  EXPECT_EQ(144ULL, FoldGMulInt.value().getLimitedValue());
  Optional<APInt> FoldGMulMix =
      ConstantFoldBinOp(TargetOpcode::G_MUL, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGMulMix.has_value());
  EXPECT_EQ(0ULL, FoldGMulMix.value().getLimitedValue());

  // Test G_OR folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGOrInt =
      ConstantFoldBinOp(TargetOpcode::G_OR, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGOrInt.has_value());
  EXPECT_EQ(25ULL, FoldGOrInt.value().getLimitedValue());
  Optional<APInt> FoldGOrMix =
      ConstantFoldBinOp(TargetOpcode::G_OR, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGOrMix.has_value());
  EXPECT_EQ(1073741840ULL, FoldGOrMix.value().getLimitedValue());

  // Test G_SHL folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGShlInt =
      ConstantFoldBinOp(TargetOpcode::G_SHL, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGShlInt.has_value());
  EXPECT_EQ(8192ULL, FoldGShlInt.value().getLimitedValue());
  Optional<APInt> FoldGShlMix =
      ConstantFoldBinOp(TargetOpcode::G_SHL, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGShlMix.has_value());
  EXPECT_EQ(0ULL, FoldGShlMix.value().getLimitedValue());

  // Test G_SUB folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGSubInt =
      ConstantFoldBinOp(TargetOpcode::G_SUB, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSubInt.has_value());
  EXPECT_EQ(7ULL, FoldGSubInt.value().getLimitedValue());
  Optional<APInt> FoldGSubMix =
      ConstantFoldBinOp(TargetOpcode::G_SUB, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSubMix.has_value());
  EXPECT_EQ(3221225488ULL, FoldGSubMix.value().getLimitedValue());

  // Test G_XOR folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGXorInt =
      ConstantFoldBinOp(TargetOpcode::G_XOR, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGXorInt.has_value());
  EXPECT_EQ(25ULL, FoldGXorInt.value().getLimitedValue());
  Optional<APInt> FoldGXorMix =
      ConstantFoldBinOp(TargetOpcode::G_XOR, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGXorMix.has_value());
  EXPECT_EQ(1073741840ULL, FoldGXorMix.value().getLimitedValue());

  // Test G_UDIV folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGUdivInt =
      ConstantFoldBinOp(TargetOpcode::G_UDIV, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGUdivInt.has_value());
  EXPECT_EQ(1ULL, FoldGUdivInt.value().getLimitedValue());
  Optional<APInt> FoldGUdivMix =
      ConstantFoldBinOp(TargetOpcode::G_UDIV, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGUdivMix.has_value());
  EXPECT_EQ(0ULL, FoldGUdivMix.value().getLimitedValue());

  // Test G_SDIV folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGSdivInt =
      ConstantFoldBinOp(TargetOpcode::G_SDIV, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSdivInt.has_value());
  EXPECT_EQ(1ULL, FoldGSdivInt.value().getLimitedValue());
  Optional<APInt> FoldGSdivMix =
      ConstantFoldBinOp(TargetOpcode::G_SDIV, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSdivMix.has_value());
  EXPECT_EQ(0ULL, FoldGSdivMix.value().getLimitedValue());

  // Test G_UREM folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGUremInt =
      ConstantFoldBinOp(TargetOpcode::G_UDIV, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGUremInt.has_value());
  EXPECT_EQ(1ULL, FoldGUremInt.value().getLimitedValue());
  Optional<APInt> FoldGUremMix =
      ConstantFoldBinOp(TargetOpcode::G_UDIV, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGUremMix.has_value());
  EXPECT_EQ(0ULL, FoldGUremMix.value().getLimitedValue());

  // Test G_SREM folding Integer + Mixed Int-Float cases
  Optional<APInt> FoldGSremInt =
      ConstantFoldBinOp(TargetOpcode::G_SREM, MIBCst1.getReg(0),
                        MIBCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSremInt.has_value());
  EXPECT_EQ(7ULL, FoldGSremInt.value().getLimitedValue());
  Optional<APInt> FoldGSremMix =
      ConstantFoldBinOp(TargetOpcode::G_SREM, MIBCst1.getReg(0),
                        MIBFCst2.getReg(0), *MRI);
  EXPECT_TRUE(FoldGSremMix.has_value());
  EXPECT_EQ(16ULL, FoldGSremMix.value().getLimitedValue());
}

} // namespace
