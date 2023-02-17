add_llvm_component_group(SNES)

set(LLVM_TARGET_DEFINITIONS SNES.td)

tablegen(LLVM SNESGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM SNESGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM SNESGenCallingConv.inc -gen-callingconv)
tablegen(LLVM SNESGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM SNESGenDisassemblerTables.inc -gen-disassembler)
tablegen(LLVM SNESGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM SNESGenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM SNESGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM SNESGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(SNESCommonTableGen)

add_llvm_target(SNESCodeGen
  SNESAsmPrinter.cpp
  SNESInstrInfo.cpp
  SNESISelDAGToDAG.cpp
  SNESISelLowering.cpp
  SNESFrameLowering.cpp
  SNESMachineFunctionInfo.cpp
  SNESRegisterInfo.cpp
  SNESSubtarget.cpp
  SNESTargetMachine.cpp
  SNESMCInstLower.cpp
  SNESTargetObjectFile.cpp

  LINK_COMPONENTS
  AsmPrinter
  CodeGen
  Core
  MC
  SelectionDAG
  SNESDesc
  SNESInfo
  Support
  Target

  ADD_TO_COMPONENT
  SNES
  )

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
