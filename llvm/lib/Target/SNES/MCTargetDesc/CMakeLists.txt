add_llvm_component_library(LLVMSNESDesc
  SNESAsmBackend.cpp
  SNESELFObjectWriter.cpp
  SNESInstPrinter.cpp
  SNESMCAsmInfo.cpp
  SNESMCCodeEmitter.cpp
  SNESMCTargetDesc.cpp
  SNESMCExpr.cpp
  SNESTargetStreamer.cpp

  LINK_COMPONENTS
  MC
  SNESInfo
  Support

  ADD_TO_COMPONENT
  SNES
  )
