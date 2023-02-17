add_llvm_component_library(LLVMSNESAsmParser
  SNESAsmParser.cpp

  LINK_COMPONENTS
  MC
  MCParser
  SNESDesc
  SNESInfo
  Support

  ADD_TO_COMPONENT
  SNES
  )
