add_llvm_component_library(LLVMSNESDisassembler
  SNESDisassembler.cpp

  LINK_COMPONENTS
  MCDisassembler
  SNESInfo
  Support

  ADD_TO_COMPONENT
  SNES
  )
