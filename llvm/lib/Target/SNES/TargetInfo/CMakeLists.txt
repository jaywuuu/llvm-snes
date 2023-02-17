add_llvm_component_library(LLVMSNESInfo
  SNESTargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  SNES
  )
