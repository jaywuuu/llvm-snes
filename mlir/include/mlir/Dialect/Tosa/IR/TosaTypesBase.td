//===-- TosaTypesBase.td - TOSA type definitions -----------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the type definitions for the TOSA dialect.
//
//===----------------------------------------------------------------------===//

#ifndef TOSA_TYPES_BASE
#define TOSA_TYPES_BASE

include "mlir/IR/OpBase.td"

//===----------------------------------------------------------------------===//
// Tosa Type Definitions.
//===----------------------------------------------------------------------===//

// The base class of a quantized type.
// Param tuple is: [bitwidth, zeropt, smantissa, sexp, low_end, high_end].
// Where low and high ends are 0,255 when unsigned, -128,127 when signed, for
// the 8-bit case.
class Tosa_QuantizedType<string n, list<int> params, bit signed>
  : Type<And<[CPred<"$_self.isa<mlir::quant::QuantizedType>()">,
              CPred<"$_self.cast<mlir::quant::QuantizedType>()" #
                    ".getStorageTypeIntegralWidth() == " # !head(params)>]>,
    "Q" # !if (signed, "int", "uint") # !head(params) # " type"> {
  string name = n;
  string asTraitArgsStr = !interleave(params, ", ") #
                          !if(signed, ", true", ", false");
}

//===----------------------------------------------------------------------===//
// Non-Quantized Signed Integer Types.
// Used to express accumulator results or compare results.
//===----------------------------------------------------------------------===//

def Tosa_Int8 : I<8>;
def Tosa_Int16 : I<16>;
def Tosa_Int32 : I<32>;
def Tosa_Int48 : I<48>;
def Tosa_Int64 : I<64>;

def Tosa_SignedInt : AnyTypeOf<[Tosa_Int8,
                                Tosa_Int16,
                                Tosa_Int32,
                                Tosa_Int48,
                                Tosa_Int64]>;

def Tosa_Bool : I<1>;

// No unsigned unquantized int types.
def Tosa_Int : AnyTypeOf<[Tosa_Bool,
                          Tosa_SignedInt]>;

def Tosa_Int32Or64 : AnyTypeOf<[Tosa_Int32,
                   	        Tosa_Int64]>;

//===----------------------------------------------------------------------===//
// Quantized Integer Types.
// Datatype for network feature map or weight content.
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// Name    Symmetry   Grouping                Sign
//===----------------------------------------------------------------------===//
// aint8 : asymmetric per tensor,             signed
// uint8 : asymmetric per tensor ,            unsigned
// int4  : symmetric  per channel,            signed
// int8  : symmetric  per tensor/per channel, signed
// int16 : symmetric  per tensor,             signed
//===----------------------------------------------------------------------===//
def Tosa_QuantizedInt	: AnyTypeOf<[Tosa_QuantizedType<"aint8", [8], 1>,
                                     Tosa_QuantizedType<"uint8", [8], 0>,
                                     Tosa_QuantizedType<"int4", [4, 0], 1>,
                                     Tosa_QuantizedType<"int8", [8, 0], 1>,
                                     Tosa_QuantizedType<"int16", [16, 0], 1>]>;

//===----------------------------------------------------------------------===//
// Floating-point types.
//===----------------------------------------------------------------------===//
def Tosa_Float : AnyTypeOf<[
                            F32,
			    F16,
			    BF16]>;

//===----------------------------------------------------------------------===//
// Multi-category types.
//===----------------------------------------------------------------------===//
def Tosa_AnyNumber : AnyTypeOf<[Tosa_Int, Tosa_QuantizedInt, Tosa_Float],
                               "number">;

//===----------------------------------------------------------------------===//
// Tensor types
//===----------------------------------------------------------------------===//

def Tosa_Int32Or64Tensor : TensorOf<[Tosa_Int32Or64]>;

def Tosa_Tensor : TensorOf<[Tosa_AnyNumber]>;

// Any tensor element type allowed in Tosa ops.
def Tosa_ElementType : Type<Or<[Tosa_Int.predicate, Tosa_QuantizedInt.predicate,
                                Tosa_Float.predicate]>, "tosa.dtype">;

class Tosa_TensorOfOrNone<list<Type> allowedTypes, string description = ""> :
  AnyTypeOf<[TensorOf<allowedTypes>, NoneType], description>;

//===----------------------------------------------------------------------===//
// Tensor types with constrained ranks.
//===----------------------------------------------------------------------===//

// Must be listed rank.
def Tosa_Tensor1D : 1DTensorOf<[Tosa_AnyNumber]>;
def Tosa_Tensor2D : 2DTensorOf<[Tosa_AnyNumber]>;
def Tosa_Tensor4D : 4DTensorOf<[Tosa_AnyNumber]>;
def Tosa_Tensor5D : TensorRankOf<[Tosa_AnyNumber], [5]>;
def Tosa_Tensor6D : TensorRankOf<[Tosa_AnyNumber], [6]>;

// Ranked tensors up to given rank.
def Tosa_Tensor1Dto2D : TensorRankOf<[Tosa_AnyNumber], [1,2]>;
def Tosa_Tensor1Dto4D : TensorRankOf<[Tosa_AnyNumber], [1,2,3,4]>;
def Tosa_Tensor1Dto5D : TensorRankOf<[Tosa_AnyNumber], [1,2,3,4,5]>;
def Tosa_Tensor1Dto6D : TensorRankOf<[Tosa_AnyNumber], [1,2,3,4,5,6]>;

def Tosa_TensorUpto4D : TensorRankOf<[Tosa_AnyNumber], [0,1,2,3,4]>;
def Tosa_TensorUpto6D : TensorRankOf<[Tosa_AnyNumber], [0,1,2,3,4,5,6]>;

//===----------------------------------------------------------------------===//
// Attribute predicates and classes.
//===----------------------------------------------------------------------===//
class ArrayMaxCt<int n> : AttrConstraint<
    CPred<"$_self.cast<::mlir::ArrayAttr>().size() <= " # n>,
    "with at least " # n # " elements">;

def Tosa_IntArrayAttr2 : Confined<I64ArrayAttr, [ArrayCount<2>]>;
def Tosa_IntArrayAttr3 : Confined<I64ArrayAttr, [ArrayCount<3>]>;
def Tosa_IntArrayAttr4 : Confined<I64ArrayAttr, [ArrayCount<4>]>;
def Tosa_IntArrayAttr5 : Confined<I64ArrayAttr, [ArrayCount<5>]>;
def Tosa_IntArrayAttr6 : Confined<I64ArrayAttr, [ArrayCount<6>]>;

def Tosa_IntArrayAttrUpto2 : Confined<I64ArrayAttr, [ArrayMaxCt<2>]>;
def Tosa_IntArrayAttrUpto4 : Confined<I64ArrayAttr, [ArrayMaxCt<4>]>;
def Tosa_IntArrayAttrUpto5 : Confined<I64ArrayAttr, [ArrayMaxCt<5>]>;

//===----------------------------------------------------------------------===//
// Iterable attributes.
//===----------------------------------------------------------------------===//
// Supported regimes for tosa.resize.
def Tosa_ResizeTypeAttr : StringBasedAttr<
    CPred<"$_self.cast<StringAttr>().getValue() == \"BILINEAR\"  || " #
          "$_self.cast<StringAttr>().getValue() == \"NEAREST_NEIGHBOR\"">,
    "Supported resize/upsampling strategies">;

def Tosa_TensorTypeAttr : TypeAttrBase<"TensorType", "Tensor type attribute">;

// Tensor to buffer types.
def Tosa_Buffer : MemRefOf<[Tosa_AnyNumber]>;
def Tosa_TupleBuffer : NestedTupleOf<[Tosa_Buffer]>;
def Tosa_BufOrTuple : AnyTypeOf<[Tosa_Buffer, Tosa_TupleBuffer]>;

#endif // TOSA_TYPES_BASE
