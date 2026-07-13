{- | Validated hand-built SPIR-V 1.6 starter modules.

The starter modules are pure values; inspect their words or serialize them
only after checking the assembler result:

@
module Main (main) where

import Vpipe.SpirV (moduleWords, vertexModule)

main :: IO ()
main = print (fmap moduleWords vertexModule)
@
-}
module Vpipe.SpirV (SpirVModule, moduleBytes, moduleWords, vertexModule, fragmentModule, computeModule) where

import Vpipe.SpirV.Assembler
import Vpipe.SpirV.Generated

vertexModule, fragmentModule, computeModule :: Either AssemblerError SpirVModule
vertexModule = runAssembler $ do
  void <- typeVoid
  functionType <- typeFunction void []
  float <- typeFloat 32
  vector4 <- typeVector float 4
  int32 <- typeInt 32 1
  bool <- typeBool
  let inputStorage = enumerant "StorageClass" "Input"
  outputPointer <- typePointer storageClassOutput vector4
  inputPointer <- typePointer inputStorage int32
  zero <- constantF32 float 0
  one <- constantF32 float 0x3f800000
  negativeHalf <- constantF32 float 0xbf000000
  positiveHalf <- constantF32 float 0x3f000000
  lowerLeft <- constantComposite vector4 [negativeHalf, positiveHalf, zero, one]
  lowerRight <- constantComposite vector4 [positiveHalf, positiveHalf, zero, one]
  upperMiddle <- constantComposite vector4 [zero, negativeHalf, zero, one]
  indexZero <- constantWord int32 [0]
  indexOne <- constantWord int32 [1]
  output <- emitVariable outputPointer storageClassOutput
  vertexIndex <- emitVariable inputPointer inputStorage
  function <- emitFunction void functionType
  emitEntryPoint executionModelVertex function "main" [output, vertexIndex]
  emitName output "gl_Position"
  emitName vertexIndex "gl_VertexIndex"
  emitDecorate output decorationBuiltIn [builtInPosition]
  emitDecorate vertexIndex decorationBuiltIn [enumerant "BuiltIn" "VertexIndex"]
  _ <- emitLabel
  loadedIndex <- emitLoad int32 vertexIndex
  isFirstVertex <- emitBinary IEqual bool loadedIndex indexZero
  isSecondVertex <- emitBinary IEqual bool loadedIndex indexOne
  laterPosition <- emitSelect vector4 isSecondVertex lowerRight upperMiddle
  position <- emitSelect vector4 isFirstVertex lowerLeft laterPosition
  emitStore output position
  emitReturn
  emitFunctionEnd
  finishModule
fragmentModule = runAssembler $ do
  void <- typeVoid
  functionType <- typeFunction void []
  float <- typeFloat 32
  vector4 <- typeVector float 4
  outputPointer <- typePointer storageClassOutput vector4
  zero <- constantF32 float 0
  one <- constantF32 float 0x3f800000
  color <- constantComposite vector4 [one, zero, zero, one]
  output <- emitVariable outputPointer storageClassOutput
  function <- emitFunction void functionType
  emitEntryPoint executionModelFragment function "main" [output]
  emitExecutionMode function executionModeOriginUpperLeft []
  emitName output "color"
  emitDecorate output decorationLocation [0]
  _ <- emitLabel
  emitStore output color
  emitReturn
  emitFunctionEnd
  finishModule
computeModule = runAssembler $ do
  void <- typeVoid
  functionType <- typeFunction void []
  function <- emitFunction void functionType
  emitEntryPoint executionModelGLCompute function "main" []
  emitExecutionMode function executionModeLocalSize [1, 1, 1]
  _ <- emitLabel
  emitReturn
  emitFunctionEnd
  finishModule
