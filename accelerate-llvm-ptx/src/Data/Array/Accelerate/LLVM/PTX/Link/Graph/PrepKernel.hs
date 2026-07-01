{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Array.Accelerate.LLVM.PTX.Link.Graph.PrepKernel where

import LLVM.AST.Type.Representation
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.State (LLVM)
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base (codeGenKernel)
import qualified LLVM.AST.Type.Function as LLVM
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import LLVM.AST.Type.Module (Module)
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Intrinsic ()
import Data.Array.Accelerate.Analysis.Match
import LLVM.AST.Type.Instruction
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.AST.Operation
import Data.Array.Accelerate.LLVM.CodeGen.Sugar 
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Operand
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment (GraphEnv (..))
import Data.Array.Accelerate.AST.Schedule.Uniform
import Data.Array.Accelerate.AST.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Shape
import Data.Array.Accelerate.LLVM.PTX.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Compile
import Control.Monad.Reader
import Crypto.Hash.XKCP
import Data.String
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch



type PrepEnv env = Ptr (Struct (KernelArgPtrs env))

type PrepKernel env = PrepEnv env -> Ptr (Struct (KernelArgs env)) -> ()

type PrepContext env f = (Env GraphEnv env, SArgs env f)


type family KernelArg a where
  KernelArg (m DIM1 e) = Ptr (BufferEltR e)
  KernelArg (Var' e) = (BufferEltR e)

type family KernelArgs f where
  KernelArgs () = ()
  KernelArgs (t -> f) = (KernelArg t, KernelArgs f)


type family KernelArgPtrs f where 
  KernelArgPtrs () = ()
  KernelArgPtrs (t -> f) = (Ptr (KernelArg t), KernelArgPtrs f)

-- Exists Idx env
-- IPV lijst Tupr gebruiken als mapping naar de kleine environment Tupr
-- TupR (Idx genv) kenv' -> TupIDX kenv kenv'
-- TupleIdx (type van het complete tuple constructie) (type van de waarde op die index)

-- Einde elke dag klein taakje voor volgende dag opschrijven

compilePrep ::  UID -> String -> PrepContext env env' -> LLVM PTX (ObjectR (PrepKernel env'))
compilePrep uid name ss = do
  dev <- asks ptxDeviceProperties
  let uid' = hashIncrement 3 uid
  m <- prepKernelCodeGen name ss
  obj <- compile uid' (fromString name) (simpleLaunchConfig dev) m
  obj `seq` return ()
  return obj

prepKernelCodeGen
  :: String
  -> PrepContext env' env -> LLVM PTX (Module (PrepKernel env))
prepKernelCodeGen name ctx = do
  (_, m) <- codeGenKernel name 
    ( LLVM.Lam (PtrPrimType inStructType defaultAddrSpace) "in_env"
    . LLVM.Lam (PtrPrimType outStructType defaultAddrSpace) "out_env"
    ) (prepKernelCodeGen' ctx)
  return m
  where
    outStructType = StructPrimType False $ outEnvStructType (snd ctx)
    inStructType = StructPrimType False $ inEnvStructType (snd ctx)
    

prepKernelCodeGen' 
  :: PrepContext bigEnv f
  -> CodeGen PTX ()
prepKernelCodeGen' (bigEnv, args) = do
  declareAliasScopes (countMutOuts args)
  storeKernelArgs bigEnv args inOperandEnv outOperandEnv TupleIdxSelf TupleIdxSelf
  return_
  where
    outEnvTp = StructPrimType False (outEnvStructType args)
    outOperandEnv = LocalReference (PrimType (PtrPrimType outEnvTp defaultAddrSpace)) "out_env"
    inEnvTp = StructPrimType False (inEnvStructType args)
    inOperandEnv = LocalReference (PrimType (PtrPrimType inEnvTp defaultAddrSpace)) "in_env"


countMutOuts :: SArgs env f -> Int
countMutOuts args = count' args 0
  where
    count' :: SArgs env f -> Int -> Int
    count' (SArgScalar _ :>: sargs) acc = count' sargs acc
    count' (SArgBuffer Mut _ :>: sargs) acc = count' sargs (acc + 1)
    count' (SArgBuffer Out _ :>: sargs) acc = count' sargs (acc + 1)
    count' (SArgBuffer _ _ :>: sargs) acc = count' sargs acc
    count' ArgsNil acc = acc

outEnvStructType :: SArgs env f -> TupR PrimType (KernelArgs f)
outEnvStructType ArgsNil = TupRunit
outEnvStructType (SArgScalar (Var tp _) :>: sargs)
  | Refl <- marshalScalarArg tp = TupRsingle (bufferEltR tp) `TupRpair` outEnvStructType sargs
outEnvStructType (SArgBuffer _ (Var tp _) :>: sargs)
  =
  TupRsingle (PtrPrimType (bufferEltR tp') defaultAddrSpace) `TupRpair` outEnvStructType sargs
  where
      tp' = case tp of
        GroundRbuffer t -> t
        _ -> internalError "Buffer impossible"

inEnvStructType :: SArgs env f -> TupR PrimType (KernelArgPtrs f)
inEnvStructType ArgsNil = TupRunit
inEnvStructType (SArgScalar (Var tp _) :>: sargs)
  | Refl <- marshalScalarArg tp = TupRsingle (PtrPrimType (bufferEltR tp) defaultAddrSpace) `TupRpair` inEnvStructType sargs
inEnvStructType (SArgBuffer _ (Var tp _) :>: sargs)
  =
  TupRsingle (PtrPrimType (PtrPrimType (bufferEltR tp') defaultAddrSpace) defaultAddrSpace) `TupRpair` inEnvStructType sargs
  where
      tp' = case tp of
        GroundRbuffer t -> t
        _ -> internalError "Buffer impossible"


storeKernelArgs :: Env GraphEnv env-> SArgs env f -> Operand (Ptr (Struct structIn)) -> Operand (Ptr (Struct structOut)) -> TupleIdx structIn (KernelArgPtrs f) -> TupleIdx structOut (KernelArgs f) -> CodeGen PTX ()
storeKernelArgs env (SArgScalar (Var tp idx) :>: sargs) inStruct outStruct inStructIdx outStructIdx = do

    ldPtr <- instr' $ GetElementPtr $ gepStruct (PtrPrimType (bufferEltR tp) defaultAddrSpace) inStruct (tupleLeft inStructIdx)
    ptrToScalar <- instrMD' (Load NonVolatile ldPtr Nothing) (bufferMetadata' Nothing)

    -- Load from environment
    value <- case prj' idx env of
      (ScalarVal _ _) -> load NonVolatile tp ptrToScalar Nothing
      _ -> internalError "invalid argument type, expected scalar"
    
    -- Store to struct
    storePtr <- instr' $ GetElementPtr $ gepStruct (bufferEltR tp) outStruct (tupleLeft outStructIdx)
    store NonVolatile tp storePtr value Nothing

    storeKernelArgs env sargs inStruct outStruct (tupleRight inStructIdx) (tupleRight outStructIdx)
storeKernelArgs env (SArgBuffer _ (Var tp idx) :>: sargs) inStruct outStruct inStructIdx outStructIdx = do
    
    let 
      tp' = case tp of
        GroundRbuffer t -> t
        _ -> internalError "Buffer impossible"

    ldPtr <- instr' $ GetElementPtr $ gepStruct (PtrPrimType (PtrPrimType (bufferEltR tp') defaultAddrSpace) defaultAddrSpace) inStruct (tupleLeft inStructIdx)
    ptrToBuffer <- instrMD' (Load NonVolatile ldPtr Nothing) (bufferMetadata' Nothing)

    -- Load from environment
    value <- case prj' idx env of
      (BufferVal _) -> instr' $ Load NonVolatile ptrToBuffer Nothing
      _ -> internalError "invalid argument type, expected buffer"
    
    -- Store to struct
    storePtr <- instr' $ GetElementPtr $ gepStruct (PtrPrimType (bufferEltR tp') defaultAddrSpace) outStruct (tupleLeft outStructIdx)
    _ <- instr' $ Store NonVolatile storePtr value Nothing

    storeKernelArgs env sargs inStruct outStruct (tupleRight inStructIdx) (tupleRight outStructIdx)
storeKernelArgs _ ArgsNil _ _ _ _ = return ()
