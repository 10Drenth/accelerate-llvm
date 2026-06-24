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
import LLVM.AST.Type.Constant (Constant(NullPtrConstant))
import Data.Array.Accelerate.LLVM.PTX.Compile.Cache
import Data.Array.Accelerate.LLVM.PTX.Compile
import Control.Monad.Reader
import Crypto.Hash.XKCP
import Data.String
import Data.Array.Accelerate.LLVM.PTX.Analysis.Launch



type PrepEnv env = Ptr (SizedArray Word)

type PrepKernel env env' = PrepEnv env -> Ptr (SizedArray Word) -> Ptr (Struct (KernelArgs env')) -> ()

type PrepContext env f = (Env GraphEnv env, SArgs env f)


type family KernelArg a where
  KernelArg (m DIM1 e) = Ptr (BufferEltR e)
  KernelArg (Var' e) = (BufferEltR e)

type family KernelArgs f where
  KernelArgs () = ()
  KernelArgs (t -> f) = (KernelArg t, KernelArgs f)


-- Exists Idx env
-- IPV lijst Tupr gebruiken als mapping naar de kleine environment Tupr
-- TupR (Idx genv) kenv' -> TupIDX kenv kenv'
-- TupleIdx (type van het complete tuple constructie) (type van de waarde op die index)

-- Einde elke dag klein taakje voor volgende dag opschrijven

compilePrep ::  UID -> String -> PrepContext env env' -> LLVM PTX (ObjectR (PrepKernel env env'))
compilePrep uid name ss = do
  dev <- asks ptxDeviceProperties
  let uid' = hashIncrement 3 uid
  m <- prepKernelCodeGen name ss
  obj <- compile uid' (fromString name) (simpleLaunchConfig dev) m
  obj `seq` return ()
  return obj

prepKernelCodeGen
  :: String
  -> PrepContext env' f -> LLVM PTX (Module (PrepKernel env f))
prepKernelCodeGen name ctx = do
  -- | Refl <- marshalFunResultUnit env = do
  (_, m) <- codeGenKernel name 
    ( LLVM.Lam kernelDataRawType "in_env"
    . LLVM.Lam kernelDataRawType "kernel_data"
    . LLVM.Lam (PtrPrimType structType defaultAddrSpace) "out_env"
    ) (prepKernelCodeGen' ctx)
  return m
  where
    kernelDataRawType :: PrimType (Ptr (SizedArray Word))
    kernelDataRawType = PtrPrimType (ArrayPrimType 0 primType) defaultAddrSpace
    structType = StructPrimType False $ outEnvStructType (snd ctx)

prepKernelCodeGen' 
  :: PrepContext bigEnv f
  -> CodeGen PTX ()
prepKernelCodeGen' (bigEnv, args) = do
  declareAliasScopes (countMutOuts args)
  storeKernelArgs bigEnv args operandEnv TupleIdxSelf
  return_
  where
    envTp = StructPrimType False (outEnvStructType args)
    operandEnv = LocalReference (PrimType (PtrPrimType envTp defaultAddrSpace)) "out_env"


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
  = --TupRsingle (PtrPrimType (bufferEltR tp) defaultAddrSpace)
  TupRsingle (PtrPrimType (bufferEltR tp') defaultAddrSpace) `TupRpair` outEnvStructType sargs
  where
      tp' = case tp of
        GroundRbuffer t -> t
        _ -> internalError "Buffer impossible"


storeKernelArgs :: Env GraphEnv env-> SArgs env f -> Operand (Ptr (Struct struct)) -> TupleIdx struct (KernelArgs f) -> CodeGen PTX ()
storeKernelArgs env (SArgScalar (Var tp idx) :>: sargs) struct structIdx = do
  -- | Refl <- scalarReprBase tp = do

    -- let ptrToScalar = undefined -- TODO
    let 
      ptrToScalar = ConstantOperand $ NullPtrConstant $ PrimType 
                  $ PtrPrimType (bufferEltR tp) defaultAddrSpace

    -- Load from environment
    value <- case prj' idx env of
      (ScalarVal _ _) -> load NonVolatile tp ptrToScalar Nothing
      _ -> internalError "invalid argument type, expected scalar"
    
    -- Store to struct
    storePtr <- instr' $ GetElementPtr $ gepStruct (bufferEltR tp) struct (tupleLeft structIdx)
    store NonVolatile tp storePtr value Nothing

    storeKernelArgs env sargs struct (tupleRight structIdx)
storeKernelArgs env (SArgBuffer _ (Var tp idx) :>: sargs) struct structIdx = do
    
    let 
      tp' = case tp of
        GroundRbuffer t -> t
        _ -> internalError "Buffer impossible"

    -- let ptrToBuffer = undefined -- TODO
    let 
      ptrToBuffer = ConstantOperand $ NullPtrConstant $ PrimType 
                  $ PtrPrimType 
                    (PtrPrimType (bufferEltR tp') defaultAddrSpace) 
                  defaultAddrSpace

    -- Load from environment
    value <- case prj' idx env of
      (BufferVal _) -> instr' $ Load NonVolatile ptrToBuffer Nothing
      _ -> internalError "invalid argument type, expected buffer"
    
    -- Store to struct
    storePtr <- instr' $ GetElementPtr $ gepStruct (PtrPrimType (bufferEltR tp') defaultAddrSpace) struct (tupleLeft structIdx)
    _ <- instr' $ Store NonVolatile storePtr value Nothing

    storeKernelArgs env sargs struct (tupleRight structIdx)
storeKernelArgs _ ArgsNil _ _ = return ()

-- type family ReprBaseR t where
--   ReprBaseR Signal = Word
--   ReprBaseR SignalResolver = Word
--   ReprBaseR (Ref t) = ReprBaseR t
--   ReprBaseR (OutputRef t) = ReprBaseR t
--   ReprBaseR (Buffer t) = Ptr (BufferEltR t)
--   ReprBaseR t = t

-- type family ReprBasesR t where
--   ReprBasesR () = ()
--   ReprBasesR (a, b) = (ReprBasesR a, ReprBasesR b)
--   ReprBasesR t = ReprBaseR t

-- -- Representation of values when in memory. See the definitions of ReprBaseR
-- -- and BufferEltR for more information
-- type StorageBaseR t = BufferEltR (ReprBaseR t)
-- type StorageBasesR t = BufferEltR (ReprBasesR t)

-- scalarReprBase :: ScalarType tp -> (tp, BufferEltR tp) :~: (ReprBaseR tp, StorageBaseR tp)
-- scalarReprBase (VectorScalarType _) = Refl
-- scalarReprBase (SingleScalarType (NumSingleType (IntegralNumType tp))) = case tp of
--   TypeInt    -> Refl
--   TypeInt8   -> Refl
--   TypeInt16  -> Refl
--   TypeInt32  -> Refl
--   TypeInt64  -> Refl
--   TypeWord   -> Refl
--   TypeWord8  -> Refl
--   TypeWord16 -> Refl
--   TypeWord32 -> Refl
--   TypeWord64 -> Refl
-- scalarReprBase (SingleScalarType (NumSingleType (FloatingNumType tp))) = case tp of
--   TypeHalf   -> Refl
--   TypeFloat  -> Refl
--   TypeDouble -> Refl



