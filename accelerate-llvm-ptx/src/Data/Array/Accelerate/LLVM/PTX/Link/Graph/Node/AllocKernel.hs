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
{-# LANGUAGE InstanceSigs #-}

module Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.AllocKernel where


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
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Environment (GraphEnv (..), GraphMemory, MIdx)
import Data.Array.Accelerate.AST.Schedule.Uniform hiding (Body)
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
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.IR (Operands (OP_Int32))
import Data.Array.Accelerate.LLVM.CodeGen.Constant (scalar)
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Node.Kernel
import Data.Array.Accelerate.LLVM.PTX.Link.Graph.Marshal
import Foreign (castPtr)
import GHC.IO (unsafePerformIO)
import Data.Array.Accelerate.LLVM.PTX.State
import Data.ByteString.Lazy (ByteString)
import Data.Array.Accelerate.LLVM.CodeGen.Base
import LLVM.AST.Type.Function
import LLVM.AST.Type.Name
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Debug.Trace
import Data.Array.Accelerate.Representation.Elt (scalarTypeSize)

data AllocNode = AllocNode
  { inputDims       :: [MIdx]
  , bufferAllocDest :: MIdx
  , allocKernel     :: KernelPhase
  }
  deriving (Show)

allocNodeMarshalRep
  :: AllocNode
  -> Marshal MarshalData
  -- -> Marshal (Marshal (Marshal 
  -- ( MIdx
  -- , MStorable Word32 )
  -- , Marshal [MIdx] )
  -- , KernelPhase )
allocNodeMarshalRep (AllocNode args dest kern) = 
  let argsCount = MStorable (Prelude.fromIntegral $ length args :: Word32)
  in trace ("Alloc rep info:" ++ 
  "\nwrite_index sa: " ++ show (marshalSize dest) ++ ", " ++ show (marshalAlignment dest) ++
  "\narg_count sa: " ++ show (marshalSize argsCount) ++ ", " ++ show (marshalAlignment argsCount) ++
  "\narg_indices sa: " ++ show (marshalSize (Marshal args)) ++ ", " ++ show (marshalAlignment (Marshal args)) ++
  "\nalloc_kernel sa: " ++ show (marshalSize kern) ++ ", " ++ show (marshalAlignment kern) 
    ++ "\n  path: " ++ show (modulePath kern)
    ++ "\n  symbol: " ++ show (kernelSymbol kern)
  )
  $ structMarshalData 
  [ toField dest
  , toField argsCount
  , toField $ Marshal args
  , toField kern
  ]
  -- $  Marshal (Marshal (Marshal 
  -- ( dest
  -- , argsCount)
  -- , Marshal args)
  -- , kern)

instance MarshalToC AllocNode where
  marshalSize :: AllocNode -> Int
  marshalSize = marshalSize . allocNodeMarshalRep
  marshalAlignment :: AllocNode -> Int
  marshalAlignment = marshalAlignment . allocNodeMarshalRep
  marshalWrite :: Ptr AllocNode -> AllocNode -> IO ()
  marshalWrite ptr v = marshalWrite (castPtr ptr) $ allocNodeMarshalRep v


makeAllocNode :: MIdx -> ByteString -> GenArgs env sh e -> AllocNode
makeAllocNode dst seed g@(env,shr,vars,_) = let
  obj = (unsafePerformIO $ evalPTX defaultTarget $ compileAlloc (hashlazy seed) "buffer_alloc_kernel" g)
  in AllocNode
    (inputIndices shr $ prjVars vars env)
    dst
    (KernelPhase (objPath obj) (objSym obj) 1 1 0)



inputIndices :: ShapeR sh -> Distribute GraphEnv sh -> [MIdx]
inputIndices ShapeRz _ = []
inputIndices (ShapeRsnoc shr) (sh, ScalarVal idx _) = idx : inputIndices shr sh

type family InputPtrs ptrs where
  InputPtrs (ptrs, Int) = (InputPtrs ptrs, Ptr Int32)
  InputPtrs () = ()

type GenArgs env sh e = ( Env GraphEnv env
                        , ShapeR sh
                        , ExpVars env sh
                        , ScalarType e
                        )
type AllocKernel e sh = Ptr (Struct (InputPtrs sh)) -- Ptrs to input dims
                  -> Ptr (Ptr (BufferEltR e)) -- Ptr to destination 
                  -> Ptr Int32 -- Size write destination 
                  -> ()


compileAlloc ::  UID -> String -> GenArgs env sh e -> LLVM PTX (ObjectR (AllocKernel e sh))
compileAlloc uid name gen = do
  dev <- asks ptxDeviceProperties
  m <- allocKernelCodeGen name gen
  obj <- compile uid (fromString name) (simpleLaunchConfig dev) m
  obj `seq` return ()
  return obj

allocKernelCodeGen :: String -> GenArgs env sh e -> LLVM PTX (Module (AllocKernel e sh))
allocKernelCodeGen name g@(env,shr,_,tp) | Refl <- marshalFunResultUnit' env = do
  (_, m) <- codeGenKernel name
    ( LLVM.Lam (PtrPrimType inDimsType defaultAddrSpace) "in_dims"
    . LLVM.Lam (PtrPrimType (PtrPrimType (bufferEltR tp) defaultAddrSpace) defaultAddrSpace) "out_buffer_destination"
    . LLVM.Lam (PtrPrimType (ScalarPrimType scalarType) defaultAddrSpace) "out_size_destination"
    ) (allocCodeGen g)
  return m
  where
    inDimsType = StructPrimType False (inEnvStructType shr)

inEnvStructType :: ShapeR sh -> TupR PrimType (InputPtrs sh)
inEnvStructType ShapeRz = TupRunit
inEnvStructType (ShapeRsnoc shr) =
  inEnvStructType shr `TupRpair` TupRsingle (PtrPrimType (ScalarPrimType scalarType) defaultAddrSpace)

allocCodeGen :: GenArgs env sh e -> CodeGen PTX ()
allocCodeGen (env,shr,vars,tp) = let
  dimsTp = PtrPrimType (StructPrimType False (inEnvStructType shr)) defaultAddrSpace
  dimsOp = LocalReference (PrimType dimsTp) "in_dims"
  bufferDstTp = PtrPrimType (PtrPrimType (bufferEltR tp) defaultAddrSpace) defaultAddrSpace
  bufferDstOp = LocalReference (PrimType bufferDstTp) "out_buffer_destination"
  sizeDstTp :: PrimType (Ptr Int32)
  sizeDstTp = PtrPrimType (ScalarPrimType scalarType) defaultAddrSpace
  sizeDstOp = LocalReference (PrimType sizeDstTp) "out_size_destination"


  in do
    declareAliasScopes 4
    -- Calculate size
    sz <- calcSizeCodeGen tp dimsOp shr (prjVars vars env) TupleIdxSelf
    -- Store size
    store NonVolatile scalarType sizeDstOp sz Nothing
    sz' <- instr' $ Ext (IntegralBoundedType TypeInt32) (IntegralBoundedType TypeInt64) sz
    -- Allocate Buffer
    bufferPtr <- malloc tp sz'
    -- let bufferPtr = constant (TupRsingle )
    -- Store Ptr to Buffer
    _ <- instr' $ Store NonVolatile bufferDstOp bufferPtr Nothing

    -- let testTp = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt8
    -- _ <- instr' $ Store NonVolatile (ptrCast (ScalarPrimType testTp) bufferPtr) (scalar testTp 8) Nothing

    return_

malloc :: ScalarType e -> Operand Int64 -> CodeGen PTX (Operand (Ptr (BufferEltR e)))
malloc tp x =
  call' (lamUnnamed primType $ LLVM.AST.Type.Function.Body (PrimType $ PtrPrimType (bufferEltR tp) defaultAddrSpace) Nothing (Label "malloc"))
       (ArgumentsCons x [] ArgumentsNil) []

calcSizeCodeGen :: ScalarType e -> Operand (Ptr (Struct struct)) -> ShapeR sh -> Distribute GraphEnv sh -> TupleIdx struct (InputPtrs sh) -> CodeGen PTX (Operand Int32)
calcSizeCodeGen tp _ ShapeRz _ _ = trace ("Kernel allocation of type: " ++ show tp) $ return (scalar scalarType (Prelude.fromIntegral $ scalarTypeSize tp))
calcSizeCodeGen tp dimsOp (ShapeRsnoc shr') (v, ScalarVal _ _) tupIdx = do

    recV <- calcSizeCodeGen tp dimsOp shr' v (tupleLeft tupIdx)

    dimPtrIdx <- instr' $ GetElementPtr $ gepStruct (PtrPrimType (ScalarPrimType scalarType) defaultAddrSpace) dimsOp (tupleRight tupIdx)
    dimPtr <- instr' $ Load NonVolatile dimPtrIdx Nothing
    dim <- instr' $ Load NonVolatile dimPtr Nothing

    instr' $ Mul numType dim recV


marshalFunResultUnit' :: Env GraphEnv env -> LLVM.Result (MarshalFun env) :~: ()
marshalFunResultUnit' Empty = Refl
marshalFunResultUnit' (Push env _) = marshalFunResultUnit' env

