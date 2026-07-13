{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Vpipe.Compute.IR.Internal (
  ComputeM (..),
  ComputeRecorder (..),
  ComputeStatement (..),
  StorageBuf (..),
  StorageResource (..),
  PushResource (..),
  StorageElement (..),
  StorageElementSupported,
  AtomicInteger,
  AtomicIntegerSupported,
  ComputeCompileError (..),
  emptyComputeRecorder,
) where

import Control.Monad.State.Strict (StateT)
import Data.Int (Int32)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Linear (V2, V4)
import Vpipe.Buffer.Format (BufferFormat (..), FieldLayout, HostFormat)
import Vpipe.Expr.Internal (ShaderTy (..), ShaderValue, SomeExpr)
import Vpipe.Expr.Reify (NodeId)
import Vpipe.Pipeline.Internal (StorageBuffer)
import Vpipe.SpirV.Codegen (CodegenError)

newtype ComputeM env a = ComputeM
  { unComputeM :: StateT (ComputeRecorder env) (Either ComputeCompileError) a
  }
  deriving newtype (Functor, Applicative, Monad)

data StorageBuf a = StorageBuf
  { storageBufSymbol :: String
  , storageBufType :: ShaderTy
  , storageBufLayout :: FieldLayout
  }
  deriving stock (Eq, Show)

type role StorageBuf nominal

data ComputeStatement
  = WriteStatement String SomeExpr SomeExpr
  | AtomicAddStatement String SomeExpr SomeExpr
  | WhenStatement SomeExpr [ComputeStatement]

data StorageResource env = forall a. (StorageElement a) => StorageResource
  { storageResourceSymbol :: String
  , storageResourceAccessor :: env -> StorageBuffer a
  , storageResourceType :: ShaderTy
  , storageResourceLayout :: FieldLayout
  }

data PushResource env = forall a. (StorageElement a) => PushResource
  { pushResourceSymbol :: String
  , pushResourceAccessor :: env -> a
  , pushResourceType :: ShaderTy
  , pushResourceLayout :: FieldLayout
  }

data ComputeRecorder env = ComputeRecorder
  { computeStorageResources :: [StorageResource env]
  , computePushResources :: [PushResource env]
  , computeStatements :: [ComputeStatement]
  }

emptyComputeRecorder :: ComputeRecorder env
emptyComputeRecorder = ComputeRecorder [] [] []

type family StorageElementSupported a :: Constraint where
  StorageElementSupported Float = ()
  StorageElementSupported Int32 = ()
  StorageElementSupported Word32 = ()
  StorageElementSupported (V2 Float) = ()
  StorageElementSupported (V4 Float) = ()
  StorageElementSupported unsupported =
    TypeError
      ( 'Text "Compute storage resources do not support element type "
          ':<>: 'ShowType unsupported
          ':<>: 'Text ". Use Float, Int32, Word32, V2 Float, or V4 Float."
      )

class (ShaderValue a, BufferFormat a, HostFormat a ~ a) => StorageElement a where
  storageElementType :: proxy a -> ShaderTy
  storageElementLayout :: proxy a -> FieldLayout
  storageElementLayout _ = bufferFieldLayout (Proxy @a)

instance StorageElement Float where storageElementType _ = TyFloat
instance StorageElement Int32 where storageElementType _ = TyInt
instance StorageElement Word32 where storageElementType _ = TyWord
instance StorageElement (V2 Float) where storageElementType _ = TyVector 2
instance StorageElement (V4 Float) where storageElementType _ = TyVector 4

type family AtomicIntegerSupported a :: Constraint where
  AtomicIntegerSupported Int32 = ()
  AtomicIntegerSupported Word32 = ()
  AtomicIntegerSupported unsupported =
    TypeError
      ( 'Text "atomicAdd does not support element type "
          ':<>: 'ShowType unsupported
          ':<>: 'Text ". Use a StorageBuf Int32 or StorageBuf Word32."
      )

class (StorageElement a) => AtomicInteger a
instance AtomicInteger Int32
instance AtomicInteger Word32

data ComputeCompileError
  = InvalidDispatch String
  | WorkgroupCountOverflow
  | InvalidWorkload String
  | PushConstantLimitExceeded Int
  | ComputeRootArityMismatch Int Int
  | ComputeRootMismatch
      { computeReifiedRoots :: [NodeId]
      , computeActionRoots :: [NodeId]
      }
  | ComputeCodegenError CodegenError
  deriving stock (Eq, Show)
