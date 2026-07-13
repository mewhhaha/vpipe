{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | Task 09 Prototype B spike. This file deliberately stays outside the
library: it records the alternative that was evaluated, not supported API.
-}
module PrototypeB where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)

data ResourceKind = Uniform | Texture | Storage

data Binding = Binding Symbol ResourceKind Type

data Pipeline (layout :: [Binding]) env = Pipeline

data Handle (kind :: ResourceKind) value = Handle Int

class ResolveBinding (name :: Symbol) (kind :: ResourceKind) value env where
  resolveBinding :: Proxy name -> env -> Handle kind value

data ExampleEnvironment = ExampleEnvironment
  { cameraHandle :: Handle 'Uniform Float
  , outputHandle :: Handle 'Storage Float
  }

instance ResolveBinding "camera" 'Uniform Float ExampleEnvironment where
  resolveBinding _ = cameraHandle

instance ResolveBinding "output" 'Storage Float ExampleEnvironment where
  resolveBinding _ = outputHandle

type ExampleLayout =
  '[ 'Binding "camera" 'Uniform Float
   , 'Binding "output" 'Storage Float
   ]

example :: Pipeline ExampleLayout ExampleEnvironment
example = Pipeline

-- Adding @'Binding "albedo" 'Texture Float@ to ExampleLayout requires a new
-- ResolveBinding instance. In the full prototype, recursive layout resolution
-- surfaced that missing instance rather than the resource usage at the call
-- site; this was the principal error-quality regression versus Prototype A.
