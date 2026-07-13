module CoerceStorageBuf where

import Data.Coerce (coerce)
import Data.Int (Int32)
import Vpipe.Compute (StorageBuf)

-- EXPECT: Couldn't match type
badStorage :: StorageBuf Float -> StorageBuf Int32
badStorage = coerce
