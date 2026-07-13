module UnsupportedStorageElement where

import Linear (V3)
import Vpipe.Compute
import Vpipe.Pipeline (StorageBuffer)

data Environment

source :: Environment -> StorageBuffer (V3 Float)
source = undefined

-- EXPECT: Compute storage resources do not support element type
-- EXPECT: Use Float, Int32, Word32, V2 Float, or V4 Float.
badStorage :: ComputeM Environment (StorageBuf (V3 Float))
badStorage = storageBuffer source
