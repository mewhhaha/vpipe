module AtomicFloat where

import Vpipe.Compute
import Vpipe.Expr (constant)

-- EXPECT: atomicAdd does not support element type Float.
-- EXPECT: Use a StorageBuf Int32 or StorageBuf Word32.
badAtomic :: StorageBuf Float -> ComputeM env ()
badAtomic buffer = atomicAdd buffer (constant 0) (constant 1)
