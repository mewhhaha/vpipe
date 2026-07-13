{- | Type-safe Vulkan 1.3 graphics and compute.

The umbrella module is intentionally documentation-only: import the small
capability modules a program uses so names such as buffer and image usages stay
unambiguous. A minimal context probe is:

@
module Main (main) where

import Vpipe.Context (contextDeviceName, defaultVpipeConfig, withVpipe)

main :: IO ()
main = withVpipe defaultVpipeConfig (putStrLn . contextDeviceName)
@

The source distribution includes complete graphics and compute tutorials under
@docs/tutorials@.
-}
module Vpipe where
