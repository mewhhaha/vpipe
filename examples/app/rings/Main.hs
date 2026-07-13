module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Rings (runRings)

main :: IO ()
main = parseExampleOptions >>= runRings
