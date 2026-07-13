module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Cube (runCube)

main :: IO ()
main = parseExampleOptions >>= runCube
