module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Shadertoy (runShadertoy)

main :: IO ()
main = parseExampleOptions >>= runShadertoy
