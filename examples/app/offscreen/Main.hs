module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Offscreen (runOffscreen)

main :: IO ()
main = parseExampleOptions >>= runOffscreen
