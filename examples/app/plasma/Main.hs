module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Plasma (runPlasma)

main :: IO ()
main = parseExampleOptions >>= runPlasma
