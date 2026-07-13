module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Particles (runParticles)

main :: IO ()
main = parseExampleOptions >>= runParticles
