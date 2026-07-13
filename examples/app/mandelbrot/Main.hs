module Main (main) where

import Vpipe.Examples.Common (parseExampleOptions)
import Vpipe.Examples.Mandelbrot (runMandelbrot)

main :: IO ()
main = parseExampleOptions >>= runMandelbrot
