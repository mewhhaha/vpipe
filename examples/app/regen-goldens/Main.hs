module Main (main) where

import Vpipe.Examples.Common (ExampleOptions (..), runOffscreenTriangle)
import Vpipe.Examples.Cube (runCube)
import Vpipe.Examples.Mandelbrot (runMandelbrot)
import Vpipe.Examples.Offscreen (runOffscreen)
import Vpipe.Examples.Particles (runParticles)
import Vpipe.Examples.Plasma (runPlasma)
import Vpipe.Examples.Rings (runRings)
import Vpipe.Examples.Shadertoy (runShadertoy)

main :: IO ()
main = do
  runOffscreenTriangle (goldenOptions "triangle")
  runOffscreenTriangle (goldenOptions "headless")
  runCube (goldenOptions "cube")
  runOffscreen (goldenOptions "offscreen")
  runParticles (goldenOptions "particles")
  runShadertoy (goldenOptions "shadertoy")
  runMandelbrot (goldenOptions "mandelbrot")
  runPlasma (goldenOptions "plasma")
  runRings (goldenOptions "rings")

goldenOptions :: String -> ExampleOptions
goldenOptions name = ExampleOptions (Just 2) (Just ("examples/golden/" <> name <> ".png"))
