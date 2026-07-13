module Vpipe.Examples.Mandelbrot (runMandelbrot) where

import Linear (V2, V4 (..))
import Vpipe.Expr ((<.), (<=.))
import Vpipe.Expr qualified as Expr
import Vpipe.GLFW (Key, KeyState, Window, getKey)
import Vpipe.GLFW qualified as GLFW

import Vpipe.Examples.Common (ExampleOptions)
import Vpipe.Examples.FullscreenShader (FullscreenShader (..), runFullscreenShader)

runMandelbrot :: ExampleOptions -> IO ()
runMandelbrot = runFullscreenShader mandelbrot

mandelbrot :: FullscreenShader
mandelbrot =
  FullscreenShader
    { fullscreenShaderLabel = "mandelbrot"
    , fullscreenShaderTitle = "vpipe mandelbrot — arrows/A/D pan, W/S zoom"
    , fullscreenShaderFragment = shade
    , fullscreenShaderParameters = parameters
    }

parameters :: Maybe Window -> Float -> IO (V4 Float)
parameters Nothing time = pure (V4 time 0 0 1)
parameters (Just window) time = do
  horizontal <- keyAxis window GLFW.KeyLeft GLFW.KeyRight
  horizontalLetters <- keyAxis window GLFW.KeyA GLFW.KeyD
  vertical <- keyAxis window GLFW.KeyDown GLFW.KeyUp
  zoom <- keyAxis window GLFW.KeyS GLFW.KeyW
  pure (V4 time (horizontal + horizontalLetters) vertical (2 ** (-zoom)))

keyAxis :: Window -> Key -> Key -> IO Float
keyAxis window negative positive = do
  negativeState <- getKey window negative
  positiveState <- getKey window positive
  pure (keyValue positiveState - keyValue negativeState)

keyValue :: KeyState -> Float
keyValue state
  | state == GLFW.KeyPressed = 1
  | state == GLFW.KeyRepeating = 1
  | otherwise = 0

shade :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
shade inputs uv =
  let time = Expr.x inputs
      scale = Expr.w inputs
      centerX = Expr.constant (-0.5) + Expr.y inputs * scale * Expr.constant 0.35
      centerY = Expr.z inputs * scale * Expr.constant 0.35
      coordinate =
        Expr.vec2
          ((Expr.x uv - Expr.constant 0.5) * Expr.constant 3 * scale + centerX)
          ((Expr.y uv - Expr.constant 0.5) * Expr.constant 2 * scale + centerY)
      finalState = Expr.whileE (\state -> Expr.w state <. maximumIterations) (mandelbrotStep coordinate) (Expr.constant (V4 0 0 0 0))
      escapedIterations = Expr.z finalState
      intensity = escapedIterations / maximumIterations
      red = Expr.constant 0.5 + Expr.constant 0.5 * cos (Expr.constant 6.28318 * intensity + time * Expr.constant 0.35)
      green = Expr.constant 0.5 + Expr.constant 0.5 * cos (Expr.constant 6.28318 * intensity + Expr.constant 2.1 + time * Expr.constant 0.23)
      blue = Expr.constant 0.5 + Expr.constant 0.5 * cos (Expr.constant 6.28318 * intensity + Expr.constant 4.2 - time * Expr.constant 0.18)
      interior = escapedIterations Expr.>=. maximumIterations
   in Expr.ifThenElseE interior (Expr.constant (V4 0.015 0.01 0.04 1)) (Expr.vec4 red green blue (Expr.constant 1))

mandelbrotStep :: Expr.F (V2 Float) -> Expr.F (V4 Float) -> Expr.F (V4 Float)
mandelbrotStep coordinate state =
  let zx = Expr.x state
      zy = Expr.y state
      inside = zx * zx + zy * zy <=. Expr.constant 4
      nextX = zx * zx - zy * zy + Expr.x coordinate
      nextY = Expr.constant 2 * zx * zy + Expr.y coordinate
      active = Expr.ifE inside (Expr.constant 1) (Expr.constant 0)
   in Expr.vec4
        (Expr.ifThenElseE inside nextX zx)
        (Expr.ifThenElseE inside nextY zy)
        (Expr.z state + active)
        (Expr.w state + Expr.constant 1)

maximumIterations :: Expr.F Float
maximumIterations = Expr.constant 56
