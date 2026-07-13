module Vpipe.Examples.Rings (runRings) where

import Linear (V2 (..), V4 (..))
import Vpipe.Expr qualified as Expr

import Vpipe.Examples.Common (ExampleOptions)
import Vpipe.Examples.FullscreenShader (FullscreenShader (..), runFullscreenShader)

runRings :: ExampleOptions -> IO ()
runRings = runFullscreenShader rings

rings :: FullscreenShader
rings =
  FullscreenShader
    { fullscreenShaderLabel = "rings"
    , fullscreenShaderTitle = "vpipe interference rings"
    , fullscreenShaderFragment = shade
    , fullscreenShaderParameters = \_ time -> pure (V4 time 0 0 0)
    }

shade :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
shade inputs uv =
  let time = Expr.x inputs
      point = uv - Expr.constant (V2 0.5 0.5)
      firstCenter = Expr.vec2 (sin time * Expr.constant 0.22) (cos (time * Expr.constant 1.3) * Expr.constant 0.18)
      secondCenter = Expr.vec2 (cos (time * Expr.constant 0.8) * Expr.constant 0.24) (sin (time * Expr.constant 1.1) * Expr.constant 0.2)
      thirdCenter = Expr.vec2 (sin (time * Expr.constant 0.6 + Expr.constant 2) * Expr.constant 0.16) (cos (time * Expr.constant 0.9 + Expr.constant 1) * Expr.constant 0.24)
      firstDistance = sqrt (Expr.dot (point - firstCenter) (point - firstCenter))
      secondDistance = sqrt (Expr.dot (point - secondCenter) (point - secondCenter))
      thirdDistance = sqrt (Expr.dot (point - thirdCenter) (point - thirdCenter))
      interference =
        sin (firstDistance * Expr.constant 42 - time * Expr.constant 3)
          + sin (secondDistance * Expr.constant 38 - time * Expr.constant 2.4)
          + sin (thirdDistance * Expr.constant 46 + time * Expr.constant 2.7)
      glow distance = Expr.clamp (Expr.constant 0.004 / (distance * distance + Expr.constant 0.002)) (Expr.constant 0) (Expr.constant 1)
      firstGlow = glow firstDistance
      secondGlow = glow secondDistance
      thirdGlow = glow thirdDistance
      base = Expr.constant 0.5 + interference * Expr.constant 0.16
   in Expr.vec4
        (Expr.clamp (base + firstGlow * Expr.constant 0.75) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (Expr.constant 0.3 + base * Expr.constant 0.5 + secondGlow * Expr.constant 0.7) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (Expr.constant 0.25 + base * Expr.constant 0.6 + thirdGlow * Expr.constant 0.8) (Expr.constant 0) (Expr.constant 1))
        (Expr.constant 1)
