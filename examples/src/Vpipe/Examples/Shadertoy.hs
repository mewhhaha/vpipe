module Vpipe.Examples.Shadertoy (runShadertoy) where

import Linear (V2, V4 (..))
import Vpipe.Expr qualified as Expr

import Vpipe.Examples.Common (ExampleOptions)
import Vpipe.Examples.FullscreenShader (FullscreenShader (..), runFullscreenShader)

runShadertoy :: ExampleOptions -> IO ()
runShadertoy = runFullscreenShader shadertoy

shadertoy :: FullscreenShader
shadertoy =
  FullscreenShader
    { fullscreenShaderLabel = "shadertoy"
    , fullscreenShaderTitle = "vpipe shadertoy"
    , fullscreenShaderFragment = shade
    , fullscreenShaderParameters = \_ time ->
        pure (V4 time (0.5 + 0.18 * cos time) (0.5 + 0.18 * sin time) 0)
    }

shade :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
shade parameters uv =
  let time = Expr.x parameters
      mouse = Expr.vec2 (Expr.y parameters) (Expr.z parameters)
      delta = uv - mouse
      distanceSquared = Expr.dot delta delta
      glow = Expr.clamp (Expr.constant 0.025 / (distanceSquared + Expr.constant 0.008)) (Expr.constant 0) (Expr.constant 1)
      red = Expr.constant 0.5 + Expr.constant 0.5 * sin (Expr.x uv * Expr.constant 9 + time)
      green = Expr.constant 0.5 + Expr.constant 0.5 * sin (Expr.y uv * Expr.constant 11 - time * Expr.constant 1.3)
      blue = Expr.constant 0.5 + Expr.constant 0.5 * sin ((Expr.x uv + Expr.y uv) * Expr.constant 7 + time * Expr.constant 0.7)
   in Expr.vec4
        (Expr.clamp (red + glow) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (green + glow * Expr.constant 0.6) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (blue + glow * Expr.constant 0.25) (Expr.constant 0) (Expr.constant 1))
        (Expr.constant 1)
