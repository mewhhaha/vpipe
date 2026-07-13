module Vpipe.Examples.Plasma (runPlasma) where

import Linear (V2 (..), V4 (..))
import Vpipe.Expr qualified as Expr

import Vpipe.Examples.Common (ExampleOptions)
import Vpipe.Examples.FullscreenShader (FullscreenShader (..), runFullscreenShader)

runPlasma :: ExampleOptions -> IO ()
runPlasma = runFullscreenShader plasma

plasma :: FullscreenShader
plasma =
  FullscreenShader
    { fullscreenShaderLabel = "plasma"
    , fullscreenShaderTitle = "vpipe plasma"
    , fullscreenShaderFragment = shade
    , fullscreenShaderParameters = \_ time -> pure (V4 time 0 0 0)
    }

shade :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
shade inputs uv =
  let time = Expr.x inputs
      centered = uv - Expr.constant (V2 0.5 0.5)
      radius = sqrt (Expr.dot centered centered)
      waves =
        sin (Expr.x centered * Expr.constant 12 + time)
          + sin (Expr.y centered * Expr.constant 15 - time * Expr.constant 1.3)
          + sin ((Expr.x centered + Expr.y centered) * Expr.constant 9 + time * Expr.constant 0.7)
          + sin (radius * Expr.constant 32 - time * Expr.constant 2)
      phase = waves * Expr.constant 0.7
   in Expr.vec4
        (Expr.constant 0.5 + Expr.constant 0.5 * sin (phase + time * Expr.constant 0.3))
        (Expr.constant 0.5 + Expr.constant 0.5 * sin (phase + Expr.constant 2.094))
        (Expr.constant 0.5 + Expr.constant 0.5 * sin (phase + Expr.constant 4.189))
        (Expr.constant 1)
