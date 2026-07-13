module Main (main) where

import Data.Maybe (fromMaybe)
import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), parseExampleOptions, runOffscreenTriangle)

main :: IO ()
main = do
  options <- parseExampleOptions
  let screenshot = fromMaybe "headless.png" (exampleScreenshot options)
  runOffscreenTriangle options{exampleScreenshot = Just screenshot}
