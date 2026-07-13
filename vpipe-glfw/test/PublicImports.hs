{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Vpipe.Context (VpipeConfig, defaultVpipeConfig)
import Vpipe.GLFW (Key, KeyState, Window, getKey, pattern KeyEscape, pattern KeyPressed)

escapeKeyState :: Window -> IO KeyState
escapeKeyState window = getKey window KeyEscape

isEscapePressed :: KeyState -> Bool
isEscapePressed state = state == KeyPressed

defaultEscapeKey :: Key
defaultEscapeKey = KeyEscape

defaultConfig :: VpipeConfig
defaultConfig = defaultVpipeConfig

main :: IO ()
main = escapeKeyState `seq` isEscapePressed `seq` defaultEscapeKey `seq` defaultConfig `seq` pure ()
