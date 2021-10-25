module XMonadConfig.Input
  ( myModMask
  , myKeys) where

import System.Exit (exitSuccess)
import XMonad
import XMonad.Util.EZConfig (mkKeymap)

myModMask :: KeyMask
myModMask = mod4Mask

myKeys = \c -> mkKeymap c
  [ -- Terminal and Emacs
    ("M-v", spawn $ XMonad.terminal c)
  , ("M-m q", io exitSuccess)
  ]
