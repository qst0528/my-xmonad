module XMonadConfig.Input
  ( myModMask
  , addMyKeys) where

import System.Exit (exitSuccess)
import XMonad

import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)
import XMonad.Util.NamedActions (addName, NamedAction, (^++^), HasName, xMessage, addDescrKeys')
import XMonad.Util.EZConfig (mkKeymap, mkNamedKeymap)
import XMonad.Util.Run      (safeSpawn, safeSpawnProg)

myModMask :: KeyMask
myModMask = mod4Mask

addMyKeys :: XConfig l -> XConfig l
addMyKeys = addDescrKeys' ((myModMask, xK_h), xMessage) myKeys

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys = \c -> mkNamedKeymap c $
  [ -- Terminal and Emacs
    ("M-v"       , addName "Open Terminal" $ safeSpawnProg $ XMonad.terminal c)
  , ("M-e"       , addName "Open Emacs" $ safeSpawn "emacsclient" ["--create-frame"])
  , ("M-<Tab>"   , addName "Focus Next Window" $ windows W.focusDown)
  , ("M-S-<Tab>" , addName "Focus Previous Window" $ windows W.focusUp)
  , ("M-m r"     , addName "Restart XMonad" $ spawn "xmonad --restart")
  , ("M-m q"     , addName "Quit XMonad" $ io exitSuccess)
  ]
  ^++^
  [
   (("M-" ++ otherModMasks ++ key), addName "Screen" $ f sc)
      | (key, sc)          <- zip ["a", "r", "s"] [0..]
      , (otherModMasks, f) <- [("", viewScreen def), ("S-", sendToScreen def)]
  ]
