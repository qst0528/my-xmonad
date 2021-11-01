module XMonadConfig.Common
  ( myNormalBorderColor
  , myFocusedBorderColor
  , myTerminal
  , myModMask
  , myBorderWidth
  , myFocusFollowsMouse
  , myClickJustFocuses
  , myWorkspaces
  , myXPConfig) where

import XMonad
import XMonad.Prompt
import qualified Data.Map        as M

myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myTerminal :: String
myTerminal = "st"

myBorderWidth :: Dimension
myBorderWidth = 4

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myModMask :: KeyMask
myModMask = mod4Mask

myXPConfig :: XPConfig
myXPConfig = def
  { font     = "xft:M PLUS 1 Code:size=24"
  , height   = 64
  , position = CenteredAt 0.3 0.8
  }

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]
