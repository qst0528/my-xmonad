module XMonadConfig.XConfig
  ( myTerminal
  , myWorkspaces
  , myBorderWidth
  , myFocusFollowsMouse
  , myClickJustFocuses
  , myXPConfig
  ) where
import XMonad (Dimension)
import XMonad.Prompt

myTerminal :: String
myTerminal = "st"

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]

myBorderWidth :: Dimension
myBorderWidth = 4

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myXPConfig :: XPConfig
myXPConfig = def
  { font     = "xft:M PLUS 1 Code:size=24"
  , height   = 64
  , position = CenteredAt 0.3 0.8
  }
