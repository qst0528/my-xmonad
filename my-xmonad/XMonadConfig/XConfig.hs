module XMonadConfig.XConfig
  ( myTerminal
  , myWorkspaces
  , myBorderWidth
  ) where
import XMonad (Dimension)

myTerminal :: String
myTerminal = "st"

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]

myBorderWidth :: Dimension
myBorderWidth = 4
