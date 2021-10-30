import XMonad
import XMonad.Util.EZConfig (checkKeymap)

import XMonadConfig.XConfig (myTerminal, myWorkspaces, myBorderWidth)
import XMonadConfig.Input   (myModMask, addMyKeys)



main :: IO ()
main = xmonad $ myConfig

myConfig = addMyKeys def
  { modMask     = myModMask
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  }
