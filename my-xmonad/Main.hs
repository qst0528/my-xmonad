import XMonad
import XMonad.Util.EZConfig (checkKeymap)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

import XMonadConfig.XConfig (myTerminal, myWorkspaces,
                             myBorderWidth, myFocusFollowsMouse,
                             myClickJustFocuses)
import XMonadConfig.Input   (myModMask, addMyKeys)

main :: IO ()
main = xmonad $ ewmhFullscreen . ewmh $ myConfig

myConfig = addMyKeys def
  { modMask     = myModMask
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses  = myClickJustFocuses
  }
