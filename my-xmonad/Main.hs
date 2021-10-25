import XMonad

import XMonadConfig.XConfig (myTerminal, myWorkspaces, myBorderWidth)
import XMonadConfig.Input    (myModMask, myKeys)

main :: IO ()
main = xmonad def
  { modMask     = myModMask
  , keys        = myKeys
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  }
