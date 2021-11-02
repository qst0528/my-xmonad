module XMonadConfig.Hook (myManageHook, myStartupHook) where

import XMonad
import XMonad.Config.Prime        (ManageHook)
import XMonad.Util.Run      (safeSpawn)
import XMonad.Layout.Fullscreen   (fullscreenManageHook)
import XMonad.Hooks.StatusBar     (statusBarProp, withEasySB)
import XMonad.Hooks.StatusBar.PP  (xmobarPP)
import XMonad.Hooks.ManageHelpers (composeOne,
                                   (-?>),
                                   doFullFloat,
                                   doCenterFloat,
                                   isFullscreen,
                                   isDialog)

myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+> composeOne
  [
    isFullscreen -?> doFullFloat
  , isDialog     -?> doCenterFloat
  ]

myStartupHook :: X ()
myStartupHook = safeSpawn "xmessage" ["XMonad restarted!"]
