module XMonadConfig (myXConfig) where

import XMonad

import XMonad.Hooks.ManageDocks  (docks)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen, ewmh)

import XMonadConfig.Common ( myNormalBorderColor
                           , myFocusedBorderColor
                           , myTerminal
                           , myModMask
                           , myBorderWidth
                           , myFocusFollowsMouse
                           , myClickJustFocuses
                           , myWorkspaces)
import XMonadConfig.Hook   (myManageHook, myStartupHook)
import XMonadConfig.Bind   (addMyKeys)

myXConfig = docks . addMyKeys . ewmhFullscreen . ewmh $
  def
  { -- Attributes
    normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , terminal           = myTerminal
  , modMask            = myModMask
  , borderWidth        = myBorderWidth
  , focusFollowsMouse  = myFocusFollowsMouse
  , clickJustFocuses   = myClickJustFocuses
    -- Hooks
  , manageHook      = myManageHook
  , workspaces      = myWorkspaces
  , startupHook     = myStartupHook
  }
