{-# LANGUAGE RebindableSyntax #-}

import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)

import XMonad.Config.Prime
import XMonad.Util.Run            (safeSpawn, safeSpawnProg, runInTerm)
import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Prompt
import XMonad.Prompt.Pass         (passPrompt, passEditPrompt)
import XMonad.Prompt.RunOrRaise   (runOrRaisePrompt)
import XMonad.Prompt.Ssh          (sshPrompt)
import XMonad.Hooks.ManageDocks   (docks, avoidStruts)
import XMonad.Hooks.EwmhDesktops  (ewmhFullscreen, ewmh)
import XMonad.Hooks.StatusBar     (statusBarPropTo,
                                   withEasySB,
                                   StatusBarConfig,
                                   killAllStatusBars,
                                   dynamicSBs)
import XMonad.Hooks.StatusBar.PP  (xmobarPP, PP(..), wrap, shorten, xmobarColor, xmobarStrip)
import XMonad.Hooks.ManageHelpers (composeOne,
                                   (-?>),
                                   doFullFloat,
                                   doCenterFloat,
                                   isFullscreen,
                                   isDialog)
import XMonad.Layout.Fullscreen   (fullscreenSupportBorder)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)

main :: IO ()
main = xmonad $ do
  -- Attributes to set
  normalBorderColor  =: "#dddddd"
  focusedBorderColor =: "#ff0000"
  terminal           =: "st"
  modMask            =: mod4Mask
  borderWidth        =: 4
  focusFollowsMouse  =: False
  clickJustFocuses   =: False
  
  -- Attributes to modify
  manageHook  =+ composeOne [ isFullscreen -?> doFullFloat
                            , isDialog     -?> doCenterFloat
                            ]

  startupHook =+ safeSpawn "picom" ["--daemon"]
  startupHook =+ killAllStatusBars

  resetLayout $ Tall 1 (3/100) (1/2)
  modifyLayout avoidStruts

  apply $ fullscreenSupportBorder . ewmhFullscreen . ewmh . docks

  apply $ dynamicSBs barSpawner

  keys =+
    [ ("M-v"        , runInTerm "" "tmux new-session")
    , ("M-S-v"      , sshPrompt myXPConfig)
    , ("M-e"        , safeSpawn "emacsclient" ["--create-frame"])
    , ("M-i p"      , safeSpawnProg "nyxt-personal")
    , ("M-i w"      , safeSpawnProg "nyxt-work")
    , ("M-i n"      , safeSpawnProg "nyxt-nsfw")
    , ("M-i y"      , safeSpawnProg "youtube")
    , ("M-i m"      , safeSpawnProg "google-meet")
    , ("M-p p"      , passPrompt myXPConfig)
    , ("M-p e"      , passEditPrompt myXPConfig)
    , ("M-<Tab>"    , windows W.focusDown)
    , ("M-S-<Tab>"  , windows W.focusUp)
    , ("M-<Return>" , windows W.shiftMaster)
    , ("M-q"        , kill)
    , ("M-l"        , runOrRaisePrompt myXPConfig)
    , ("M-m r"      , spawn "xmonad --restart")
    , ("M-m q"      , io exitSuccess)
    ]
  withScreens $ do
    sKeys    =: ["a", "r", "s"]
    sActions =: [("M-"  , viewScreen),
                 ("M-S-", sendToScreen)]
  keys =+
    [("M-" ++ otherModMasks ++ key, f sc)
    | (key, sc)          <- zip ["a", "r", "s"] [0..]
    , (otherModMasks, f) <- [("", viewScreen def), ("S-", sendToScreen def)]
    ]

myXPConfig :: XPConfig
myXPConfig = def
  { font     = "xft:M PLUS 1 Code:size=24"
  , height   = 64
  , position = CenteredAt 0.3 0.8
  }

xmobarMain :: StatusBarConfig
xmobarMain = statusBarPropTo "_XMONAD_LOG_0" "xmobar -x 0 ~/.config/xmobar/xmobarrc_main" (pure $ xmobarMainPP 0)
xmobarSub1 :: StatusBarConfig
xmobarSub1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 1 ~/.config/xmobar/xmobarrc_sub1" (pure $ xmobarMainPP 1)
xmobarSub2 :: StatusBarConfig
xmobarSub2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 2 ~/.config/xmobar/xmobarrc_sub2" (pure $ xmobarMainPP 2)

xmobarMainPP :: ScreenId -> PP
xmobarMainPP = \s -> xmobarPP
  { ppOrder  = \[ws, l, _, wins] -> [ws, l, wins]
  , ppSort   = getSortByXineramaRule
  , ppExtras = [
                logTitlesOnScreen s formatFocused formatUnfocused
               ]
  }
  where
    formatFocused   = wrap "[" "]" . xmobarColor "#ff79c6" "" . shorten 60 . xmobarStrip
    formatUnfocused = wrap "(" ")" . xmobarColor "#bd93f9" "" . shorten 35 . xmobarStrip

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure $ xmobarMain
barSpawner 1 = pure $ xmobarSub1
barSpawner 2 = pure $ xmobarSub2
barSpawner _ = mempty
