{-# LANGUAGE RebindableSyntax #-}

import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)

import XMonad.Config.Prime
import XMonad.Util.Run            (safeSpawn, safeSpawnProg, unsafeSpawn, runInTerm)
import XMonad.Util.Loggers
import XMonad.Util.Cursor         (setDefaultCursor)
import XMonad.Util.Hacks as Hacks
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)
import XMonad.Prompt
import XMonad.Prompt.Unicode      (typeUnicodePrompt)
import XMonad.Prompt.Pass         (passPrompt, passEditPrompt)
import XMonad.Prompt.RunOrRaise   (runOrRaisePrompt)
import XMonad.Prompt.Ssh          (sshPrompt)
import XMonad.Actions.Search as S (promptSearch,
                                  selectSearch,
                                  SearchEngine,
                                  wikipedia,
                                  github,
                                  ebay,
                                  dictionary,
                                  amazon,
                                  hoogle,
                                  maps,
                                  google)
import XMonad.Hooks.ManageDocks   (docks, avoidStruts)
import XMonad.Hooks.EwmhDesktops  (ewmhFullscreen, ewmh)
import XMonad.Hooks.StatusBar     (statusBarPropTo,
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
import XMonad.Layout.Accordion    (Accordion(..))
-- import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen, verticalScreenOrderer)

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
  startupHook =+ setDefaultCursor xC_left_ptr
  startupHook =+ killAllStatusBars

  resetLayout $ Tall 1 (3/100) (1/2) ||| Accordion
  modifyLayout avoidStruts

  apply $ fullscreenSupportBorder . ewmhFullscreen . ewmh . docks . Hacks.javaHack

  apply $ dynamicSBs barSpawner

  keys =- ["M-<Space>"]
  keys =+
    [ ("M-v"        , runInTerm "" "tmux new-session")
    , ("M-S-v"      , sshPrompt myXPConfig)
    , ("M-e"        , safeSpawn "emacsclient" ["--create-frame", "--eval", "(dired \"~\")"])
    , ("M-x"        , runOrRaisePrompt myXPConfig)
    ]

  keys =+
    [ ("M-i p"      , safeSpawnProg "nyxt-personal")
    , ("M-i w"      , safeSpawnProg "nyxt-work")
    , ("M-i n"      , safeSpawnProg "nyxt-nsfw")
    , ("M-i y"      , safeSpawnProg "youtube")
    , ("M-i m"      , safeSpawnProg "google-meet")
    ]

  keys =+
    [ ("M-l " ++ k  , S.promptSearch myXPConfig f) | (k, f) <- searchList ]

  keys =+
    [ ("M-w l"      , sendMessage NextLayout)
    , ("M-<Tab>"    , windows W.focusDown)
    , ("M-S-<Tab>"  , windows W.focusUp)
    , ("M-<Return>" , windows W.shiftMaster)
    ]

  keys =+
    [ ("M-p p"      , passPrompt myXPConfig)
    , ("M-p e"      , passEditPrompt myXPConfig)
    , ("M-u"        , typeUnicodePrompt "/usr/share/unicode/UnicodeData.txt" myXPConfig)
    ]
    
  keys =+
    [ ("M-q"        , kill)
    , ("M-S-q"      , withFocused $ \w -> spawn ("xkill -id " ++ show w))
    , ("<Print>"    , spawn "scrot --focused ~/Pictures/ScreenShots/%F_%H%M%S%Z_window.png --exec 'optipng -o4 $f'")
    , ("M-m s"      , spawn "scrot ~/Pictures/ScreenShots/%F_%H%M%S%Z.png --exec 'optipng -o3 $f'")
    , ("M-m r"      , unsafeSpawn "xmonad --restart")
    , ("M-m q"      , io exitSuccess)
    ]
    
  withScreens $ do
    sKeys    =: ["r", "s", "t"]

myXPConfig :: XPConfig
myXPConfig = def
  { font     = "xft:DejaVu Sans Mono:size=24"
  , height   = 64
  , position = CenteredAt 0.3 0.8
  }

searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.google)
             , ("h", S.hoogle)
             , ("w", S.wikipedia)
             , ("a", S.amazon)
             , ("e", S.ebay)
             , ("d", S.dictionary)
             ]

myXmobarFont :: String
myXmobarFont = " --font='xft:M PLUS 1  Code'"

xmobarMain :: StatusBarConfig
xmobarMain = statusBarPropTo "_XMONAD_LOG_0" ("xmobar_wrapper --screen=0 --config=$HOME/.config/xmobar/xmobarrc_main" ++ myXmobarFont) (pure $ xmobarMainPP 0)
xmobarSub1 :: StatusBarConfig
xmobarSub1 = statusBarPropTo "_XMONAD_LOG_1" ("xmobar_wrapper --screen=1 --config=$HOME/.config/xmobar/xmobarrc_sub1" ++ myXmobarFont) (pure $ xmobarMainPP 1)
xmobarSub2 :: StatusBarConfig
xmobarSub2 = statusBarPropTo "_XMONAD_LOG_2" ("xmobar_wrapper --screen=2 --config=$HOME/.config/xmobar/xmobarrc_sub2" ++ myXmobarFont) (pure $ xmobarMainPP 2)
 
xmobarMainPP :: ScreenId -> PP
xmobarMainPP = \s -> xmobarPP
  { ppOrder  = \(ws:_:_:xs) -> [ws] ++ xs
  , ppSort   = getSortByXineramaRule
  , ppExtras = [ logLayoutOnScreen s
               , logTitlesOnScreen s formatFocused formatUnfocused
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
