{-# LANGUAGE RebindableSyntax #-}

import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)

import XMonad.Config.Prime
import XMonad.Util.Run            (safeSpawn, safeSpawnProg, unsafeSpawn, runInTerm)
import XMonad.Util.Loggers
import XMonad.Util.Cursor         (setDefaultCursor)
import XMonad.Util.Hacks as Hacks
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule, WorkspaceSort)
import XMonad.Util.NamedScratchpad
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
import XMonad.Hooks.ManageDocks   (docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops  (ewmhFullscreen, ewmh, addEwmhWorkspaceSort)
import XMonad.Hooks.StatusBar     (statusBarPropTo,
                                   StatusBarConfig,
                                   killAllStatusBars,
                                   dynamicSBs)
import XMonad.Hooks.StatusBar.PP  (xmobarPP
                                  , PP(..)
                                  , wrap
                                  , shorten
                                  , xmobarColor
                                  , xmobarStrip
                                  , filterOutWsPP)
import XMonad.Hooks.ManageHelpers (composeOne,
                                   (-?>),
                                   doFullFloat,
                                   doCenterFloat,
                                   doRaise,
                                   isFullscreen,
                                   transience,
                                   isDialog)
import XMonad.Layout.Fullscreen   (fullscreenSupportBorder)
import XMonad.Layout.Spacing      (Spacing(..), smartSpacingWithEdge)
import XMonad.Layout.Accordion    (Accordion(..))
-- import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen, verticalScreenOrderer)

main :: IO ()
main = xmonad $ do
  -- Attributes to set
  normalBorderColor  =: "#dddddd"
  focusedBorderColor =: "#ff30e0"
  terminal           =: "st"
  modMask            =: mod4Mask
  borderWidth        =: 4
  focusFollowsMouse  =: False
  clickJustFocuses   =: False
  
  -- Attributes to modify
  manageHook  =+ namedScratchpadManageHook scratchpads
  manageHook  =+ composeOne [ isFullscreen -?> doFullFloat
                            , isDialog     -?> doCenterFloat
                            , transience
                            , title =? "MaCoPiX" -?> doFloat
                            ]
  startupHook =+ setDefaultCursor xC_left_ptr
  startupHook =+ killAllStatusBars
  startupHook =+ safeSpawn "picom" ["-b"]
  startupHook =+ safeSpawn "feh" ["--bg-max", "Pictures/Wallpapers/"]

  resetLayout $ Tall 1 (3/100) (1/2)
  modifyLayout $ smartSpacingWithEdge 10
  modifyLayout avoidStruts

  apply $ fullscreenSupportBorder . ewmhFullscreen . ewmh . docks . Hacks.javaHack

  apply $ dynamicSBs barSpawner

  withWorkspaces $ do
--    wsNames =: ["🀐", "🀑", "🀒", "🀓", "🀔", "🀕", "🀖", "🀗", "🀘"]
    wsKeys =: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

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
    , ("M-c s"      , namedScratchpadAction scratchpads "slack")
    ]

  keys =+
    [ ("M-l " ++ k  , S.promptSearch myXPConfig f) | (k, f) <- searchList ]

  keys =+
    [ ("M-w l"      , sendMessage NextLayout)
    , ("M-w s"      , sendMessage ToggleStruts)
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
    , ("M-m t"      , namedScratchpadAction scratchpads "htop")
    , ("M-m f"      , refresh)
    , ("M-m r"      , unsafeSpawn "xmonad --restart")
    , ("M-m q"      , io exitSuccess)
    ]

  withScreens $ do
    sKeys    =: ["r", "s", "t"]

myXPConfig :: XPConfig
myXPConfig = def
  { font     = "xft:Noto Sans Mono CJK JP:size=12"
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

xmobarMain :: StatusBarConfig
xmobarMain = statusBarPropTo "_XMONAD_LOG_0" ("xmobar --screen=0 $HOME/.config/xmobar/xmobarrc_main") (pure $ xmobarMainPP 0)
xmobarSub1 :: StatusBarConfig
xmobarSub1 = statusBarPropTo "_XMONAD_LOG_1" ("xmobar --screen=1 $HOME/.config/xmobar/xmobarrc_sub1") (pure $ xmobarMainPP 1)
xmobarSub2 :: StatusBarConfig
xmobarSub2 = statusBarPropTo "_XMONAD_LOG_2" ("xmobar --screen=2 $HOME/.config/xmobar/xmobarrc_sub2") (pure $ xmobarMainPP 2)
 
xmobarMainPP :: ScreenId -> PP
xmobarMainPP = \s -> filterOutWsPP [scratchpadWorkspaceTag] xmobarPP
  { ppOrder  = \(ws:_:_:xs) -> ws : xs
  , ppSort   = getSortByXineramaRule
  , ppExtras = [ logLayoutOnScreen s
               , logTitlesOnScreen s formatFocused formatUnfocused
               ]
  }
  where
    formatFocused   = wrap "[" "]" . xmobarColor "#ff79c6" "" . shorten 60 . xmobarStrip
    formatUnfocused = wrap "(" ")" . xmobarColor "#bd93f9" "" . shorten 35 . xmobarStrip

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure xmobarMain
barSpawner 1 = pure xmobarSub1
barSpawner 2 = pure xmobarSub2
barSpawner _ = mempty

scratchpads =
  [ NS "htop"  "st -c htop-sp -e htop" (className =? "htop-sp")
       (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
  , NS "slack" "slack" (className =? "Slack")
       (customFloating $ W.RationalRect (1/16) (1/16) (7/8) (7/8))
  ]
