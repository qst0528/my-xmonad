{-# LANGUAGE RebindableSyntax #-}

import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)

import XMonad.Config.Prime
    ( (++),
      (<+>),
      ($),
      Eq((==)),
      Fractional((/), fromRational),
      Num(fromInteger),
      Show(show),
      Applicative(pure),
      Monoid(mempty),
      Bool(..),
      IO,
      String,
      Maybe(..),
      mod4Mask,
      xC_left_ptr,
      (.),
      io,
      spawn,
      (=?),
      className,
      doFloat,
      doIgnore,
      title,
      appName,
      kill,
      refresh,
      sendMessage,
      windows,
      withFocused,
      (>>),
      apply,
      borderWidth,
      clickJustFocuses,
      focusFollowsMouse,
      focusedBorderColor,
      keys,
      manageHook,
      modMask,
      modifyLayout,
      normalBorderColor,
      resetLayout,
      sKeys,
      startupHook,
      terminal,
      withScreens,
      withWorkspaces,
      wsKeys,
      wsNames,
      xmonad,
      Default(def),
      ScreenId,
      ChangeLayout(NextLayout),
      Tall(Tall),
      RemovableClass((=-)),
      SettableClass((=:)),
      SummableClass((=+)), addLayout )
import XMonad.Util.Run            (safeSpawn, safeSpawnProg, unsafeSpawn, runInTerm)
import XMonad.Util.Loggers ( logLayoutOnScreen, logTitlesOnScreen, logWhenActive, logConst )
import XMonad.Util.Cursor         (setDefaultCursor)
import XMonad.Util.Hacks as Hacks ( javaHack )
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule, WorkspaceSort)
import XMonad.Util.NamedScratchpad
    ( customFloating,
      nonFloating,
      namedScratchpadAction,
      namedScratchpadManageHook,
      scratchpadWorkspaceTag,
      NamedScratchpad(NS) )
import XMonad.Prompt
    ( XPConfig(font, height, position), XPPosition(CenteredAt) )
import XMonad.Prompt.Unicode      (typeUnicodePrompt)
import XMonad.Prompt.Pass         (passPrompt, passEditPrompt, passGenerateAndCopyPrompt)
import XMonad.Prompt.RunOrRaise   (runOrRaisePrompt)
import XMonad.Prompt.Ssh          (sshPrompt)
import XMonad.Prompt.Man          (manPrompt)
import XMonad.Actions.GridSelect  (gridselectWorkspace, HasColorizer (defaultColorizer))
import XMonad.Actions.DynamicProjects (dynamicProjects,
                                       shiftToProjectPrompt,
                                       switchProjectPrompt,
                                       Project(..))
import XMonad.Actions.CycleWS (moveTo, hiddenWS, emptyWS, WSType(..), Direction1D(..))
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
                                   doLower,
                                   isFullscreen,
                                   transience,
                                   isDialog)
import XMonad.Layout.Fullscreen   (fullscreenSupportBorder)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing      (Spacing(..), spacingRaw, Border(..))
import XMonad.Layout.Accordion    (Accordion(..))
import XMonad.Layout.NoBorders (hasBorder)
import XMonad.Layout.Gaps (gaps, Gaps(..), Direction2D(..))
import XMonad.Util.NamedWindows (getName)
-- import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen, verticalScreenOrderer)

main :: IO ()
main = xmonad $ do
  -- Attributes to set
  normalBorderColor  =: "#dddddd"
  focusedBorderColor =: "#ff30e0"
  terminal           =: "st"
  modMask            =: mod4Mask
  borderWidth        =: 3
  focusFollowsMouse  =: False
  clickJustFocuses   =: False
  
  -- Attributes to modify
  manageHook  =+ namedScratchpadManageHook scratchpads
  manageHook  =+ composeOne [ isFullscreen -?> (doFullFloat <+> doRaise)
                            , isDialog     -?> doCenterFloat
                            , transience
                            , title =? "MaCoPiX" -?> doFloat
                            , className =? "Cairo-clock" -?> (hasBorder False <+> doIgnore <+> doLower)
                            ]
  startupHook =+ setDefaultCursor xC_left_ptr
  startupHook =+ killAllStatusBars
  startupHook =+ safeSpawn "tmux" ["start-server"]
  startupHook =+ safeSpawn "picom" ["-b", "--experimental-backends"]
  startupHook =+ safeSpawn "feh" ["--bg-max", "Pictures/Wallpapers/"]
  startupHook =+ safeSpawn "bash" [".xmonad/host-specific.sh"]

  resetLayout $ Tall 1 (3/100) (1/2)
  addLayout $ gaps [ (R, 630) ] $ Tall 1 (3/100) (1/2)
  apply $ fullscreenSupportBorder . ewmhFullscreen . ewmh . docks . Hacks.javaHack
  modifyLayout $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
  modifyLayout avoidStruts

  apply $ dynamicSBs barSpawner
  apply $ dynamicProjects projects

  withWorkspaces $ do
--    wsNames =: ["🀐", "🀑", "🀒", "🀓", "🀔", "🀕", "🀖", "🀗", "🀘"]
    wsKeys  =: ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
    wsNames =: ["dashboard", "scratch", "web-personal"]

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
    , ("M-i c"      , safeSpawnProg "chromium")
    ]

  keys =+
    [ ("M-l " ++ k  , S.promptSearch myXPConfig f) | (k, f) <- searchList ]

  keys =+
    [ ("M-c m"      , safeSpawnProg "google-meet")
    , ("M-c s"      , namedScratchpadAction scratchpads "slack")
    ]

  keys =+
    [ ("M-g"        , gridselectWorkspace def W.greedyView)
    ]

  keys =+
    [ ("M-w l"      , sendMessage NextLayout)
    , ("M-w s"      , sendMessage ToggleStruts)
    , ("M-w f"      , withFocused $ windows . W.sink)
    , ("M-f"        , moveTo Next (hiddenWS :&: Not emptyWS))
    , ("M-b"        , moveTo Prev (hiddenWS :&: Not emptyWS))
    , ("M-n"        , switchProjectPrompt myXPConfig)
    , ("M-S-n"      , shiftToProjectPrompt myXPConfig)
    , ("M-<Tab>"    , windows W.focusDown)
    , ("M-S-<Tab>"  , windows W.focusUp)
    , ("M-<Return>" , windows W.shiftMaster)
    ]

  keys =+ 
   [ ("M-p p"      , passPrompt myXPConfig)
   , ("M-p e"      , passEditPrompt myXPConfig)
   , ("M-p g"      , passGenerateAndCopyPrompt myXPConfig)
   , ("M-u"        , typeUnicodePrompt "/usr/share/unicode/UnicodeData.txt" myXPConfig)
   ]
    
  keys =+
    [ ("M-q"        , kill)
    , ("M-S-q"      , withFocused $ \w -> spawn ("xkill -id " ++ show w))
    , ("M-h m"      , manPrompt myXPConfig)
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
  , height   = 56
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
xmobarMain = statusBarPropTo "_XMONAD_LOG_0" "xmobar --screen=0 $HOME/.config/xmobar/xmobarrc_main" (pure $ xmobarMainPP 0)
xmobarSub1 :: StatusBarConfig
xmobarSub1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar --screen=1 $HOME/.config/xmobar/xmobarrc_sub1" (pure $ xmobarMainPP 1)
xmobarSub2 :: StatusBarConfig
xmobarSub2 = statusBarPropTo "_XMONAD_LOG_2" "xmobar --screen=2 $HOME/.config/xmobar/xmobarrc_sub2" (pure $ xmobarMainPP 2)
 
xmobarMainPP :: ScreenId -> PP
xmobarMainPP = \s -> filterOutWsPP [scratchpadWorkspaceTag] xmobarPP
  { ppOrder  = \(ws:_:_:xs) -> ws : xs
  , ppSort   = getSortByXineramaRule
  , ppExtras = [ logLayoutOnScreen s
               , logWhenActive s (logConst "*")
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

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "htop"  "st -c htop-sp -e htop" (className =? "htop-sp")
       (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))
  , NS "slack" "slack" (className =? "Slack") nonFloating
  ]

projects :: [Project]
projects =
  [ Project { projectName      = "scratch"
            , projectDirectory = "~"
            , projectStartHook = Nothing
            }
  , Project { projectName      = "dashboard"
            , projectDirectory = "~"
            , projectStartHook = Just $ do spawn ".xmonad/dashboard"
            }
  , Project { projectName      = "web-personal"
            , projectDirectory = "~/Downloads"
            , projectStartHook = Just $ do spawn "nyxt-personal"
            }
  , Project { projectName      = "web-work"
            , projectDirectory = "~/work"
            , projectStartHook = Just $ do spawn "nyxt-work"
            }
  , Project { projectName      = "web-nsfw"
            , projectDirectory = "~/Downloads"
            , projectStartHook = Just $ do spawn "nyxt-nsfw"
            }
  , Project { projectName      = "gimp"
            , projectDirectory = "~/Pictures"
            , projectStartHook = Just $ do spawn "gimp"
            }
  , Project { projectName      = "notion"
            , projectDirectory = "~/work"
            , projectStartHook = Just $ do spawn "notion-app-nativefier"
            }
  , Project { projectName      = "game"
            , projectDirectory = "~/Games"
            , projectStartHook = Nothing
            }
  ]
