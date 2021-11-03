module XMonadConfig.Bind
  (addMyKeys) where

import System.Exit (exitSuccess)
import XMonad

import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Pass       (passPrompt, passEditPrompt)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Ssh        (sshPrompt)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)
import XMonad.Util.NamedActions (addName, NamedAction, (^++^), HasName, xMessage, addDescrKeys')
import XMonad.Util.EZConfig (mkKeymap, mkNamedKeymap)
import XMonad.Util.Run      (safeSpawn, safeSpawnProg, runInTerm)

import XMonadConfig.Common (myXPConfig, myWorkspaces, myModMask)

addMyKeys :: XConfig l -> XConfig l
addMyKeys = addDescrKeys' ((myModMask, xK_h), xMessage) myKeys

myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys = \c -> mkNamedKeymap c $
  [ -- Terminal and Emacs
    ("M-v"        , addName "Open Terminal" $ runInTerm "" "tmux new-session")
  , ("M-S-v"      , addName "Open SSH prompt" $ sshPrompt myXPConfig)
  , ("M-e"        , addName "Open Emacs" $ safeSpawn "emacsclient" ["--create-frame"])
  , ("M-i p"      , addName "Launch Nyxt Personal" $ safeSpawnProg "nyxt-personal")
  , ("M-i w"      , addName "Launch Nyxt Work" $ safeSpawnProg "nyxt-work")
  , ("M-i n"      , addName "Launch Nyxt NSFW" $ safeSpawnProg "nyxt-nsfw")
  , ("M-i y"      , addName "Launch Youtube" $ safeSpawnProg "youtube")
  , ("M-i m"      , addName "Launch Google Meet" $ safeSpawnProg "google-meet")
  , ("M-p p"      , addName "Clip pass Password" $ passPrompt myXPConfig)
  , ("M-p e"      , addName "Edit Pass Password" $ passEditPrompt myXPConfig)
  , ("M-<Tab>"    , addName "Focus Next Window" $ windows W.focusDown)
  , ("M-S-<Tab>"  , addName "Focus Previous Window" $ windows W.focusUp)
  , ("M-<Return>" , addName "Swap Focused window and Master" $ windows W.shiftMaster)
  , ("M-q"        , addName "Close Focused Window" kill)
  , ("M-l"        , addName "Run or Raise" $ runOrRaisePrompt myXPConfig)
  , ("M-m r"      , addName "Restart XMonad" $ spawn "xmonad --restart")
  , ("M-m q"      , addName "Quit XMonad" $ io exitSuccess)
  ]
  ^++^
  [
   (("M-" ++ otherModMasks ++ key), addName "Screen" $ f sc)
      | (key, sc)          <- zip ["a", "r", "s"] [0..]
      , (otherModMasks, f) <- [("", viewScreen def), ("S-", sendToScreen def)]
  ]
  ^++^
  [
    ("M-" ++ otherModMasks ++ key, addName ("Workspace" ++ tag)$ f tag)
      | (tag, key)         <- zip myWorkspaces (map (\x -> show x) [1..9])
      , (otherModMasks, f) <- [("", windows . W.greedyView), ("S-", windows . W.shift)]
  ]
  
