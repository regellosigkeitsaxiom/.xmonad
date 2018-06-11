import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
--import XMonad.Hooks.Focus --Not released yet
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig(additionalKeysP)
import qualified XMonad.StackSet as W
import XMonad.Util.Run

--[ Системные модули ] --
import Data.List
import System.IO
import System.Exit

--[ Собственные модули ]--
import Launcher ( spawnConfigured )
import Xmobarrel

import Control.Exception
  ( catch
  , SomeException
  )

--проверять нажатые клавиши через xev
--названия окошек смотреть через xprop

tweeksConf = "/home/valentin/.xmonad/cfg/xmobars.conf"

launchXmobars = do
    catch 
        ( do
          x <- readTweeks tweeksConf
          sequence_ $ map spawn $ tweeks2String basePosition x
        )
        ( \e -> print ( e :: SomeException ))

main = do
    launchXmobars
    spawnConfigured "main.cfg"
    spawn "xmodmap .Xmodmap"
    --spawnConfigured "gau.cfg"
    --xmonad $ ignoreNetActiveWindow $ ewmh def 
    xmonad $ ewmh def 
         { modMask = mod4Mask
         , layoutHook = myLayoutHook
         , manageHook = myMH2 
         --, logHook = myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
         , logHook = dynamicLogString def >>= xmonadPropLog
         , workspaces = myWorkspaces
         , focusedBorderColor = bordColor
         , borderWidth = 5
         , normalBorderColor = "#666666"
         , terminal = "roxterm"
         , handleEventHook = docksEventHook
         , startupHook = docksStartupHook
         } `additionalKeysP` myKeys 

bordColor = "#e01d4b"
focusColor = "#8888ff"
myKeys =
    [ ("<XF86AudioPlay>", spawn "mocp -G")
    , ("M-<XF86AudioPlay>", spawn "mocp -S; mocp -o s; mocp -o r; mocp -o a; mocp -p")
    , ("<XF86AudioStop>", spawn "mocp -s")
    , ("<XF86AudioNext>", spawn "mocp -f")
    , ("M-<XF86AudioNext>", spawn "mocp -k +20")
    , ("<XF86AudioPrev>", spawn "mocp -r")
    , ("M-<XF86AudioPrev>", spawn "mocp -k -20")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 3" )
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 3" )
    --, ("<XF86AudioLowerVolume>", spawn "amixer -c 0 -- sset Master 3dB-")
    --, ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 -- sset Master 3dB+")
    --, ("<XF86Launch1>", spawn "sudo pm-hibernate") --Works on Lenovo
    --, ("M-<XF86Launch1>", spawn "sudo pm-suspend") --Works on Lenovo
    --, ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioMute>", spawn "pamixer -t" )
    , ("M-<XF86MonBrightnessDown>", spawn "xbacklight -1")
    , ("M-<XF86MonBrightnessUp>", spawn "xbacklight +1")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -4")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight +4")
    , ("M-x", spawn "pkill xmobar") --For debug
    , ("M-S-x", liftIO $ launchXmobars ) --For debug
    , ("M-m", spawn "~/pe/magnifu.sh" )
    , ("M-0", spawn "obfuscator" )
    , ("M-o", spawn "firefox -P default")
    , ("M-9", spawn "slack")
    , ("M-e", windows $ W.greedyView "E")
    , ("M-S-e", windows $ W.shift "E")
    , ("M-r", windows $ W.greedyView "R")
    , ("M-S-r", windows $ W.shift "R")
    , ("M-w", windows $ W.greedyView "W")
    , ("M-S-w", windows $ W.shift "W")
    , ("M-q", windows $ W.greedyView "Q")
    , ("M-S-q", windows $ W.shift "Q")
    , ("M-a", windows $ W.greedyView "A")
    , ("M-S-a", windows $ W.shift "A")
    , ("M-s", windows $ W.greedyView "S")
    , ("M-S-s", windows $ W.shift "S")
    , ("M-d", windows $ W.greedyView "D")
    , ("M-S-d", windows $ W.shift "D")
    , ("M-f", windows $ W.greedyView "F")
    , ("M-S-f", windows $ W.shift "F")
    , ("M-i", spawn "pkill xmobar; xmonad --recompile; xmonad --restart")
    , ("M-S-i", io (exitWith ExitSuccess))
    , ("<Print>", spawn "scrot")
    , ("M-<Print>", spawn "scrot -u")
    , ("M-v", sendMessage ToggleStruts)
    , ("M-\\", spawn "xinput set-prop 11 141 0; xdotool mousemove 2000 2000")
    , ("M-S-\\", spawn "xinput set-prop 11 141 1;") -- Hardware-dependent
    --, ("M-\\", spawn "xinput set-prop 12 139 0; xdotool mousemove 2000 2000")
    --, ("M-S-\\", spawn "xinput set-prop 12 139 1;") -- Hardware-dependent
    , ("M-]", spawn "roxterm -e ghci") --Calculator
    ]

myLayoutHook = avoidStruts
             . smartBorders
             . mediaSpaceHook
             . webSpaceHook
             $ myDefaultHook

myDefaultHook = Tall 1 ( 1/12 ) ( 2/3 )
            ||| Mirror ( Tall 1 ( 1/12 ) ( 2/3 ))
            ||| Full
mediaSpaceHook = onWorkspaces ["F"] Full
webSpaceHook = onWorkspaces ["E"] $ Mirror ( Tall 1 (1/20) (4/5)) ||| Full

myMH = myMH2 <+> manageHook def

keepMaster :: String -> ManageHook
keepMaster c = assertSlave <+> assertMaster
  where
  assertSlave = fmap (/= c) className --> doF W.swapDown
  assertMaster = className =? c --> doF W.swapMaster

myWorkspaces = ["Q","W","E","R","A","S","D","F"]

myMH2 = composeAll . concat $
  [ [ className =? c --> doShift "F" | c <- wFulls ]
  , [ keepMaster "Firefox" ]
  , [ className =? c --> doShift "R" | c <- wReads ]
  , [ className =? c --> doShift "E" | c <- wBrows ]
  , [ className =? c --> doF W.focusDown | c <- wFocus ]
  , [ className =? c --> doShift "W" | c <- wSlack ]
  ]
  where
  wFulls = [ "mplayer2", "MPlayer", "ivie", "Inkskape", "Steam", "adom", "My experiment" ]
  wFocus = [ "Slack", "slack", "Firefox", "Acroread" ]
  wSlack = [ "Slack", "slack" ]
  wReads = [ "MComix", "Evince", "Acroread" ]
  wBrows = [ "Firefox" ]
