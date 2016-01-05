import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
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

import Control.Exception ( catch
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
    spawnConfigured "gau.cfg"
    xmonad $ ewmh defaultConfig 
         { modMask = mod4Mask
         , layoutHook = myLayoutHook
         , manageHook = myMH2 
         --, logHook = myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
         , logHook = dynamicLogString defaultPP >>= xmonadPropLog
         , workspaces = myWorkspaces
         , focusedBorderColor = bordColor
         , borderWidth = 5
         , normalBorderColor = "#666666"
         , terminal = "roxterm"
         } `additionalKeysP` myKeys 

bordColor = "#e01d4b"
focusColor = "#8888ff"
myKeys =
    [ ("<XF86AudioNext>", spawn "mocp -p")
    , ("<XF86AudioPlay>", spawn "mocp -G")
    , ("<XF86AudioStop>", spawn "mocp -s")
    , ("<XF86AudioPrev>", spawn "mocp -r")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Headphone 10-")
    , ("M-<XF86AudioLowerVolume>", spawn "amixer set Headphone 3dB-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Headphone 10+")
    , ("M-<XF86AudioRaiseVolume>", spawn "amixer set Headphone 3dB+")
    , ("<XF86Launch1>", spawn "sudo pm-hibernate")
    , ("M-<XF86Launch1>", spawn "sudo pm-suspend")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -4")
    , ("<XF86MonBrightnessUp>", spawn "xbacklight +4")

    , ("M-x", spawn "pkill xmobar")
    , ("M-S-x", liftIO $ launchXmobars )
    
    , ("M-e", windows $ W.greedyView "draw")
    , ("M-S-e", windows $ W.shift "draw")
    , ("M-r", windows $ W.greedyView "read")
    , ("M-S-r", windows $ W.shift "read")
    , ("M-w", windows $ W.greedyView "web")
    , ("M-S-w", windows $ W.shift "web")
    , ("M-q", windows $ W.greedyView "main")
    , ("M-S-q", windows $ W.shift "main")
    , ("M-a", windows $ W.greedyView "metr")
    , ("M-S-a", windows $ W.shift "metr")
    , ("M-s", windows $ W.greedyView "media")
    , ("M-S-s", windows $ W.shift "media")
    , ("M-d", windows $ W.greedyView "work")
    , ("M-S-d", windows $ W.shift "work")
    , ("M-f", windows $ W.greedyView "free")
    , ("M-S-f", windows $ W.shift "free")
    , ("M-i", spawn "pkill xmobar; xmonad --recompile; xmonad --restart")
    , ("M-S-i", io (exitWith ExitSuccess))
    , ("M-o", spawn "firefox")
    , ("M-Print", spawn "scrot -s")
    , ("M-v", sendMessage ToggleStruts)
    , ("M-\\", spawn "synclient TouchpadOff=1; xdotool mousemove 2000 2000")
  --, ("M-S-l", spawn "synclient TouchpadOff=0; xdotool mousemove 683 384")
    , ("M-S-\\", spawn "synclient TouchpadOff=0")
    ]

myLayoutHook = avoidStruts
             . smartBorders
             . workSpaceHook2
             . workSpaceHook
             $ customLayoutHook
workSpaceHook = onWorkspaces ["metr"] metrLayoutHook
workSpaceHook2 = onWorkspaces ["media"] mediaLayoutHook
mediaLayoutHook = Full ||| Tall 1 (5/100) (1/2)

metrLayoutHook = tiled ||| Mirror tiled ||| Full
    where tiled = Tall 2 (2/100) (1/2)

customLayoutHook = layoutHook defaultConfig

myMH = myMH2 <+> manageHook defaultConfig

myWorkspaces = ["main","web","draw","read","metr","media","free","work"]

myMH2 = composeAll
    [ className =? "mplayer2" --> doShift "media"
    , className =? "MPlayer" --> doShift "media"
    , className =? "Firefox" --> doShift "web"
    , className =? "Evince" --> doShift "read"
    , className =? "Inkscape" --> doShift "draw"
    , className =? "Mcomix" --> doShift "read"
    , className =? "Steam" --> doShift "free"
    , className =? "adom" --> doShift "media"
    , title =? "My experiment" --> doShift "media"
    , stringProperty "WM_WINDOW_ROLE" =? "metr" --> doShift "metr"
    ] --Добавить автоматическое убирание дока при посещении "media"
