module Xmobarrel

where

import Text.Read ( readMaybe )
import Data.List ( find
                 , isPrefixOf
                 )

data Static = Static {xpos, ypos, width, height :: Int} deriving ( Show )
data Align = L | R | C deriving ( Read, Eq )

--Width and pattern
data Tweek = Tweek Int String
    deriving ( Read )

--Should get dynamically
screenWidth :: Int
screenWidth = 1920

basePosition = Static { xpos = 0
                      , ypos = 0
                      , width = 40
                      , height = 20
                      }

configLine = "~/.xmonad/cfg/xmobar.config"

output :: Static -> String -> String
output pos pat =
    "xmobar " ++
    configLine ++ " " ++
    "-p" ++ " '" ++
    show pos ++ "' " ++
    "-t" ++ " '" ++
    pat ++ "'"

tweeks2String :: Static -> [ Tweek ] -> [ String ]
tweeks2String st [Tweek a b] = [ output st{width=screenWidth-xpos st} b ]
tweeks2String st ( tw@(Tweek a b) : tws ) =
    output st{width=a} b : tweeks2String st{xpos=xpos st+a} tws

readTweeks :: FilePath -> IO [ Tweek ]
readTweeks f = do
    t <- readFile f --Need to catch exception !
    case readMaybe t of
        Just x -> return x
        Nothing -> do
            putStrLn "Could not parse xmobars.conf, insanely defaulting"
            return xmobars

xmobars :: [ Tweek ]
xmobars = [ Tweek 60 "%cpu%"
          , Tweek 60 "%memory%"
          , Tweek 40 "%swap%"
          , Tweek 140 "}%diskio%{"
          , Tweek 120 "}%wlp2s0%{"
          , Tweek 0 "}{%UIII%  <fc=#ff0000>%date%</fc>"
          ]
