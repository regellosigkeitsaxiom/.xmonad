module Launcher where

import System.IO ( readFile )
import System.Directory ( getHomeDirectory )
import Control.Monad ( sequence_ )
import XMonad.Core ( spawn )

import XMonad.Core
import Control.Monad.IO.Class

trim :: String -> String
trim = reverse.chip.reverse.chip 
    where
    chip [] = []
    chip (x:xs) | x == ' ' = chip xs
                | otherwise = x:xs

skipComments :: [String] -> [String]
skipComments [] = []
skipComments (x:xs) | y == [] = skipComments xs
                    | otherwise = y : skipComments xs
    where y = cutComments x

cutComments :: String -> String
cutComments [] = []
cutComments (x:xs) | x == '#' = []
                   | otherwise = x : cutComments xs

readCfg :: FilePath -> IO [String]
readCfg f = do
    h <- getHomeDirectory
    x <- readFile $ h ++ "/.xmonad/cfg/" ++ f
    let c = skipComments . map trim . lines $ x
    return c

spawnConfigured :: FilePath -> IO ()
spawnConfigured f = do
    c <- readCfg f
    sequence_ $ map spawn c
