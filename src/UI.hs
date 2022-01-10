module UI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery

import Minesweeper

startMinesweeperUI :: IO ()
startMinesweeperUI = do
    startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    getBody w #+ [string $ showBoard board]
              # set UI.style [("white-space", "pre-line")]
    return ()

board :: Board
board = generateBoard 5 5 generateBombs