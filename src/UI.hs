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
    displayBoard w board

board :: Board
board = generateBoard 5 5 generateBombs

displayBoard :: Window -> Board -> UI ()
displayBoard w board = do displayRow w (height board) board

displayRow :: Window -> Int -> Board -> UI ()
displayRow w 0 board = do getBody w #+ [string "\n"]
                          return ()
displayRow w currentRow board = do getBody w #+ [row $ displayCells w currentRow (width board) board]
                                   displayRow w (currentRow - 1) board

displayCells :: Window -> Int -> Int -> Board -> [UI Element]
displayCells w _ 0 _ = []
displayCells w currentRow currentColumn board =
    let x = (width board) - currentColumn + 1
        y = (height board) - currentRow + 1
        currentCell = (x, y)
        currentCellFormat = if (isBomb board currentCell)
                            then bombCell
                            else if (isFlagged board currentCell)
                                 then flaggedCell
                                 else if (isUntouched board currentCell)
                                      then untouchedCell
                                      else show $ countNeighbouringBombs board currentCell
    in [UI.button # set text currentCellFormat # set UI.style [("width", "50px")]] ++ displayCells w currentRow (currentColumn - 1) board