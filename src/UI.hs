module UI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery

import Data.IORef
import Control.Monad.Trans (liftIO)

import Minesweeper

canvasSize :: Int
canvasSize = 25*boardSize

boardSize :: Int
boardSize = 5

data Modes = Discover | Flag

startMinesweeperUI :: IO ()
startMinesweeperUI = do
    startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set UI.title "Minesweeper"

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    mode <- liftIO $ newIORef Discover
    pos <- liftIO $ newIORef (0,0)

    discoverMode <- UI.button #+ [string "discover"]
    flagMode <- UI.button #+ [string "flag"]

    drawBoard board canvas

    getBody w #+ [column [element canvas], element discoverMode, element flagMode]
    return ()

board :: Board
board = generateBoard boardSize boardSize generateBombs

drawBoard :: Board -> Element -> UI ()
drawBoard board canvas = do
    canvas # set' UI.lineWidth 1.0
    canvas # set' UI.strokeStyle "gray"
    canvas # set' UI.textFont "14px"
    canvas # set' UI.textAlign UI.Center
    drawGridLines canvas
    drawRow (height board) board canvas

drawRow :: Int -> Board -> Element -> UI ()
drawRow 0 _ _ = return ()
drawRow currentRow board canvas = do drawCells currentRow (width board) board canvas
                                     drawRow (currentRow - 1) board canvas


drawCells :: Int -> Int -> Board -> Element -> UI ()
drawCells _ 0 _ _ = return ()
drawCells currentRow currentColumn board canvas = do
    let x = (width board) - currentColumn + 1
        y = (height board) - currentRow + 1
        currentCell = (x, y)
        textPosition = (fromIntegral ( x*25 - 12 ), fromIntegral( y*25 -12 ))
        currentCellFormat = if (isBomb board currentCell)
                            then bombCell
                            else if (isFlagged board currentCell)
                                 then flaggedCell
                                 else if (isUntouched board currentCell)
                                      then untouchedCell
                                      else show $ countNeighbouringBombs board currentCell
    canvas # UI.fillText currentCellFormat textPosition
    drawCells currentRow (currentColumn - 1) board canvas

drawGridLines :: Element -> UI ()
drawGridLines canvas = do
    let max       = fromIntegral canvasSize
        ranges    = [0, 25..max]
        top       = zip (repeat 0) ranges
        bottom    = zip (repeat max) ranges
        left      = zip ranges (repeat 0)
        right     = zip ranges (repeat max)
    mapM_ (drawLine canvas) (zip top bottom)
    mapM_ (drawLine canvas) (zip left right)

drawLine :: Element -> (UI.Point, UI.Point) -> UI ()
drawLine canvas (a, b) = do
    UI.beginPath canvas
    UI.moveTo a canvas
    UI.lineTo b canvas
    UI.closePath canvas
    UI.stroke canvas








-- setup :: Window -> UI ()
-- setup w = do
--     displayBoard w board

-- displayBoard :: Window -> Board -> UI ()
-- displayBoard w board = do displayRow w (height board) board

-- displayRow :: Window -> Int -> Board -> UI ()
-- displayRow w 0 board = do getBody w #+ [string "\n"]
--                           return ()
-- displayRow w currentRow board = do getBody w #+ [row $ displayCells w currentRow (width board) board]
--                                    displayRow w (currentRow - 1) board

-- displayCells :: Window -> Int -> Int -> Board -> [UI Element]
-- displayCells w _ 0 _ = []
-- displayCells w currentRow currentColumn board =
--     let x = (width board) - currentColumn + 1
--         y = (height board) - currentRow + 1
--         currentCell = (x, y)
--         currentCellFormat = if (isBomb board currentCell)
--                             then bombCell
--                             else if (isFlagged board currentCell)
--                                  then flaggedCell
--                                  else if (isUntouched board currentCell)
--                                       then untouchedCell
--                                       else show $ countNeighbouringBombs board currentCell
--     in [UI.button # set text currentCellFormat # set UI.style [("width", "50px")]] ++ displayCells w currentRow (currentColumn - 1) board