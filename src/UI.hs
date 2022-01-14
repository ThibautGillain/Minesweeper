module UI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery

import Data.IORef
import Data.Bool
import Control.Monad.Trans (liftIO)

import Minesweeper

-- This module implements the User Interface of the Minesweeper game.
-- It uses the Threepenny GUI library.
-- The User Interface is pretty simple.

-- Size of the canvas containing the board
canvasSize :: Int
canvasSize = 25*boardSize

-- Size of the board. It's stored as a single constant so we only have square grids but it can be splitted into two
-- different constants to play with rectangular grids
boardSize :: Int
boardSize = 5

-- Density of mines in the board. It can be increased to add difficulty.
density :: Double
density = 0.15

-- This data type represents the main possible actions that a user can do on a Minesweeper Board
data Modes = Discover | Flag | UnFlag

startMinesweeperUI :: IO ()
startMinesweeperUI = do
    startGUI defaultConfig setup


-- This setup method sets up the HTML page displayed to the user. 
-- It also manages every possible interactions with the user inside the UI monad. It uses IORef to maintain some kind of state
setup :: Window -> UI ()
setup w = do
    return w # set UI.title "Minesweeper"

    -- Setting of the canvas 
    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    -- Generation of the initial board
    initialBoard <- liftIO $ generateBoardWithBombs boardSize boardSize (floor $ (fromIntegral (boardSize*boardSize)) * density)
    
    -- State of the application. Three IORefs represent the current board, the current mode (Discover, Flag, Unflag) and the current mouse position in the canvas.
    currentBoard <- liftIO $ newIORef initialBoard
    mode <- liftIO $ newIORef Discover
    pos <- liftIO $ newIORef (0,0)

    -- Different buttons of the UI
    discoverMode <- UI.button #+ [string "DISCOVER"]
    flagMode <- UI.button #+ [string "FLAG"]
    unflagMode <- UI.button #+ [string "UNFLAG"]
    newBoardButton <- UI.button #+ [string "NEW BOARD"]
    playMoveButton <- UI.button #+ [string "PLAY MOVE"]

    -- Those are two div containers and different messages that are used to display the end game message, the current mode and a message
    -- when we use the "PLAY MOVE" button but the application couldn't find an obvious move.
    currentModeContainer <- UI.div
    currentModeString <- string "Current mode : DISCOVER"
    element currentModeContainer # set children [currentModeString]  

    endGameMessageContainer <- UI.div
    winMessage <- string "You successfully uncovered all the safe cells. Click NEW BOARD to play another game"
    lostMessage <- string "You lost, you hit a mine. Click NEW BOARD to play another game "

    noSafeMoveMessageContainer <- UI.div
    noSafeMoveMessage <- string "No obvious safe move available"

    -- We draw the board in the canvas.
    drawBoard initialBoard False canvas

    -- On click functions:

    -- The first three functions binds to the mode buttons the update of the IORef representing the current mode of play.
    on UI.click discoverMode $ \_ -> 
        do liftIO $ writeIORef mode Discover
           currentModeString <- string "Current mode : DISCOVER"
           element currentModeContainer # set children [currentModeString] 
    
    on UI.click flagMode $ \_ ->
        do liftIO $ writeIORef mode Flag
           currentModeString <- string "Current mode : FLAG"
           element currentModeContainer # set children [currentModeString] 

    on UI.click unflagMode $ \_ ->
        do liftIO $ writeIORef mode UnFlag
           currentModeString <- string "Current mode : UNFLAG"
           element currentModeContainer # set children [currentModeString] 

    -- This function binds to the "NEW BOARD" button the generation of a new board.
    -- It generates a new board and then writes it in the IORef representing the current board. It also sets the mode back to Discover.
    -- It finally draw the new board in the canvas.
    on UI.click newBoardButton $ \_ ->
        do newBoard <- liftIO $ generateBoardWithBombs boardSize boardSize (floor $ (fromIntegral (boardSize*boardSize)) * density)
           element noSafeMoveMessageContainer # set children []
           liftIO $ writeIORef currentBoard newBoard
           liftIO $ writeIORef mode Discover
           currentModeString <- string "Current mode : DISCOVER"
           element currentModeContainer # set children [currentModeString] 
           element endGameMessageContainer # set children []
           drawBoard newBoard False canvas

    -- This function binds to the "PLAY MOVE" button the automatic play of a play if there's an obvious play available.
    -- It first checks for a safe play to discover a cell that is definitely not containing a bomb.
    -- If there's no such play available. It will check if there is a cell that definitely contains a bomb and it will flag it.
    -- If those two moves are not available, it will just display to the user that no obvious move are currently available.
    on UI.click playMoveButton $ \_ ->
        do current <- liftIO $ readIORef currentBoard
           case safeMove current of
               Just cellToDiscover -> do
                   let newBoard = discoverCell current cellToDiscover
                   element noSafeMoveMessageContainer # set children []
                   liftIO $ writeIORef currentBoard newBoard
                   drawBoard newBoard False canvas
                   return ()
               Nothing -> do
                   case safeMoveToFlag current of
                       Just cellToFlag -> do 
                            let newBoard = flagCell current cellToFlag
                            element noSafeMoveMessageContainer # set children []
                            liftIO $ writeIORef currentBoard newBoard
                            drawBoard newBoard False canvas
                            return ()
                       Nothing -> do 
                            element noSafeMoveMessageContainer # set children [noSafeMoveMessage]
                            return ()

    -- This method binds to the movement of the mouse in the canvas the update of the pos IORef which contains 
    -- the position of the mouse at any time.
    on UI.mousemove canvas $ \(x,y) ->
        do liftIO $ writeIORef pos (x,y)

    -- This method binds a click in the canvas to the corresponding play.
    -- It will first check if the game is not ended and that the board should be clickable.
    -- If the user can play, il will converts the mouse position to the actual cell that the user clicked
    -- and play the move accordingly to the current mode.
    -- In the end, it will update the current board IORef and display the new board.
    on UI.mousedown canvas $ \_ -> do
        (x,y) <- liftIO $ readIORef pos
        m <- liftIO $ readIORef mode
        current <- liftIO $ readIORef currentBoard
        if (isGameEnded current)
        then return ()
        else do
            case m of 
                Discover -> do  
                        let cellToDiscover = getCellIndexFromMousePos (x,y)
                            newBoard = discoverCell current cellToDiscover
                        
                        liftIO $ writeIORef currentBoard newBoard
                        if (isBoardLost newBoard)
                            then do 
                                element noSafeMoveMessageContainer # set children []
                                element endGameMessageContainer # set children [lostMessage]
                                drawBoard newBoard True canvas
                                return ()
                            else if (isBoardWon newBoard)
                                then do 
                                    element noSafeMoveMessageContainer # set children []
                                    element endGameMessageContainer # set children [winMessage]
                                    drawBoard newBoard True canvas
                                    return ()
                                    else do 
                                        element noSafeMoveMessageContainer # set children []
                                        drawBoard newBoard False canvas
                                        return ()

                Flag -> do 
                    let cellToFlag = getCellIndexFromMousePos (x,y)
                        newBoard = flagCell current cellToFlag
                    element noSafeMoveMessageContainer # set children []
                    liftIO $ writeIORef currentBoard newBoard
                    drawBoard newBoard False canvas

                UnFlag -> do 
                    let cellToUnFlag = getCellIndexFromMousePos (x,y)
                        newBoard = unflagCell current cellToUnFlag
                    element noSafeMoveMessageContainer # set children []
                    liftIO $ writeIORef currentBoard newBoard
                    drawBoard newBoard False canvas
    
    -- We add to the body element all the elements of our page.
    getBody w #+ [
        column [element canvas]
        , element discoverMode
        , element flagMode
        , element unflagMode
        , element newBoardButton
        , element playMoveButton
        , element noSafeMoveMessageContainer
        , element currentModeContainer
        , element endGameMessageContainer]
    return ()


-- These functions are drawing the board on the canvas.
-- It first draw the lines of the grid and then draw each of the cells of the board using recursion.
drawBoard :: Board -> Bool -> Element  -> UI ()
drawBoard board showBombs canvas = do
    canvas # UI.clearCanvas
    canvas # set' UI.lineWidth 1.0
    canvas # set' UI.strokeStyle "gray"
    canvas # set' UI.textFont "14px"
    canvas # set' UI.textAlign UI.Center
    drawGridLines canvas
    drawRow (height board) board showBombs canvas

drawRow :: Int -> Board -> Bool -> Element -> UI ()
drawRow 0 _ _ _ = return ()
drawRow currentRow board showBombs canvas = do drawCells currentRow (width board) board showBombs canvas
                                               drawRow (currentRow - 1) board showBombs canvas

drawCells :: Int -> Int -> Board -> Bool -> Element -> UI ()
drawCells _ 0 _ _ _ = return ()
drawCells currentRow currentColumn board showBombs canvas = do
    let x = (width board) - currentColumn + 1
        y = (height board) - currentRow + 1
        currentCell = (x, y)
        textPosition = (fromIntegral ( x*25 - 12 ), fromIntegral( y*25 -12 ))
        currentCellFormat = if showBombs && (isBomb board currentCell)
                                then bombCell
                                else if (isFlagged board currentCell)
                                    then flaggedCell
                                    else if (isUntouched board currentCell)
                                      then untouchedCell
                                      else show $ countNeighbouringBombs board currentCell
    canvas # set' UI.fillStyle (UI.htmlColor (getCellColour currentCellFormat))                                  
    canvas # UI.fillText currentCellFormat textPosition
    canvas # set' UI.fillStyle (UI.htmlColor "black")
    drawCells currentRow (currentColumn - 1) board showBombs canvas

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

-- This function returns a few colours depending on the format of the cell to draw in the canvas
getCellColour :: String -> String
getCellColour "B" = "red"
getCellColour "F" = "black"
getCellColour "0" = "grey"
getCellColour "1" = "blue"
getCellColour "2" = "green"
getCellColour "3" = "red"
getCellColour _ = "brown"

-- This function converts the mouse position to the corresponding cell indexes.
getCellIndexFromMousePos :: (Int, Int) -> (Int, Int)
getCellIndexFromMousePos (x, y) = 
    ((floor $ (fromIntegral x)/25.0)+1, (floor $ (fromIntegral y)/25.0)+1) 