module Minesweeper where

import qualified Data.Set as Set
import System.Random

type Cell = (Int, Int)

data Board = Board { 
    width :: Int,
    height :: Int,
    allCells :: Set.Set Cell,
    untouchedCells :: Set.Set Cell,
    discoveredCells :: Set.Set Cell,
    flaggedCells :: Set.Set Cell,
    bombs :: Set.Set Cell
} deriving (Show)

isBoardLost :: Board -> Bool
isBoardLost board = not $ null $ (bombs board)  `Set.intersection` (discoveredCells board)

isBoardWon :: Board -> Bool
isBoardWon board = (not (isBoardLost board)) && (sizeOfUnion == getBoardSize board)
                    where sizeOfUnion = Set.size $ (bombs board) `Set.union` (discoveredCells board)

isGameEnded :: Board -> Bool
isGameEnded board = isBoardLost board || isBoardWon board

discoverCell :: Board -> Cell -> Board
discoverCell board cell = if (not $ isDiscovered board cell) && (not $ isFlagged board cell)
                          then board { 
                                discoveredCells = Set.insert cell (discoveredCells board)
                                , untouchedCells = Set.delete cell (untouchedCells board) }
                          else board

flagCell :: Board -> Cell -> Board
flagCell board cell = if not $ isDiscovered board cell 
                      then board { 
                        flaggedCells = Set.insert cell (flaggedCells board) 
                        , untouchedCells = Set.delete cell (untouchedCells board)}
                      else board

unflagCell :: Board -> Cell -> Board
unflagCell board cell = if (isFlagged board cell) && (not $ isDiscovered board cell) 
                        then board { 
                            flaggedCells = Set.delete cell (flaggedCells board)
                            , untouchedCells = Set.insert cell (untouchedCells board) }
                        else board

mineCell :: Board -> Cell -> Board
mineCell board cell = board { bombs = Set.insert cell (bombs board)}

isDiscovered :: Board -> Cell -> Bool
isDiscovered board cell = Set.member cell (discoveredCells board)

isFlagged :: Board -> Cell -> Bool
isFlagged board cell = Set.member cell (flaggedCells board)

isUntouched :: Board -> Cell -> Bool
isUntouched board cell = Set.member cell (untouchedCells board)

isBomb :: Board -> Cell -> Bool
isBomb board cell = Set.member cell (bombs board)

getBoardSize :: Board -> Int
getBoardSize board = (width board) * (height board)

getNeighbourCells :: Cell -> Set.Set Cell
getNeighbourCells cell = Set.fromList $ getTopNeighbours cell ++ getSideNeighbours cell ++ getBottomNeighbours cell

getTopNeighbours :: Cell -> [Cell]
getTopNeighbours cell = [(rowIndex-1, columnIndex-1), (rowIndex-1, columnIndex), (rowIndex-1, columnIndex+1)]
                        where rowIndex = fst cell
                              columnIndex = snd cell

getBottomNeighbours :: Cell -> [Cell]
getBottomNeighbours cell = [(rowIndex+1, columnIndex-1), (rowIndex+1, columnIndex), (rowIndex+1, columnIndex+1)]
                            where rowIndex = fst cell
                                  columnIndex = snd cell

getSideNeighbours :: Cell -> [Cell]
getSideNeighbours cell = [(rowIndex, columnIndex-1), (rowIndex, columnIndex+1)]
                        where rowIndex = fst cell
                              columnIndex = snd cell

countNeighbouringBombs :: Board -> Cell -> Int
countNeighbouringBombs board cell = Set.size $ Set.intersection (bombs board) (getNeighbourCells cell)

generateCellsSet :: Int -> Int -> Set.Set Cell
generateCellsSet width height = Set.fromList (generateCellsList rowIndexes columnIndexes)
                                where rowIndexes = [1..height]
                                      columnIndexes = [1..width]

generateCellsList :: [Int] -> [Int] -> [Cell]
generateCellsList [] _ = []
generateCellsList rowIndexes columnIndexes = generateCellsRow (head rowIndexes) columnIndexes ++ generateCellsList (tail rowIndexes) columnIndexes

generateCellsRow :: Int -> [Int] -> [Cell]
generateCellsRow rowIndex columnIndexes = (zip (replicate (length columnIndexes) rowIndex) columnIndexes)

-- TODO: To be done randomly
-- generateBombs :: Int -> Int -> Int -> Set.Set Cell
-- generateBombs height width n = do return Set.empty

generateBombsList :: Int -> Int -> Int -> IO [Cell]
generateBombsList _ _ 0 = return []
generateBombsList height width n = do
    x <- randomRIO (1, height)
    y <- randomRIO (1, width)
    otherMines <- generateBombsList height width (n-1)
    return $ (x,y):otherMines

generateMinesInBoard :: Board -> [Cell] -> Board
generateMinesInBoard board [] = board
generateMinesInBoard board (cell:tail) = generateMinesInBoard (mineCell board cell) tail


generateBoard :: Int -> Int -> Board
generateBoard w h = Board {
                            width = w,
                            height = h,
                            allCells = generateCellsSet w h,
                            untouchedCells = generateCellsSet w h,
                            discoveredCells = Set.empty,
                            flaggedCells = Set.empty,
                            bombs = Set.empty
                          }

generateBoardWithBombs :: Int -> Int -> Int -> IO Board
generateBoardWithBombs w h n = do
    let board = generateBoard w h
    bombsList <- generateBombsList w h n
    return $ generateMinesInBoard board bombsList


-- https://github.com/endymion64/HaskellMines/blob/master/MyBoard.hs
                                    
showBoard :: Board -> String
showBoard board = 
    printBorder (width board) ++
    printRow (height board) board

printBorder :: Int -> String
printBorder 0 = "+\n"
printBorder w = "+---" ++ printBorder (w-1)

printRow :: Int -> Board -> String
printRow 0 board = "\n"
printRow currentRow board = 
    printCells currentRow (width board) board ++
    printBorder (width board) ++
    printRow (currentRow-1) board

printCells :: Int -> Int -> Board -> String
printCells _ 0 _ = "|\n"
printCells currentRow currentColumn board = 
    let x = (width board) - currentColumn + 1
        y = (height board) - currentRow + 1
        currentCell = (x, y)
        currentCellFormat = if (isBomb board currentCell)
                            then bombCellConsole
                            else if (isFlagged board currentCell)
                                 then flaggedCellConsole
                                 else if (isUntouched board currentCell)
                                      then untouchedCellConsole
                                      else show $ countNeighbouringBombs board currentCell
    in currentCellFormat ++ printCells currentRow (currentColumn -1) board

bombCellConsole :: String
bombCellConsole = "| B "

flaggedCellConsole :: String
flaggedCellConsole = "| F "

untouchedCellConsole :: String
untouchedCellConsole = "| . "

bombCell :: String
bombCell = "B"

flaggedCell :: String
flaggedCell = "F"

untouchedCell :: String
untouchedCell = "."