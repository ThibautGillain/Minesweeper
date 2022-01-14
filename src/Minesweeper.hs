module Minesweeper where

import qualified Data.Set as Set
import System.Random
import Data.Maybe

-- This module is modelling the game of Minesweeper

type Cell = (Int, Int)

-- The Board type models the entire board of the Minesweeper game. The parameters represents the following :
-- width, height : Size of the grid
-- allCells : Set of all cells of the grid
-- untouchedCells : Set of the cells that are not discovered yet. Same as allCells at start
-- discoveredCells : Set of the cells that have been discovered. Empty at start
-- flaggedCells : Set of the cells that have been flagged by the player. Empty at start
-- bombs : Set of cells which contains bombs. Generated randomly when generating a board.
data Board = Board { 
    width :: Int,
    height :: Int,
    allCells :: Set.Set Cell,
    untouchedCells :: Set.Set Cell,
    discoveredCells :: Set.Set Cell,
    flaggedCells :: Set.Set Cell,
    bombs :: Set.Set Cell
} deriving (Show)



-- Endgame detection functions. Using Sets to model the current state of the board makes those functions quite straightforward.
isBoardLost :: Board -> Bool
isBoardLost board = not $ null $ (bombs board)  `Set.intersection` (discoveredCells board)

isBoardWon :: Board -> Bool
isBoardWon board = (not (isBoardLost board)) && (sizeOfUnion == getBoardSize board)
                    where sizeOfUnion = Set.size $ (bombs board) `Set.union` (discoveredCells board)

isGameEnded :: Board -> Bool
isGameEnded board = isBoardLost board || isBoardWon board



-- Functions to update the board (insert/delete cells in sets) and getters
discoverCell :: Board -> Cell -> Board
discoverCell board cell = if (not $ isDiscovered board cell) && (not $ isFlagged board cell)
                          then board { 
                                discoveredCells = Set.insert cell (discoveredCells board)
                                , untouchedCells = Set.delete cell (untouchedCells board) }
                          else board

flagCell :: Board -> Cell -> Board
flagCell board cell = if not $ isDiscovered board cell 
                      then board { 
                        flaggedCells = Set.insert cell (flaggedCells board)}
                      else board

unflagCell :: Board -> Cell -> Board
unflagCell board cell = if (isFlagged board cell) && (not $ isDiscovered board cell)
                        then board { 
                            flaggedCells = Set.delete cell (flaggedCells board)}
                        else board

mineCell :: Board -> Cell -> Board
mineCell board cell = board { bombs = Set.insert cell (bombs board)}

isDiscovered :: Board -> Cell -> Bool
isDiscovered board cell = Set.member cell (discoveredCells board)

isFlagged :: Board -> Cell -> Bool
isFlagged board cell = Set.member cell (flaggedCells board)

isUnflagged :: Board -> Cell -> Bool
isUnflagged board cell = not $ Set.member cell (flaggedCells board)

isUntouched :: Board -> Cell -> Bool
isUntouched board cell = Set.member cell (untouchedCells board)

isBomb :: Board -> Cell -> Bool
isBomb board cell = Set.member cell (bombs board)

getBoardSize :: Board -> Int
getBoardSize board = (width board) * (height board)



-- Functions to get the neighbours of a cell
getNeighbourCells :: Cell -> Int -> Int -> Set.Set Cell
getNeighbourCells cell width height = Set.fromList $ filter (cellInBounds width height) (getNeighbours cell)

getNeighbours :: Cell -> [Cell]
getNeighbours cell = [(rowIndex-1, columnIndex-1), (rowIndex-1, columnIndex), (rowIndex-1, columnIndex+1),
                         (rowIndex, columnIndex-1)                             , (rowIndex, columnIndex+1),
                         (rowIndex+1, columnIndex-1), (rowIndex+1, columnIndex), (rowIndex+1, columnIndex+1)]
                        where rowIndex = fst cell
                              columnIndex = snd cell

-- This function returns the number of bombs that are in the neighbourhood of a cell.
countNeighbouringBombs :: Board -> Cell -> Int
countNeighbouringBombs board cell = Set.size $ Set.intersection (bombs board) (getNeighbourCells cell (width board) (height board))

-- This is an utility function to keep only the cells that are really in the grid (for example a cell in the corner only has 3 neighbours and not 8)
-- The ranges are the following : [1,height] and [1, width]
cellInBounds :: Int -> Int -> Cell -> Bool
cellInBounds width height (row, column) = row > 0 && row <= height && column > 0 && column <= width



-- This functions generate all the cells of a board given his size.
generateCellsSet :: Int -> Int -> Set.Set Cell
generateCellsSet width height = Set.fromList $ generateCellsList rowIndexes columnIndexes
                                where rowIndexes = [1..height]
                                      columnIndexes = [1..width]

generateCellsList :: [Int] -> [Int] -> [Cell]
generateCellsList [] _ = []
generateCellsList rowIndexes columnIndexes = generateCellsRow (head rowIndexes) columnIndexes ++ generateCellsList (tail rowIndexes) columnIndexes

generateCellsRow :: Int -> [Int] -> [Cell]
generateCellsRow rowIndex columnIndexes = (zip (replicate (length columnIndexes) rowIndex) columnIndexes)


-- These functions are randomly generating mines and then fills these mines into the corresponding set in the board
-- We first generate a board with an empty bomb set and we generate and fill the bombs separately because we have to 
-- use the IO monad for the random generation.
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



-- These three functions are used to play an automatic obvious move if possible. This move is the simplest one, it is
-- just returning a cell which is an undiscovered neighbour of a cell which has no bombs in its neighbourhood.
-- We are using the Maybe monad to either return a Just Cell or Nothing if there are no safe move available in the current board.
-- This could have be done automatically by discovering all the cells which have 0 neighbours when clicking one of those cells but I didn't manage to achieve that.
safeMove :: Board -> Maybe Cell
safeMove board = getCellWithoutBombInNeighbours board

getCellWithoutBombInNeighbours :: Board -> Maybe Cell
getCellWithoutBombInNeighbours board = if (Set.size cellsWithoutBombsInNeighbours /= 0)
                                                 then Set.lookupGT (0,0) $ getUntouchedNeighbours board (Set.lookupGT (0,0) cellsWithoutBombsInNeighbours)
                                                 else Nothing
                                                 where cellsWithoutBombsInNeighbours = Set.filter (hasNoNeighbouringBombs board) (discoveredCells board)

hasNoNeighbouringBombs :: Board -> Cell -> Bool
hasNoNeighbouringBombs board cell = if (countNeighbouringBombs board cell == 0 && getNumberOfUntouchedNeighbours board cell /= 0)
                                    then True
                                    else False



-- These three functions are used to play an automatic obvious move if possible. This is a move that will flag a cell if a discovered cell in the board
-- has as much untouched neighbours as neighbouring mines. It will then return one of the neighbour cell as a cell to flag.
-- It is the simplest pattern described here http://www.minesweeper.info/wiki/Strategy
safeMoveToFlag :: Board -> Maybe Cell
safeMoveToFlag board = getObviousCellToFlag board

getObviousCellToFlag :: Board -> Maybe Cell
getObviousCellToFlag board = if (Set.size cellsWithSameBombsAndUntouchedInNeighbours /= 0)
                                then Set.lookupGT (0,0) $ getUntouchedAndUnflaggedNeighbours board (Set.lookupGT (0,0) cellsWithSameBombsAndUntouchedInNeighbours)
                                else Nothing
                                where cellsWithSameBombsAndUntouchedInNeighbours = Set.filter (hasSameNeighbouringBombsAndUntouched board) (discoveredCells board)

hasSameNeighbouringBombsAndUntouched :: Board -> Cell -> Bool
hasSameNeighbouringBombsAndUntouched board cell = if (numberOfNeighbouringBombs == numberOfUntouchedNeighbours && numberOfFlaggedNeighbours < numberOfNeighbouringBombs)
                                                    then True
                                                    else False
                                                  where numberOfNeighbouringBombs = countNeighbouringBombs board cell
                                                        numberOfUntouchedNeighbours = getNumberOfUntouchedNeighbours board cell
                                                        numberOfFlaggedNeighbours = getNumberOfFlaggedNeighbours board cell



-- These functions are utility functions used by the automatic move functions
getNumberOfUntouchedNeighbours :: Board -> Cell -> Int
getNumberOfUntouchedNeighbours board cell = Set.size $ Set.intersection neighbours (untouchedCells board)
                                            where neighbours = getNeighbourCells cell (width board) (height board)

getNumberOfFlaggedNeighbours :: Board -> Cell -> Int
getNumberOfFlaggedNeighbours board cell = Set.size $ Set.intersection neighbours (flaggedCells board)
                                          where neighbours = getNeighbourCells cell (width board) (height board)

getUntouchedNeighbours :: Board -> Maybe Cell -> Set.Set Cell
getUntouchedNeighbours board cell = Set.intersection (untouchedCells board) (getNeighbourCells (fromJust cell) (width board) (height board))

getUntouchedAndUnflaggedNeighbours :: Board -> Maybe Cell -> Set.Set Cell
getUntouchedAndUnflaggedNeighbours board cell = Set.filter (isUnflagged board) (Set.intersection (untouchedCells board) (getNeighbourCells (fromJust cell) (width board) (height board)))



-- These functions are returning a String to format the cell displaying depending on what they contain
bombCell :: String
bombCell = "B"

flaggedCell :: String
flaggedCell = "F"

untouchedCell :: String
untouchedCell = "."



-- These functions were used at the beginning as a tool to test that the modelisation of the game was well designed.
-- It is a simple displaying of the board in the console without any interaction.
-- It has been mainly inspired by this code on github: https://github.com/endymion64/HaskellMines/blob/master/MyBoard.hs                              
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
