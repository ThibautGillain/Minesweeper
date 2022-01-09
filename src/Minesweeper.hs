module Minesweeper where

import qualified Data.Set as Set

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

discoverCell :: Board -> Cell -> Board
discoverCell board cell = board { discoveredCells = Set.insert cell (discoveredCells board) }

flagCell :: Board -> Cell -> Board
flagCell board cell = board { flaggedCells = Set.insert cell (flaggedCells board) }

unflagCell :: Board -> Cell -> Board
unflagCell board cell = board { flaggedCells = Set.delete cell (flaggedCells board) }

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
                                where rowIndexes = [1..width]
                                      columnIndexes = [1..height]

generateCellsList :: [Int] -> [Int] -> [Cell]
generateCellsList [] _ = []
generateCellsList rowIndexes columnIndexes = generateCellsRow (head rowIndexes) columnIndexes ++ generateCellsList (tail rowIndexes) columnIndexes

generateCellsRow :: Int -> [Int] -> [Cell]
generateCellsRow rowIndex columnIndexes = (zip (replicate (length columnIndexes) rowIndex) columnIndexes)

-- TODO: To be done randomly
generateBombs :: Set.Set Cell
generateBombs = Set.fromList [(1,1)]

generateBoard :: Int -> Int -> Set.Set Cell -> Board
generateBoard w h bombsSet = Board {
                                        width = w,
                                        height = h,
                                        allCells = generateCellsSet w h,
                                        untouchedCells = generateCellsSet w h,
                                        discoveredCells = Set.empty,
                                        flaggedCells = Set.empty,
                                        bombs = bombsSet
                                    }

                                    
showBoard :: Board -> String
showBoard board = show $ map (countNeighbouringBombs board) (Set.toList (allCells board))