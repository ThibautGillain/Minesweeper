module Minesweeper where

import qualified Data.Set as Set

type Cell = (Int, Int)

data Board = Board { 
    width :: Int,
    height :: Int,
    untouchedCells :: Set.Set Cell,
    discoveredCells :: Set.Set Cell,
    flaggedCells :: Set.Set Cell,
    bombs :: Set.Set Cell
}

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


