module Main where

import Minesweeper

main :: IO ()
main = do print (show (generateCellsSet 5 5))
