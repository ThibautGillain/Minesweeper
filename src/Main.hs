module Main where

import Minesweeper

main :: IO ()
main = do putStr $ showBoard board

board :: Board
board = generateBoard 5 5 generateBombs