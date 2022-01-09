module Main where

import Minesweeper

main :: IO ()
main = do print $ showBoard (generateBoard 2 2 generateBombs)
