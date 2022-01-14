# Minesweeper
Haskell Minesweeper project. 

This project is a Minesweeper game in Haskell done for the "Topics in Functional Programming" course in Trinity College Dublin.

It is playable with a GUI implemented using the Threepenny GUI library

### Start the project
To start the projet, first run **stack build** and then execute the program by running **stack exec Minesweeper**


### Modules
#### Minesweeper.hs
This module models the game of Minesweeper. The main element is the **Board** data type which contains all the informations of a board.

This module implements all the functions needed to update that board.


#### UI.hs
This module implements the user interface of the application. It is done using the Threepenny library.

To change the size of the grid, you can change the **boardSize** function.

To change the difficulty and the number of mines in a grid, you can change the **density** function which represents the percentage of mines in the board.