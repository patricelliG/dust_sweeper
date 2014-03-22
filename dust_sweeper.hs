-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

-- Define data type for a space
data Space = Space { hasDust :: Bool
                   , isDiscovered :: Bool
                   , numAdjacent :: Int
                   } deriving (Show)
 
-- Main method, mostly test code at the moment...
main = do
    putStrLn "Welcome to dust sweeper!"
    putStrLn "Please enter the board size."
    boardSize <- getLine
    putStrLn ("Creating game board of size " ++ boardSize)
    let gameBoard = board (read boardSize :: Int) '#'
    printBoard gameBoard

-- Create a 2D list of size 'x' filled with 'a'
board :: Int -> a -> [[a]]
board x = replicate x . replicate x

-- Print the game board
printBoard :: [[a]] -> [String]
printBoard x = [show a | y <- x, a <- y] 
