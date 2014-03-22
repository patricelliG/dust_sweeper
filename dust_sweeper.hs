-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

 
main = do
    putStrLn "Welcome to dust sweeper!"
    putStrLn "Please enter the board size."
    boardSize <- getLine
    putStrLn ("Creating game board of size " ++ boardSize)
    let gameBoard = board (read boardSize :: Int) '#'
    printBoard gameBoard

-- Create a 2d list of size 'x' filled with 'a'
board :: Int -> char -> [[char]]
board x = replicate x . replicate x

-- Print the game board
printBoard :: [[char]] -> [String]
printBoard x = [show a | y <- x, a <- y] 
