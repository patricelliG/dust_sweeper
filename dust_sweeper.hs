-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

-- Define data type for a space
data Space = Space { hasDust :: Bool
                   , isDiscovered :: Bool
                   , numAdjacent :: Int
                   } deriving (Show)

-- Create a 2D list of size 'x' filled with 'a'
board :: Int -> a -> [[a]]
board x = replicate x . replicate x

-- Display (Adjective) conversion values for spaces 
value :: Space -> Char 
value (Space dust discovered adjBombs) 
    | discovered /= True = 'X'
    | discovered == True && dust /= True = head $ show adjBombs
    | otherwise = 'F'

-- Return board with with visible characters 
convert :: [[Space]] -> [[Char]]
convert = map(map value)

-- Print game board 
printBoard :: [[Space]] -> IO()
printBoard = putStrLn . unlines . convert





-- Main method, mostly test code at the moment...
-- main = do
--    putStrLn "Welcome to dust sweeper!"
--    putStrLn "Please enter the board size."
--    boardSize <- getLine
--    putStrLn ("Creating game board of size " ++ boardSize)
--    let gameBoard = board (read boardSize :: Int) '#'
--    printBoard gameBoard
