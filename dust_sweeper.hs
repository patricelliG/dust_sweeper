-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

-- Notes
-- Must install cabal packages for random "cabal install cabal-install"

-- TODO
-- Move on a space

import System.Random -- Requires cabal
import System.Exit 
import Data.List

-- Define data type for a space
data Space = Space { hasDust :: Bool
                   , isDiscovered :: Bool
                   , numAdjacent :: Int
                   } deriving (Show)

main = do
    -- Welcome the player
    putStrLn "Welcome to dust sweeper!"
    putStrLn "Please enter the desired board size."
    -- Initialize the game board
    boardSizeRaw <- getLine 
    let boardSize = read boardSizeRaw
        initGameBoard = board boardSize (Space False False 0)
    -- Add the mines...oops, I mean "dust balls"
        numDustBalls = calcNumMines boardSize
    randGen <- getStdGen
    let dustLocations = take (numDustBalls * 2) (randomRs (0, (boardSize - 1)) randGen)
        gameBoard = placeAllDust initGameBoard dustLocations
    -- Play the game!
    endBoard <- doTurns "Sweep Away!!!" gameBoard 
    -- Player lost, exit the game
    putStrLn "You Lose!"
    putStrLn "Exiting the Game."

-- Conducts a single turn for the player
-- Returns True if space was explored
-- Returns False if space was a bomb
doTurns :: String -> [[Space]] -> IO [[Space]]
doTurns prompt board = do
    printBoard board
    putStrLn prompt
    putStrLn "Enter the row to be swept..."
    expRowRaw <- getLine 
    let expRow = (read expRowRaw :: Int)
    putStrLn "Enter the column to be swept..."
    expColumnRaw <- getLine 
    let expColumn = (read expColumnRaw :: Int)
    if (checkBounds (length board) expRow expColumn )
        then do
            if (isDust board expRow expColumn)
                then do
                    putStrLn "You hit a dust ball!!!"
                    return board
                else do
                    putStrLn "Exploring space..."
                    -- Check if player won
                    if checkWin board 
                        then do
                            putStrLn "YOU WON THE GAME, CONGRATULATIONS!!!"
                            putStrLn "Exiting the Game."
                            exitSuccess
                        else do
                            let newBoard = changeSpace board expRow expColumn (makeDiscovered ((board!!expRow)!!expColumn))
                            doTurns "Good job! Keep on sweepin!" newBoard 
        else doTurns "Invalid bounds!" board

            
-- Check if play is in bounds of board
checkBounds :: Int -> Int -> Int -> Bool
checkBounds boardSize row column
    | row >= boardSize || row < 0 = False
    | column >= boardSize || column < 0 = False
    | otherwise = True

-- Return a 2D list of size 'x' filled with 'a'
board :: Int -> a -> [[a]]
board x = replicate x . replicate x

-- Display (Adjective) conversion values for spaces 
value :: Space -> Char 
value (Space dust discovered adjBombs) 
    | not discovered = '='
    | discovered && not dust && adjBombs == 0 = ' '
    | discovered && not dust = head $ show adjBombs
    | otherwise = '*'

-- Return board with with visible characters 
convert :: [[Space]] -> [[Char]]
convert = map(map value)

-- Print game board 
printBoard :: [[Space]] -> IO()
printBoard = putStrLn . unlines . convert

-- Helper function for player move
-- Will return true is space at "row" "column" is a dust ball
isDust :: [[Space]] -> Int -> Int -> Bool
isDust board row column = checkForDust $ ((board!!row)!!column)

-- Helper function for isDust
checkForDust :: Space -> Bool
checkForDust (Space dust discovered adjBombs) = dust

-- Calculate the number of dust balls for board size 
calcNumMines :: Int -> Int
calcNumMines boardSize = round (fromIntegral(boardSize) / 2.0)

-- Place dust balls at locations given in "dustLocations"
placeAllDust :: [[Space]] -> [Int] -> [[Space]]
placeAllDust board dustLocations  
    | dustLocations == [] = board
    | otherwise = placeAllDust (placeDust board (dustLocations!!0) (dustLocations!!1)) (drop 2 dustLocations)

-- Place dust at "row" "column"
placeDust :: [[Space]] -> Int -> Int -> [[Space]]
placeDust board row column = incDustCountAll (changeSpace board row column (makeDust ((board!!row)!!column))) (calcAdj row column)

-- Return a board with elment at "row", "column" replaced with "newSpace" 
changeSpace :: [[Space]] -> Int -> Int -> Space -> [[Space]]
changeSpace board row column newSpace
    | row >= length board || column >= length board = board 
    | otherwise = case splitAt column (board!!row) of
        (front, oldSpace:tail) -> restoreBoard board (front ++ newSpace : tail) row

-- Recombine broken down 2D list, helper function for changeSpace
restoreBoard :: [[Space]] -> [Space] -> Int -> [[Space]] 
restoreBoard board newRow splitRow
    | otherwise = case splitAt splitRow board of
        (top, oldRow:bottom) -> top ++ newRow : bottom

-- Given row "row" and column "column" return a list of adjacent spaces
-- Note, there is no error checking here, just dumb math
calcAdj :: Int -> Int -> [(Int, Int)]
calcAdj row column 
    | otherwise = [(row,column+1),(row+1,column+1),(row+1,column),(row+1,column-1),(row,column-1),(row-1,column-1),(row-1,column),(row-1,column+1)]

-- Increment the numAdj count for all spaces in the list of "dustySpaces"
-- Sorry for the long function chain...
incDustCountAll :: [[Space]] -> [(Int, Int)]-> [[Space]]
incDustCountAll board dustySpaces 
    | dustySpaces == [] = board
    | otherwise = incDustCountAll (incDustCount board (fst (head dustySpaces)) (snd (head dustySpaces))) (tail dustySpaces)

-- Increment the numAdj count for the given space
incDustCount :: [[Space]] -> Int -> Int -> [[Space]]
incDustCount board row column
    | row < 0 || row > length board || column < 0 || column > length board = board
    | otherwise = changeSpace board row column (updateCount ((board!!row)!!column))

-- Helper function for incDustCount
updateCount :: Space -> Space
updateCount (Space dust discovered adjBombs) = (Space dust discovered (adjBombs + 1))

-- Changes a spaces type to dust, helper function for placeDust
makeDust :: Space -> Space
makeDust (Space dust discovered adjBombs) = (Space True discovered adjBombs)

-- Changes a spaces type to discovered, helper function for player move
makeDiscovered :: Space -> Space
makeDiscovered (Space dust discovered adjBombs) = (Space dust True adjBombs)

-- Check if the player has won, ie all spaces have been discovered
-- with the exception of spaces containing "dust balls"
checkWin :: [[Space]] -> Bool
checkWin board
    | (length board == 0) = True
    | otherwise = checkRow (head board) && checkWin (tail board)

-- Helper function for check win
-- Checks a row of a board, if all spaces are discovered, returns true
checkRow :: [Space] -> Bool
checkRow row 
    | (length row == 0)  = True
    | otherwise = isSpaceDiscovered (head row) && checkRow (tail row)

-- Helper function for check win
-- Will return true for undiscovered dust balls and discovered spaces
isSpaceDiscovered :: Space -> Bool
isSpaceDiscovered (Space dust discovered adjBombs)
    | dust = True
    | otherwise = discovered
