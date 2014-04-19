-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

-- Notes
-- Must install cabal packages for random "cabal install cabal-install"

-- TODO
-- Add bombs at random and update surrounding spaces
-- Move on a space

import System.Random -- Requires cabal
import Data.List

-- Define data type for a space
data Space = Space { hasDust :: Bool
                   , isDiscovered :: Bool
                   , numAdjacent :: Int
                   } deriving (Show)
main = do
    -- Welcome the player
    putStrLn "Welcome to dust sweeper!"
    putStrLn "Please enter the board size."
    -- Initialize the game board
    boardSizeRaw <- getLine
    let boardSize = read boardSizeRaw
        initGameBoard = board boardSize (Space False True 0)
    -- Add the mines...oops, I mean "dust balls"
        numMines = calcNumMines boardSize
    randGen <- getStdGen
    let dustLocations = take (numMines * 2) (randomRs (0, (boardSize - 1)) randGen)
        gameBoard = placeAllDust initGameBoard dustLocations
    printBoard gameBoard
    -- Exit the game
    putStrLn ("Exiting...")

-- Create a 2D list of size 'x' filled with 'a'
board :: Int -> a -> [[a]]
board x = replicate x . replicate x

-- Display (Adjective) conversion values for spaces 
value :: Space -> Char 
value (Space dust discovered adjBombs) 
    | not discovered = 'X'
    | discovered && not dust = head $ show adjBombs
    | otherwise = 'D'

-- Return board with with visible characters 
convert :: [[Space]] -> [[Char]]
convert = map(map value)

-- Print game board 
printBoard :: [[Space]] -> IO()
printBoard = putStrLn . unlines . convert

-- Calculate the number of dust balls for board size 
-- Checked
calcNumMines :: Int -> Int
calcNumMines boardSize = round (fromIntegral(boardSize) / 2.0)

-- Place dust balls at locations given in "dustLocations"
-- Checked
placeAllDust :: [[Space]] -> [Int] -> [[Space]]
placeAllDust board dustLocations  
    | dustLocations == [] = board
    | otherwise = placeAllDust (placeDust board (dustLocations!!0) (dustLocations!!1)) (drop 2 dustLocations)

-- Place dust at "row" "column"
-- Checked
placeDust :: [[Space]] -> Int -> Int -> [[Space]]
placeDust board row column = incDustCountAll (changeSpace board row column (makeDust ((board!!row)!!column))) (calcAdj row column)

-- Return a board with elment at "row", "column" replaced with "newSpace" 
-- Checked
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
-- Sorry for the mess...
-- Checked
incDustCountAll :: [[Space]] -> [(Int, Int)]-> [[Space]]
incDustCountAll board dustySpaces 
    | dustySpaces == [] = board
    | otherwise = incDustCountAll (incDustCount board (fst (head dustySpaces)) (snd (head dustySpaces))) (tail dustySpaces)

-- Increment the numAdj count for the given space
-- Checked
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

-- Changes a spaces type to discovered, helper function for move 
makeDiscovered :: Space -> Space
makeDiscovered (Space dust discovered adjBombs) = (Space dust True adjBombs)
