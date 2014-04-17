-- Name: dust_sweeper.hs 
-- Class: CSC_435
-- Date: 03/21/2014
-- Written By: Gary Patricelli                      

-- Notes
-- Must install cabal packages for random "cabal install cabal-install"

-- TODO
-- Add bombs at random
-- Move on a space
-- Calculate adjBombs

import System.Random -- Requires cabal
import Data.List

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
    | otherwise = 'D'

-- Return board with with visible characters 
convert :: [[Space]] -> [[Char]]
convert = map(map value)

-- Print game board 
printBoard :: [[Space]] -> IO()
printBoard = putStrLn . unlines . convert

-- Return a board with elment at "row", "column" replaced with "newSpace" 
changeSpace :: [[Space]] -> Int -> Int -> Space -> [[Space]]
changeSpace board row column newSpace
    | row > length board || column > length board = board 
    | otherwise = case splitAt column (board!!row) of
        (front, oldSpace:tail) -> restoreBoard board (front ++ newSpace : tail) row

-- Recombine broken down 2D list, helper function for changeSpace
restoreBoard :: [[Space]] -> [Space] -> Int -> [[Space]] 
restoreBoard board newRow splitRow
    | otherwise = case splitAt splitRow board of
        (top, oldRow:bottom) -> top ++ newRow : bottom




-- Main method, mostly test code at the moment...
-- main = do
--    putStrLn "Welcome to dust sweeper!"
--    putStrLn "Please enter the board size."
--    boardSize <- getLine
--    putStrLn ("Creating game board of size " ++ boardSize)
--    let gameBoard = board (read boardSize :: Int) '#'
--    printBoard gameBoard
