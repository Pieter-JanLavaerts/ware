module Main where

import Data.Semigroup

initN = 4 :: Int
boardWidth = 6 :: Int
boardSize = boardWidth * 2 :: Int

data Board = Board [Int] deriving Show

initSide = take boardSize $ repeat initN
initBoard = Board initSide 

front :: Board -> [Int]
front (Board ps) = take boardWidth ps

back :: Board -> [Int]
back (Board ps) = drop boardWidth ps

flipb :: Board -> Board
flipb b = Board $ back b ++ front b

prettyBoard :: Board -> String
prettyBoard b = (show $ reverse $ back b) ++ "\n" ++ (show $ front b)

empty :: Int -> Board -> Board
empty i (Board ps) = Board $ before ++ [0] ++ after
    where
        before = take (i - 1) ps
        after = drop i ps

move :: Int -> Board -> Board
move i (Board ps) = empty i $ Board $ zipWith (+) ps sow
    where
        n = ps !! i
        spread = take n $ repeat 1
        paddingBefore = take i $ repeat 0
        paddingAfter = repeat 0
        sow = paddingBefore ++ spread ++ paddingAfter

moveLoop :: Board -> IO()
moveLoop b = do
	putStrLn $ prettyBoard b
	m <- getLine
	moveLoop $ flipb $ move (read m) b

main :: IO ()
main = moveLoop initBoard

