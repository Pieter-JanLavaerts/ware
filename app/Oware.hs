module Oware where

import Text.Printf
import Data.List (intercalate)

initN :: Int
initN = 4
boardWidth :: Int
boardWidth = 6
boardSize :: Int
boardSize = boardWidth * 2

data Board = Board [Int] Int Int deriving (Show, Eq)

initSide :: [Int]
initSide = replicate boardSize initN

initBoard :: Board
initBoard = Board initSide 0 0

front :: Board -> [Int]
front (Board ps _ _) = take boardWidth ps

back :: Board -> [Int]
back (Board ps _ _) = drop boardWidth ps

frontHand :: Board -> Int
frontHand (Board _ fh _) = fh

backHand :: Board -> Int
backHand (Board _ _ bh) = bh

flipb :: Board -> Board
flipb b = Board (back b ++ front b) (backHand b) (frontHand b)
    
fmt :: Int -> [Char]
fmt = printf "%-02d"

prettySide :: [Int] -> [Char]
prettySide ps = intercalate "|" (map fmt $ reverse ps)

prettyBoard :: Board -> String
prettyBoard b = fmt (backHand b) ++ 
    prettySide (reverse (back b)) ++ 
    "..\n.." ++ prettySide (front b) ++ fmt (frontHand b) ++ "\n"

empty :: Int -> Board -> Board
empty i (Board ps fh bh) = Board (before ++ [0] ++ after) fh bh
    where
        before = take (i - 1) ps
        after = drop i ps

moveWrap :: Int -> Board -> Board
moveWrap h (Board ps fh bh)
    | h < boardSize = capture h $ Board (zipWith (+) ps (replicate h 1 ++ repeat 0)) fh bh
    | otherwise = moveWrap (h - boardSize) (Board (map (+ 1) ps) fh bh)

addToFrontHand ::  Int -> Board -> Board
addToFrontHand n (Board ps fh bh) = Board ps (fh + n) bh

capture :: Int -> Board -> Board
capture i b
    | i > boardWidth && (n == 2 || n == 3) = capture (i - 1) $ addToFrontHand n $ empty i b
    | otherwise = b
    where
        Board ps _ _ = b
        n = ps !! (i - 1)

move :: Int -> Board -> Board
move i (Board ps fh bh)
    | i + n < boardSize = capture (i + n) $ empty i $ Board (zipWith (+) ps sow) fh bh
    | otherwise = moveWrap h $ empty i $ Board (zipWith (+) ps (paddingBefore ++ repeat 1)) fh bh
    where
        n = ps !! (i - 1)
        h = i + n - boardSize
        spread = replicate n 1
        paddingBefore = replicate i 0
        paddingAfter = repeat 0
        sow = paddingBefore ++ spread ++ paddingAfter

invalidMove :: Maybe Int -> Board -> Bool
invalidMove Nothing _ = True
invalidMove (Just n) (Board ps _ _) = n < 1 || n > boardWidth || (ps !! (n - 1)) == 0

validMove :: Maybe Int -> Board -> Maybe Int
validMove m b 
    | invalidMove m b = Nothing
    | otherwise = m

