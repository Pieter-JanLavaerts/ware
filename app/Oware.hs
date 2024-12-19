module Oware where

import Text.Printf
import Data.List (intercalate)

initN :: Int
initN = 4
boardWidth :: Int
boardWidth = 6
boardSize :: Int
boardSize = boardWidth * 2
winAmount :: Int
winAmount = 25

data Board = Board [Int] Int Int deriving (Show, Eq)

(!) :: Board -> Int -> Int
(!) (Board ps _ _) i = ps !! (i - 1)

initPebbles :: [Int]
initPebbles = replicate boardSize initN

initBoard :: Board
initBoard = Board initPebbles 0 0

front :: Board -> [Int]
front (Board ps _ _) = take boardWidth ps

back :: Board -> [Int]
back (Board ps _ _) = drop boardWidth ps

pebbles :: Board -> [Int]
pebbles (Board ps _ _) = ps

frontHand :: Board -> Int
frontHand (Board _ fh _) = fh

backHand :: Board -> Int
backHand (Board _ _ bh) = bh

flipb :: Board -> Board
flipb b = Board (back b ++ front b) (backHand b) (frontHand b)
    
fmt :: Int -> [Char]
fmt = printf "%-02d"

prettySide :: [Int] -> [Char]
prettySide ps = "|" ++ intercalate "|" (map fmt ps) ++ "|"

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

addToBackHand ::  Int -> Board -> Board
addToBackHand n (Board ps fh bh) = Board ps fh (bh + n)

grandSlam :: Int -> Board -> Bool
grandSlam i b = allCapture && restEmpty where
        allCapture = all (\x -> x == 2 || x == 3) $ take (i - boardWidth) $ back b
        restEmpty = all (== 0) $ drop (i - boardWidth) $ back b

isCapture :: Int -> Board -> Bool
isCapture i b = (i > boardWidth) && (n == 2 || n == 3) && not (grandSlam i b)
    where
        n = b ! i

capture :: Int -> Board -> Board
capture i b
    | isCapture i b = capture (i - 1) $ addToFrontHand n $ empty i b
    | otherwise = b
    where
        n = b ! i

move :: Int -> Board -> Board
move i (Board ps fh bh)
    | i + n <= boardSize = capture (i + n) $ empty i $ Board (zipWith (+) ps sow) fh bh
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
invalidMove (Just n) b = outRange || emptyPit || all (== 0) backAfterMove
    where
        ps = pebbles b
        outRange = n < 1 || n > boardWidth
        emptyPit = (ps !! (n - 1)) == 0
        backAfterMove = back $ move n b

frontDraw :: Board -> Bool
frontDraw b = frontHand b == winAmount - 1

backDraw :: Board -> Bool
backDraw = frontDraw . flipb

gameDraw :: Board -> Bool
gameDraw b = frontDraw b && backDraw b

frontWon :: Board -> Bool
frontWon b = frontHand b >= winAmount

backWon :: Board -> Bool
backWon = frontWon . flipb

validMove :: Maybe Int -> Board -> Maybe Int
validMove m b 
    | invalidMove m b = Nothing
    | otherwise = m

