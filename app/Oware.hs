module Oware where

initN :: Int
initN = 4
boardWidth :: Int
boardWidth = 6
boardSize :: Int
boardSize = boardWidth * 2

newtype Board = Board [Int] deriving (Show, Eq)

initSide :: [Int]
initSide = replicate boardSize initN

initBoard :: Board
initBoard = Board initSide

front :: Board -> [Int]
front (Board ps) = take boardWidth ps

back :: Board -> [Int]
back (Board ps) = drop boardWidth ps

flipb :: Board -> Board
flipb b = Board $ back b ++ front b

prettyBoard :: Board -> String
prettyBoard b = show (reverse $ back b) ++ "\n" ++ show (front b) ++ "\n"

empty :: Int -> Board -> Board
empty i (Board ps) = Board $ before ++ [0] ++ after
    where
        before = take (i - 1) ps
        after = drop i ps

moveWHand :: Int -> Board -> Board
moveWHand h (Board ps)
    | h < boardSize = Board $ zipWith (+) ps (replicate h 1 ++ repeat 0)
    | otherwise = moveWHand (h - boardSize) (Board (map (+ 1) ps))

move :: Int -> Board -> Board
move i (Board ps)
    | i + n < boardSize = empty i $ Board $ zipWith (+) ps sow
    | otherwise = moveWHand h $ empty i $ Board $ zipWith (+) ps (paddingBefore ++ repeat 1)
    where
        n = ps !! (i - 1)
        h = i + n - boardSize
        spread = replicate n 1
        paddingBefore = replicate i 0
        paddingAfter = repeat 0
        sow = paddingBefore ++ spread ++ paddingAfter

invalidMove :: Maybe Int -> Board -> Bool
invalidMove Nothing _ = True
invalidMove (Just n) (Board ps) = n < 1 || n > boardWidth || (ps !! (n - 1)) == 0

validMove :: Maybe Int -> Board -> Maybe Int
validMove m b 
    | invalidMove m b = Nothing
    | otherwise = m

