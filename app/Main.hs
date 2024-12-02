module Main where

import System.IO

initN :: Int
initN = 4
boardWidth :: Int
boardWidth = 6
boardSize :: Int
boardSize = boardWidth * 2

newtype Board = Board [Int] deriving Show

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
prettyBoard b = show (reverse $ back b) ++ "\n" ++ show (front b)

empty :: Int -> Board -> Board
empty i (Board ps) = Board $ before ++ [0] ++ after
    where
        before = take (i - 1) ps
        after = drop i ps

move :: Int -> Board -> Board
move i (Board ps) = empty i $ Board $ zipWith (+) ps sow
    where
        n = ps !! i
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

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt str = case reads str :: [(Int, String)] of
    [(x ,"")] -> Just x
    _ -> Nothing

getInt :: IO (Maybe Int)
getInt = do 
    readInt <$> getLine

moveLoop :: Board -> IO()
moveLoop b = do
   hSetBuffering stdout NoBuffering
   putStrLn $ prettyBoard b
   putStr "> "
   mm <- getInt
   case validMove mm b of
    Just m -> moveLoop $ flipb $ move m b
    Nothing -> do
        putStrLn "Invalid move."
        moveLoop b

main :: IO ()
main = moveLoop initBoard

