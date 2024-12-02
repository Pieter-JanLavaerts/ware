module Main where

import System.IO

initN :: Int
initN = 4
boardWidth :: Int
boardWidth = 6
boardSize :: Int
boardSize = boardWidth * 2

data Board = Board [Int] deriving Show

initSide :: [Int]
initSide = take boardSize $ repeat initN

initBoard :: Board
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

invalidMove :: (Maybe Int) -> Board -> Bool
invalidMove Nothing _ = True
invalidMove (Just n) (Board ps) = n < 0 || n > boardWidth || (ps !! (n - 1)) == 0

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt str = case reads str :: [(Int, String)] of
    [(x ,"")] -> (Just x)
    _ -> Nothing

getInt :: IO (Maybe Int)
getInt = do
    nums <- getLine
    return (readInt nums)

moveLoop :: Board -> IO()
moveLoop b = do
   hSetBuffering stdout NoBuffering
   putStrLn $ show b
   putStr "> "
   maybeM <- getInt
   if invalidMove maybeM b
       then do
           putStrLn "Invalid!"
           moveLoop b
       else do
           let Just m = maybeM
           moveLoop $ flipb $ move m b

main :: IO ()
main = moveLoop initBoard

