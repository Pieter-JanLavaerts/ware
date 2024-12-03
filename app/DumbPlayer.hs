module DumbPlayer where

import Oware

dumbMove :: Board -> Int
dumbMove (Board ps _ _) = dumbMoveIter 6 ps

dumbMoveIter :: Int -> [Int] -> Int
dumbMoveIter i ps
    | (ps !! (i - 1)) /= 0 = i
    | otherwise = dumbMoveIter (i - 1) ps
