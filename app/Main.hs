module Main where

import System.IO
import Oware

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
    Just m -> do
        putStrLn "Move."
        let moveB = move m b
        putStrLn $ prettyBoard moveB
        _ <- getInt
        moveLoop $ flipb moveB
    Nothing -> do
        putStrLn "Invalid move."
        moveLoop b

main :: IO ()
main = moveLoop initBoard

