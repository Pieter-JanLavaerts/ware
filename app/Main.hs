module Main where

import System.IO
import Oware
import DumbPlayer

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt str = case reads str :: [(Int, String)] of
    [(x ,"")] -> Just x
    _ -> Nothing

getInt :: IO (Maybe Int)
getInt = do 
    readInt <$> getLine

playerMove :: Board -> IO()
playerMove b
    | frontWon b = do 
        putStr "You won"
        return ()
    | backWon b = do 
        putStr "You lose"
        return ()
    | gameDraw b = do
        putStr "Draw"
        return ()
    | otherwise = do
        putStrLn $ prettyBoard b
        putStr "> "
        mm <- getInt
        case validMove mm b of
         Just m -> do
             putStrLn "Move."
             let moveB = move m b
             putStrLn $ prettyBoard moveB
             _ <- getInt
             computerMove $ flipb moveB
         Nothing -> do
             putStrLn "Invalid move."
             playerMove b

computerMove :: Board -> IO()
computerMove b = playerMove $ flipb $ move (dumbMove b) b

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    playerMove initBoard

