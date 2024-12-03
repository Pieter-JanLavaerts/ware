module OwareSpec(spec) where

import Test.Hspec
import Oware

setI :: Int -> Int -> Board -> Board
setI i j (Board ps) = Board $ before ++ [j] ++ after
    where
        before = take (i - 1) ps
        after = drop i ps

wrapTestBoard :: Board
wrapTestBoard = setI 6 7 $ Board (replicate boardSize 0)

wrapTestBoardAfter :: Board
wrapTestBoardAfter = Board $ [1] ++ replicate (boardWidth - 1) 0 ++ replicate boardWidth 1

spec :: Spec
spec = do
    describe "move" $ do
        it "should wrap around" $ do
            move 6 wrapTestBoard `shouldBe` wrapTestBoardAfter
    
