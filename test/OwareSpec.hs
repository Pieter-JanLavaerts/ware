module OwareSpec(spec) where

import Test.Hspec
import Oware

setI :: Int -> Int -> Board -> Board
setI i j (Board ps fh bh) = Board (before ++ [j] ++ after) fh bh
    where
        before = take (i - 1) ps
        after = drop i ps

wrapTestBoard :: Board
wrapTestBoard = setI 6 7 $ Board (replicate boardSize 0) 0 0

wrapTestBoardAfter :: Board
wrapTestBoardAfter = Board ([1] ++ replicate (boardWidth - 1) 0 ++ replicate boardWidth 1) 0 0

captureTestBoard :: Board
captureTestBoard = Board ([0, 0, 0, 0, 0, 2] ++ [1, 1, 0, 0, 0, 0]) 0 0

captureTestBoardAfter :: Board
captureTestBoardAfter = Board ([0, 0, 0, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0]) 4 0

capture4TestBoard :: Board
capture4TestBoard = Board ([1, 10, 8, 6, 5, 1] ++ [5, 5, 5, 0, 1, 1]) 0 0

capture4TestBoardAfter :: Board
capture4TestBoardAfter = Board ([1, 0, 9, 7, 6, 2] ++ [6, 6, 6, 1, 0, 0]) 4 0

capture2TestBoard :: Board
capture2TestBoard = Board ([1, 0, 9, 7, 6, 2] ++ [6, 6, 6, 1, 2, 2]) 0 0

capture2TestBoardAfter :: Board
capture2TestBoardAfter = Board ([1, 0, 9, 7, 6, 2] ++ [6, 6, 6, 1, 0, 0]) 4 0

spec :: Spec
spec = do
    describe "capture" $ do
        it "should capture" $ do
            capture 12 capture2TestBoard `shouldBe` capture2TestBoardAfter
    describe "move" $ do
        it "should wrap around" $ do
            move 6 wrapTestBoard `shouldBe` wrapTestBoardAfter
        it "should capture" $ do
            move 6 captureTestBoard `shouldBe` captureTestBoardAfter
        it "should capture" $ do
            move 2 capture4TestBoard `shouldBe` capture4TestBoardAfter
    
