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

captureTest2Board :: Board
captureTest2Board = Board ([0, 0, 0, 0, 0, 3] ++ [1, 2, 2, 3, 2, 2]) 0 0

captureTest2BoardAfter :: Board
captureTest2BoardAfter = Board ([0, 0, 0, 0, 0, 0] ++ [0, 0, 0, 3, 2, 2]) 8 0

spec :: Spec
spec = do
    describe "move" $ do
        it "should wrap around" $ do
            move 6 wrapTestBoard `shouldBe` wrapTestBoardAfter
        it "should capture" $ do
            move 6 captureTestBoard `shouldBe` captureTestBoardAfter
        it "should capture 2" $ do
            move 6 captureTest2Board `shouldBe` captureTest2BoardAfter
    
