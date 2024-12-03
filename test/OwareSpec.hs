module OwareSpec(spec) where

import Test.Hspec
import Oware

spec :: Spec
spec = do
    describe "Oware" $ do
      it "invalidMove" $ do
        invalidMove Nothing initBoard `shouldBe` True
    
