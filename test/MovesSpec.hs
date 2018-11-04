module MovesSpec (spec) where

import Moves
import Cube
import Test.Hspec

spec :: Spec
spec = describe "binaryFold" $ do
    it "2 front moves should be front2" $
        solvedCube -: front -: front `shouldBe` solvedCube -: front2
    it "3 front moves should be front'" $
        solvedCube -: front -: front -: front `shouldBe` solvedCube -: front'
    it "2 back moves should be back2" $
        solvedCube -: back -: back `shouldBe` solvedCube -: back2
    it "3 back moves should be back'" $
        solvedCube -: back -: back -: back `shouldBe` solvedCube -: back'
    it "2 left moves should be left2" $
        solvedCube -: left -: left `shouldBe` solvedCube -: left2
    it "3 left moves should be left'" $
        solvedCube -: left -: left -: left `shouldBe` solvedCube -: left'
    it "2 right moves should be right2" $
        solvedCube -: right -: right `shouldBe` solvedCube -: right2
    it "3 right moves should be right'" $
        solvedCube -: right -: right -: right `shouldBe` solvedCube -: right'
    it "2 up moves should be up2" $
        solvedCube -: up -: up `shouldBe` solvedCube -: up2
    it "3 up moves should be up'" $
        solvedCube -: up -: up -: up `shouldBe` solvedCube -: up'
    it "2 down moves should be down2" $
        solvedCube -: down -: down `shouldBe` solvedCube -: down2
    it "3 down moves should be down'" $
        solvedCube -: down -: down -: down `shouldBe` solvedCube -: down'
