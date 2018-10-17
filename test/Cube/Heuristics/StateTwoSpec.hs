module Cube.Heuristics.StateTwoSpec (spec) where

import Data.Vector.Unboxed (fromList, ifoldl)
import Heuristics.StateTwo
import Test.Hspec

spec :: Spec
spec = describe "ternaryFold" $ do
    it "[-1] equals 0" $
        ifoldl ternaryFold 0 (fromList [-1 :: Int]) `shouldBe` 0
    it "[0] equals 1" $
        ifoldl ternaryFold 0 (fromList [0 :: Int]) `shouldBe` 1
    it "[1] equals 2" $
        ifoldl ternaryFold 0 (fromList [1 :: Int]) `shouldBe` 2
    it "[0, 1] equals 7" $
        ifoldl ternaryFold 0 (fromList [0, 1 :: Int]) `shouldBe` 7
    it "[1, -1, 0, 1] equals 65" $
        ifoldl ternaryFold 0 (fromList [1, -1, 0, 1 :: Int]) `shouldBe` 65
    it "[1, 0, 1, 1, 0, -1] equals 158" $
        ifoldl ternaryFold 0 (fromList [1, 0, 1, 1, 0, -1 :: Int]) `shouldBe` 158
    it "[1, 1, 1, 1, 1, 1, 1] equals 2186" $
        ifoldl ternaryFold 0 (fromList [1, 1, 1, 1, 1, 1, 1 :: Int]) `shouldBe` 2186
