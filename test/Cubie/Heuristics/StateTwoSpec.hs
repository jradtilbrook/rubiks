module Cubie.Heuristics.StateTwoSpec (spec) where

import Test.Hspec
import Cubie.Heuristics.StateTwo
import Data.Vector.Unboxed as V

spec :: Spec
spec = describe "ternaryFold" $ do
    it "[-1] equals 0" $
        V.ifoldl ternaryFold 0 (V.fromList [-1 :: Int]) `shouldBe` 0
    it "[0] equals 1" $
        V.ifoldl ternaryFold 0 (V.fromList [0 :: Int]) `shouldBe` 1
    it "[1] equals 2" $
        V.ifoldl ternaryFold 0 (V.fromList [1 :: Int]) `shouldBe` 2
    it "[0, 1] equals 7" $
        V.ifoldl ternaryFold 0 (V.fromList [0, 1 :: Int]) `shouldBe` 7
    it "[1, -1, 0, 1] equals 65" $
        V.ifoldl ternaryFold 0 (V.fromList [1, -1, 0, 1 :: Int]) `shouldBe` 65
    it "[1, 0, 1, 1, 0, -1] equals 158" $
        V.ifoldl ternaryFold 0 (V.fromList [1, 0, 1, 1, 0, -1 :: Int]) `shouldBe` 158
    it "[1, 1, 1, 1, 1, 1, 1] equals 2186" $
        V.ifoldl ternaryFold 0 (V.fromList [1, 1, 1, 1, 1, 1, 1 :: Int]) `shouldBe` 2186
