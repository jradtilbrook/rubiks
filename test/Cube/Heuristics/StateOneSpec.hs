module Cube.Heuristics.StateOneSpec (spec) where

import Data.Vector.Unboxed (fromList, ifoldl)
import Heuristics.StateOne
import Test.Hspec

spec :: Spec
spec = describe "binaryFold" $ do
    it "[0] equals 0" $
        ifoldl binaryFold 0 (fromList [0 :: Int]) `shouldBe` 0
    it "[1] equals 1" $
        ifoldl binaryFold 0 (fromList [1 :: Int]) `shouldBe` 1
    it "[0, 1] equals 2" $
        ifoldl binaryFold 0 (fromList [0, 1 :: Int]) `shouldBe` 2
    it "[1, 1, 0, 1] equals 11" $
        ifoldl binaryFold 0 (fromList [1, 1, 0, 1 :: Int]) `shouldBe` 11
    it "[1, 0, 1, 1, 0, 1] equals 45" $
        ifoldl binaryFold 0 (fromList [1, 0, 1, 1, 0, 1 :: Int]) `shouldBe` 45
    it "[1, 1, 1, 1, 1, 1, 1] equals 127" $
        ifoldl binaryFold 0 (fromList [1, 1, 1, 1, 1, 1, 1 :: Int]) `shouldBe` 127
