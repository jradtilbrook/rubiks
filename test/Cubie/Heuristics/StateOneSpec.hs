module Cubie.Heuristics.StateOneSpec (spec) where

import Test.Hspec
import Cubie.Heuristics.StateOne
import Data.Vector.Unboxed as V

spec :: Spec
spec = describe "binaryFold" $ do
    it "[0] equals 0" $
        V.ifoldl binaryFold 0 (V.fromList [0 :: Int]) `shouldBe` 0
    it "[1] equals 1" $
        V.ifoldl binaryFold 0 (V.fromList [1 :: Int]) `shouldBe` 1
    it "[0, 1] equals 2" $
        V.ifoldl binaryFold 0 (V.fromList [0, 1 :: Int]) `shouldBe` 2
    it "[1, 1, 0, 1] equals 11" $
        V.ifoldl binaryFold 0 (V.fromList [1, 1, 0, 1 :: Int]) `shouldBe` 11
    it "[1, 0, 1, 1, 0, 1] equals 45" $
        V.ifoldl binaryFold 0 (V.fromList [1, 0, 1, 1, 0, 1 :: Int]) `shouldBe` 45
    it "[1, 1, 1, 1, 1, 1, 1] equals 127" $
        V.ifoldl binaryFold 0 (V.fromList [1, 1, 1, 1, 1, 1, 1 :: Int]) `shouldBe` 127
