module Cubie.Corners where

import Cubie.Misc
import Data.Maybe

numCorners :: Int
numCorners = 8

newtype OrientationVector = OrientationVector { fromCornerOrientation :: Vector Int }

newtype PermutationVector = PermutationVector { fromCornerPermutation :: Vector Int }

data Corner = Corner { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: Vector Int -> Maybe PermutationVector
makePermutation v  = if (length v) /= numCorners then Nothing else Just $ PermutationVector v

makeOrientation :: Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
