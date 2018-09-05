module Cubie.Corners where

import Data.Maybe
import qualified Data.Vector.Unboxed as V

numCorners :: Int
numCorners = 8

newtype OrientationVector = OrientationVector (V.Vector Int)

newtype PermutationVector = PermutationVector (V.Vector Int)

data Corner = Corner { orien :: OrientationVector, perm :: PermutationVector }

makePermutation :: V.Vector Int -> Maybe PermutationVector
makePermutation v  = if V.length v /= numCorners then Nothing else Just $ PermutationVector v

makeOrientation :: V.Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
