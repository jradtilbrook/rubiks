module Cubie.Corners where

import Data.Maybe
import qualified Data.Vector.Unboxed as U

numCorners :: Int
numCorners = 8

newtype OrientationVector = OrientationVector (U.Vector Int)

newtype PermutationVector = PermutationVector (U.Vector Int)

data Corner = Corner { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: U.Vector Int -> Maybe PermutationVector
makePermutation v  = if U.length v /= numCorners then Nothing else Just $ PermutationVector v

makeOrientation :: U.Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
