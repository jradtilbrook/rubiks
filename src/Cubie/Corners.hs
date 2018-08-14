module Cubie.Corners where

import Prelude hiding (length)
import Cubie.Misc
import Data.Maybe

numCorners :: Int
numCorners = 8

{- Will derive bounded and enum to give this the properties we want, but can treat as Int everywhere -}
type Orientation = Int

newtype OrientationVector = OrientationVector { fromCornerOrientation :: Vector Orientation }

newtype PermutationVector = PermutationVector { fromCornerPermutation :: Vector Orientation }

data Corner = Corner { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: Vector Orientation -> Maybe PermutationVector
makePermutation v  = if (length v) /= numCorners then Nothing else Just $ PermutationVector v

makeOrientation :: Vector Orientation -> Maybe PermutationVector
makeOrientation = makePermutation
