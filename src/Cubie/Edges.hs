module Cubie.Edges where

import Prelude hiding (length)
import Cubie.Misc
import Data.Maybe

numEdges :: Int
numEdges = 12

newtype OrientationVector = OrientationVector { fromEdgeOrientation :: Vector Int }

newtype PermutationVector = PermutationVector { fromEdgePermutation :: Vector Int }

data Edge = Edge { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: Vector Int -> Maybe PermutationVector
makePermutation v = if length v /= numEdges then Nothing else Just $ PermutationVector v

makeOrientation :: Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
