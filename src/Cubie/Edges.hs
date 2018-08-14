module Cubie.Edges where

import Prelude hiding (length)
import Cubie.Misc
import Data.Maybe

numEdges :: Int
numEdges = 12

type Orientation = Int

newtype OrientationVector = OrientationVector { fromEdgeOrientation :: Vector Orientation }

newtype PermutationVector = PermutationVector { fromEdgePermutation :: Vector Orientation }

data Edge = Edge { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: Vector Orientation -> Maybe PermutationVector
makePermutation v = if (length v) /= numEdges then Nothing else Just $ PermutationVector v

makeOrientation :: Vector Orientation -> Maybe PermutationVector
makeOrientation = makePermutation
