module Cubie.Edges where

import Data.Maybe
import qualified Data.Vector.Unboxed as V

numEdges :: Int
numEdges = 12

newtype OrientationVector = OrientationVector (V.Vector Int)

newtype PermutationVector = PermutationVector (V.Vector Int)

data Edge = Edge { orien :: OrientationVector, perm :: PermutationVector }

makePermutation :: V.Vector Int -> Maybe PermutationVector
makePermutation v = if V.length v /= numEdges then Nothing else Just $ PermutationVector v

makeOrientation :: V.Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
