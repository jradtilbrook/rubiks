module Cubie.Edges where

import Data.Maybe
import qualified Data.Vector.Unboxed as U

numEdges :: Int
numEdges = 12

newtype OrientationVector = OrientationVector (U.Vector Int)

newtype PermutationVector = PermutationVector (U.Vector Int)

data Edge = Edge { perm :: PermutationVector, orien :: OrientationVector }

makePermutation :: U.Vector Int -> Maybe PermutationVector
makePermutation v = if U.length v /= numEdges then Nothing else Just $ PermutationVector v

makeOrientation :: U.Vector Int -> Maybe PermutationVector
makeOrientation = makePermutation
