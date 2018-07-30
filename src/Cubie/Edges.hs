module Cubie.Edges where

import Cubie.Misc

numEdges = 12

data Orientation = Solved | Unsolved deriving (Bounded, Enum, Eq, Ord, Read, Show)

newtype OrientationVector = OrientationVector { fromEdgeOrientation :: Vector Orientation }

newtype PermutationVector = PermutationVector { fromEdgePermutation :: Vector Orientation }

data Edge = Edge { perm :: PermutationVector, orien :: OrientationVector }
