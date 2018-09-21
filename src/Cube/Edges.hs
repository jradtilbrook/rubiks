module Cube.Edges where

import Data.Maybe
import qualified Data.Vector.Unboxed as V

numEdges :: Int
numEdges = 12

data Edge = Edge { orien :: V.Vector Int, perm :: V.Vector Int } deriving (Show)

makePermutation :: V.Vector Int -> Maybe (V.Vector Int)
makePermutation v = if V.length v /= numEdges then Nothing else Just v

makeOrientation :: V.Vector Int -> Maybe (V.Vector Int)
makeOrientation = makePermutation
