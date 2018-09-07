module Cubie.Corners where

import Data.Maybe
import qualified Data.Vector.Unboxed as V

numCorners :: Int
numCorners = 8

data Corner = Corner { orien :: V.Vector Int, perm :: V.Vector Int } deriving (Show)

makePermutation :: V.Vector Int -> Maybe (V.Vector Int)
makePermutation v  = if V.length v /= numCorners then Nothing else Just v

makeOrientation :: V.Vector Int -> Maybe (V.Vector Int)
makeOrientation = makePermutation
