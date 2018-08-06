module Internal.Enum where

import qualified Cubie.Corners as C
import qualified Cubie.Edges as E

next :: (Bounded a, Enum a, Eq a) => a -> a
next o
   | o == maxBound = minBound
   | otherwise = succ o

prev :: (Bounded a, Enum a, Eq a) => a -> a
prev o
   | o == minBound = maxBound
   | otherwise = pred o
