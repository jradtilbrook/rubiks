module Cubie.Cube where

import qualified Cubie.Corners as C
import qualified Cubie.Edges as E
import qualified Data.Vector.Unboxed as V

data Cube = Cube { corners :: C.Corner, edges :: E.Edge } deriving (Show)

solvedCube = Cube
    (C.Corner (V.replicate 8 0) (V.fromList $ take 8 [1..]))
    (E.Edge (V.replicate 12 0) (V.fromList $ take 12 [1..]))
