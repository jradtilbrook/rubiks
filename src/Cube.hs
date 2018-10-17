module Cube where

import qualified Data.Vector.Unboxed as V

data Corner = Corner (V.Vector Int) (V.Vector Int) deriving (Show)
data Edge = Edge (V.Vector Int) (V.Vector Int) deriving (Show)
data Cube = Cube { corners :: Corner, edges :: Edge } deriving (Show)

class Orientation o where
    orien :: o -> V.Vector Int
class Permutation p where
    perm :: p -> V.Vector Int

instance Orientation Corner where
    orien (Corner o _) = o
instance Orientation Edge where
    orien (Edge o _) = o

instance Permutation Corner where
    perm (Corner _ p) = p
instance Permutation Edge where
    perm (Edge _ p) = p

numCorners :: Int
numCorners = 8
numEdges :: Int
numEdges = 12

solvedCorner = Corner (V.replicate numCorners 0) (V.fromList $ take numCorners [1..])
solvedEdge = Edge (V.replicate numEdges 0) (V.fromList $ take numEdges [1..])
solvedCube = Cube solvedCorner solvedEdge
