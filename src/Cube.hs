module Cube where

import qualified Data.Vector.Unboxed as V

data Corner = Corner (V.Vector Int) (V.Vector Int) deriving (Show)
data Edge = Edge (V.Vector Int) (V.Vector Int) deriving (Show)
data Cube = Cube { corners :: Corner, edges :: Edge } deriving (Show)

class Cubie c where
    cubie :: Int -> c -> Maybe (Int, Int)
class Orientation o where
    orien :: o -> V.Vector Int
class Permutation p where
    perm :: p -> V.Vector Int

instance Cubie Corner where
    cubie i (Corner o p) = if i >= numCorners then Nothing else Just (o V.! i, p V.! i)
instance Cubie Edge where
    cubie i (Edge o p) = if i >= numEdges then Nothing else Just (o V.! i, p V.! i)

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

solvedCorners = Corner (V.replicate numCorners 0) (V.fromList $ take numCorners [1..])
solvedEdges = Edge (V.replicate numEdges 0) (V.fromList $ take numEdges [1..])
solvedCube = Cube solvedCorners solvedEdges
