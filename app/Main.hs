module Main where

import Criterion.Main
import Cube
import Moves.Edges
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = defaultMain [
    bgroup "inverse front" [
        bench "composition" $ whnf front'' solvedEdge,
        bench "single" $ whnf front' solvedEdge
        ]
    ]

-- this is a test function that uses simple composition to call the front function three time to achieve an inverse
front'' = front . front . front

-- this is a single function that combines the permutation vector to apply it in a single call
front' (Edge edgeOrien edgePerm) =
    Edge ov pv
    where
        ov = V.backpermute edgeOrien move
        pv = V.backpermute edgePerm move
        move = V.fromList [0, 1, 2, 3, 4, 8, 9, 7, 6, 5, 10, 11]
