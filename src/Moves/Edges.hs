module Moves.Edges
( front
, back
, left
, right
, up
, down
) where

import Cube
import qualified Data.Vector.Unboxed as V

{-
 - Accumulator that ensures the values don't exceed 2. ie the domain is [0,1]
 -}
accumulate = V.accum (\a b -> (a + b) `mod` 2)

{-
 - Apply the given permutation list to an edge state.
 -}
move m (Edge orien perm) = Edge ov pv
    where
        ov = V.backpermute orien move
        pv = V.backpermute perm move
        move = V.fromList m

{-
 - Apply a front face rotation to the given orientation vectors
 -}
front = move [0, 1, 2, 3, 4, 9, 8, 7, 5, 6, 10, 11]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back = move [0, 1, 3, 2, 11, 5, 6, 10, 8, 9, 4, 7]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left = move [4, 5, 3, 2, 1, 0, 6, 7, 8, 9, 10, 11]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right = move [0, 1, 7, 6, 4, 5, 2, 3, 8, 9, 10, 11]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up edge = Edge ov' pv
    where
        (Edge ov pv) = move [8, 1, 2, 11, 4, 5, 6, 7, 3, 9, 10, 0] edge
        ov' = accumulate ov $ zip [0, 3, 8, 11] [1, 1..]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down edge = Edge ov' pv
    where
        (Edge ov pv) = move [0, 10, 9, 3, 4, 5, 6, 7, 8, 1, 2, 11] edge
        ov' = accumulate ov $ zip [1, 2, 9, 10] [1, 1..]
