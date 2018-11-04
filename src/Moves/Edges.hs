module Moves.Edges
( front
, front'
, front2
, back
, back'
, back2
, left
, left'
, left2
, right
, right'
, right2
, up
, up'
, up2
, down
, down'
, down2
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
front' = move [0, 1, 2, 3, 4, 8, 9, 7, 6, 5, 10, 11]
front2 = move [0, 1, 2, 3, 4, 6, 5, 7, 9, 8, 10, 11]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back = move [0, 1, 3, 2, 11, 5, 6, 10, 8, 9, 4, 7]
back' = move [0, 1, 3, 2, 10, 5, 6, 11, 8, 9, 7, 4]
back2 = move [0, 1, 2, 3, 7, 5, 6, 4, 8, 9, 11, 10]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left = move [4, 5, 2, 3, 1, 0, 6, 7, 8, 9, 10, 11]
left' = move [5, 4, 2, 3, 0, 1, 6, 7, 8, 9, 10, 11]
left2 = move [1, 0, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right = move [0, 1, 7, 6, 4, 5, 2, 3, 8, 9, 10, 11]
right' = move [0, 1, 6, 7, 4, 5, 3, 2, 8, 9, 10, 11]
right2 = move [0, 1, 3, 2, 4, 5, 7, 6, 8, 9, 10, 11]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up edge = Edge ov' pv
    where
        (Edge ov pv) = move [8, 1, 2, 11, 4, 5, 6, 7, 3, 9, 10, 0] edge
        ov' = accumulate ov $ zip [0, 3, 8, 11] [1, 1..]
up' edge = Edge ov' pv
    where
        (Edge ov pv) = move [11, 1, 2, 8, 4, 5, 6, 7, 0, 9, 10, 3] edge
        ov' = accumulate ov $ zip [0, 3, 8, 11] [1, 1..]
up2 = move [3, 1, 2, 0, 4, 5, 6, 7, 11, 9, 10, 8]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down edge = Edge ov' pv
    where
        (Edge ov pv) = move [0, 10, 9, 3, 4, 5, 6, 7, 8, 1, 2, 11] edge
        ov' = accumulate ov $ zip [1, 2, 9, 10] [1, 1..]
down' edge = Edge ov' pv
    where
        (Edge ov pv) = move [0, 9, 10, 3, 4, 5, 6, 7, 8, 2, 1, 11] edge
        ov' = accumulate ov $ zip [1, 2, 9, 10] [1, 1..]
down2 = move [0, 2, 1, 3, 4, 5, 6, 7, 8, 10, 9, 11]
