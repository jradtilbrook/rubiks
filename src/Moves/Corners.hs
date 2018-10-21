module Moves.Corners
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
 - Accumulator that ensures the values don't exceed 3. ie the domain is [-1,1]
 -}
accumulate = V.accum (\a b -> (a + b) `mod` 3)

{-
 - Apply the given permutation list to an edge state.
 -}
move m (Corner orien perm) = Corner ov pv
    where
        ov = V.backpermute orien move
        pv = V.backpermute perm move
        move = V.fromList m

{-
 - Apply a front face rotation to the given orientation vectors
 -}
front corner = Corner ov' pv
    where
        (Corner ov pv) = move [0, 6, 2, 4, 1, 5, 3, 7] corner
        ov' = accumulate ov $ zip [1, 3, 4, 6] [1, 1, -1, -1]
front' corner = Corner ov' pv
    where
        (Corner ov pv) = move [0, 4, 2, 6, 3, 5, 1, 7] corner
        ov' = accumulate ov $ zip [1, 3, 4, 6] [1, 1, -1, -1]
front2 = move [0, 3, 2, 1, 6, 5, 4, 7]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back corner = Corner ov' pv
    where
        (Corner ov pv) = move [7, 1, 5, 3, 4, 0, 6, 2] corner
        ov' = accumulate ov $ zip [0, 2, 5, 7] [1, 1, -1, -1]
back' corner = Corner ov' pv
    where
        (Corner ov pv) = move [5, 1, 7, 3, 4, 2, 6, 0] corner
        ov' = accumulate ov $ zip [0, 2, 5, 7] [1, 1, -1, -1]
back2 = move [2, 1, 0, 3, 4, 7, 6, 5]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left = move [5, 4, 2, 3, 0, 1, 6, 7]
left' = move [4, 5, 2, 3, 1, 0, 6, 7]
left2 = move [1, 0, 2, 3, 5, 4, 6, 7]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right = move [0, 1, 7, 6, 4, 5, 2, 3]
right' = move [0, 1, 6, 7, 4, 5, 3, 2]
right2 = move [0, 1, 3, 2, 4, 5, 7, 6]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up corner = Corner ov' pv
    where
        (Corner ov pv) = move [4, 1, 2, 7, 3, 5, 6, 0] corner
        ov' = accumulate ov $ zip [0, 3, 4, 7] [-1, -1, 1, 1]
up' corner = Corner ov' pv
    where
        (Corner ov pv) = move [7, 1, 2, 4, 0, 5, 6, 3] corner
        ov' = accumulate ov $ zip [0, 3, 4, 7] [-1, -1, 1, 1]
up2 = move [3, 1, 2, 0, 7, 5, 6, 4]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down corner = Corner ov' pv
    where
        (Corner ov pv) = move [0, 5, 6, 3, 4, 2, 1, 7] corner
        ov' = accumulate ov $ zip [1, 2, 5, 6] [-1, -1, 1, 1]
down' corner = Corner ov' pv
    where
        (Corner ov pv) = move [0, 6, 5, 3, 4, 1, 2, 7] corner
        ov' = accumulate ov $ zip [1, 2, 5, 6] [-1, -1, 1, 1]
down2 = move [0, 2, 1, 3, 4, 6, 5, 7]
