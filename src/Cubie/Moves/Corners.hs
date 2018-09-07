module Cubie.Moves.Corners
( front
, back
, left
, right
, up
, down
) where

import Cubie.Corners
import qualified Data.Vector.Unboxed as V

{-
 - Accumulator that ensures the values don't exceed 3. ie the domain is [-1,1]
 -}
accumulate = V.accum (\a b -> (a + b) `mod` 3)

{-
 - Apply a front face rotation to the given orientation vectors
 -}
front (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = accumulate (V.backpermute cornOrien move) $ zip [1, 3, 4, 6] [1, 1, -1, -1]
        pv = V.backpermute cornPerm move
        move = V.fromList [0, 6, 2, 4, 1, 5, 3, 7]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = accumulate (V.backpermute cornOrien move) $ zip [0, 2, 5, 7] [1, 1, -1, -1]
        pv = V.backpermute cornPerm move
        move = V.fromList [7, 1, 5, 3, 4, 0, 6, 2]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = V.backpermute cornOrien move
        pv = V.backpermute cornPerm move
        move = V.fromList [5, 4, 2, 3, 0, 1, 6, 7]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = V.backpermute cornOrien move
        pv = V.backpermute cornPerm move
        move = V.fromList [0, 1, 7, 6, 4, 5, 2, 3]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = accumulate (V.backpermute cornOrien move) $ zip [0, 3, 4, 7] [-1, -1, 1, 1]
        pv = V.backpermute cornPerm move
        move = V.fromList [4, 1, 2, 7, 3, 5, 6, 0]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down (Corner cornOrien cornPerm) =
    Corner ov pv
    where
        ov = accumulate (V.backpermute cornOrien move) $ zip [1, 2, 5, 6] [-1, -1, 1, 1]
        pv = V.backpermute cornPerm move
        move = V.fromList [0, 5, 6, 3, 4, 2, 1, 7]
