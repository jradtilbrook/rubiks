module Cubie.Moves.Edges
( front
, back
, left
, right
, up
, down
) where

import qualified Cubie.Moves as M (accumulate, permute)

-- set the bound on the accumulator
accumulate = M.accumulate 2

{-
 - Apply a front face rotation to the given orientation vectors
 -}
front (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = M.permute edgeOrien move
        pv = M.permute edgePerm move
        move = [0, 1, 2, 3, 4, 9, 8, 7, 5, 6, 10, 11]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = M.permute edgeOrien move
        pv = M.permute edgePerm move
        move = [0, 1, 3, 2, 11, 5, 6, 10, 8, 9, 4, 7]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = M.permute edgeOrien move
        pv = M.permute edgePerm move
        move = [4, 5, 3, 2, 1, 0, 6, 7, 8, 9, 10, 11]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = M.permute edgeOrien move
        pv = M.permute edgePerm move
        move = [0, 1, 7, 6, 4, 5, 2, 3, 8, 9, 10, 11]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute edgeOrien move) $ zip [0, 3, 8, 11] [1, 1..]
        pv = M.permute edgePerm move
        move = [8, 1, 2, 11, 4, 5, 6, 7, 3, 9, 10, 0]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down (edgeOrien, edgePerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute edgeOrien move) $ zip [1, 2, 9, 10] [1, 1..]
        pv = M.permute edgePerm move
        move = [0, 10, 9, 3, 4, 5, 6, 7, 8, 1, 2, 11]
