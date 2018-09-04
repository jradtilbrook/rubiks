module Cubie.Moves.Corners
( front
, back
, left
, right
, up
, down
) where

import qualified Cubie.Moves as M (accumulate, permute)

-- set the bound on the accumulator
accumulate = M.accumulate 3

{-
 - Apply a front face rotation to the given orientation vectors
 -}
front (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute cornOrien move) $ zip [1, 3, 4, 6] [1, 1, -1, -1]
        pv = M.permute cornPerm move
        move = [0, 6, 2, 4, 1, 5, 3, 7]

{-
 - Apply a back face rotation to the given orientation vectors
 -}
back (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute cornOrien move) $ zip [0, 2, 5, 7] [1, 1, -1, -1]
        pv = M.permute cornPerm move
        move = [7, 1, 5, 3, 4, 0, 6, 2]

{-
 - Apply a left face rotation to the given orientation vectors
 -}
left (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = M.permute cornOrien move
        pv = M.permute cornPerm move
        move = [5, 4, 2, 3, 0, 1, 6, 7]

{-
 - Apply a right face rotation to the given orientation vectors
 -}
right (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = M.permute cornOrien move
        pv = M.permute cornPerm move
        move = [0, 1, 7, 6, 4, 5, 2, 3]

{-
 - Apply a up face rotation to the given orientation vectors
 -}
up (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute cornOrien move) $ zip [0, 3, 4, 7] [-1, -1, 1, 1]
        pv = M.permute cornPerm move
        move = [4, 1, 2, 7, 3, 5, 6, 0]

{-
 - Apply a down face rotation to the given orientation vectors
 -}
down (cornOrien, cornPerm) =
    (ov, pv)
    where
        ov = accumulate (M.permute cornOrien move) $ zip [1, 2, 5, 6] [-1, -1, 1, 1]
        pv = M.permute cornPerm move
        move = [0, 5, 6, 3, 4, 2, 1, 7]
