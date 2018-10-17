module Cube.Moves where

import Cube
import qualified Cube.Moves.Corners as Corner
import qualified Cube.Moves.Edges as Edge

{-
 - Similar to function composition for applying moves to a cube.
 - Start with the cube state, then write each move you want to take in order.
 - Eg. Cube -: down -: right -: front
 -}
x -: f = f x

front (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.front corner
        edge' = Edge.front edge

back (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.back corner
        edge' = Edge.back edge

left (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.left corner
        edge' = Edge.left edge

right (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.right corner
        edge' = Edge.right edge

up (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.up corner
        edge' = Edge.up edge

down (Cube corner edge) = Cube corner' edge'
    where
        corner' = Corner.down corner
        edge' = Edge.down edge
