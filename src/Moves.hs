module Moves
( (-:)
, front
, back
, back'
, back2
, down
, front'
, front2
, left
, left'
, left2
, right
, right'
, right2
, up
, up'
, up2
, down'
, down2
) where

import Cube
import qualified Moves.Corners as Corner
import qualified Moves.Edges as Edge

{-
 - Similar to function composition for applying moves to a cube.
 - Start with the cube state, then write each move you want to take in order.
 - Eg. Cube -: down -: right -: front
 -}
x -: f = f x

front = move Corner.front Edge.front
front' = move Corner.front' Edge.front'
front2 = move Corner.front2 Edge.front2

back = move Corner.back Edge.back
back' = move Corner.back' Edge.back'
back2 = move Corner.back2 Edge.back2

left = move Corner.left Edge.left
left' = move Corner.left' Edge.left'
left2 = move Corner.left2 Edge.left2

right = move Corner.right Edge.right
right' = move Corner.right' Edge.right'
right2 = move Corner.right2 Edge.right2

up = move Corner.up Edge.up
up' = move Corner.up' Edge.up'
up2 = move Corner.up2 Edge.up2

down = move Corner.down Edge.down
down' = move Corner.down' Edge.down'
down2 = move Corner.down2 Edge.down2

move mc me (Cube corner edge) = Cube corner' edge'
    where
        corner' = mc corner
        edge' = me edge
