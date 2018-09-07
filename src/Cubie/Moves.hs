module Cubie.Moves where

{-
 - Similar to function composition for applying moves to a cube.
 - Start with the cube state, then write each move you want to take in order.
 - Eg. Cube -: down -: right -: front
 -}
x -: f = f x
