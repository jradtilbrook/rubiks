module Cubie.Moves
( accumulate
, permute
, (-:)
) where

import Data.Vector.Unboxed (accum, backpermute, fromList, Vector)

{-
 - Accumulate a vector using the provided array and a generic addition lambda with specific bound.
 - This is used to modify elements of a vector based on the index and value pair.
 -}
accumulate modulus = accum (\a b -> (a + b) `mod` modulus)

{-
 - Permute the given vector with the given sequence
 - This is used to reorder a vector based on the values in the provided list.
 -}
permute v s = backpermute v $ fromList s

{-
 - Similar to function composition for applying moves to a cube.
 - Start with the cube state, then write each move you want to take in order.
 - Eg. Cube -: down -: right -: front
 -}
x -: f = f x
