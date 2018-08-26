module Cubie.Heuristics.StateTwo
( ternaryFold
) where

{-
 - This is used as a reducer for the corner orientation vector.
 - It treats the vector as a ternary number and converts it to decimal using little-endian.
 -}
ternaryFold carry index value = carry + (value + 1) * 3 ^ index
