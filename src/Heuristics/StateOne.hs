module Heuristics.StateOne
( binaryFold
) where

{-
 - This is used as a reducer for the edge orientation vector.
 - It treats the vector as a binary number and converts it to decimal using little-endian.
 -}
binaryFold carry index value = carry + value * 2 ^ index
