module Heuristics.StateOne where

import Cube
import Moves.Edges
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as V

{-
 - A list of all 18 possible moves under the half-turn metric (HTM)
 -}
moves :: [Edge -> Edge]
moves = [
      front
    , back
    , left
    , right
    , up
    , down
    , front . front
    , back . back
    , left . left
    , right . right
    , up . up
    , down . down
    , front . front . front
    , back . back . back
    , left . left . left
    , right . right . right
    , up . up . up
    , down . down . down
    ]

{-
 - This is used as a reducer for the edge orientation vector.
 - It treats the vector as a binary number and converts it to decimal using little-endian.
 -}
binaryFold carry index value = carry + value * 2 ^ index

{-
 - Static lookup list. This is to be generated from a script ... TODO
 -}
heuristicList :: M.HashMap Int Int
heuristicList = M.singleton 0 0

{-
 - Generate a list of edge states by applying all moves to the provided state
 -}
nextStates :: Edge -> [Edge]
nextStates edge = map (\f -> f edge) moves
-- if any of the states are all zeros, discard them since theres nothing to come from it
{- nextStates edge = filter (\a -> V.sum (orien a) /= 0) $ map (\f -> f edge) moves -}

{-
 - Map the provided list of edge states to their corresponding heuristic index value
 - TODO: this differs from the paper because the vector is reversed
 -}
heuristicIndices :: [Edge] -> [Int]
heuristicIndices = map $ V.ifoldl binaryFold 0 . V.init . orien
{- heuristicIndices = map $ V.ifoldl binaryFold 0 . V.init . V.reverse . orien -} -- this matches the matlab version

{-
 - Set the list of keys to the value in the provided hashmap
 -}
setHashKeys :: [Int] -> Int -> M.HashMap Int Int -> M.HashMap Int Int
setHashKeys [] _ hash = hash
setHashKeys (x:xs) dist hash = M.insertWith seq x dist $ setHashKeys xs dist hash

{-
 - Given a list of edge states and distance
 -}
generateLookup :: [(Int, Edge)] -> M.HashMap Int Int -> M.HashMap Int Int
generateLookup [] hash = hash
generateLookup _ hash | M.size hash >= 400 = hash
generateLookup (x:xs) hash = generateLookup visiting hash'
    where
        -- get the list of next achievable states from the current one
        newStates = nextStates $ snd x
        -- get the corresponding heuristic indices for the next states and add them to hashmap
        heuristics = heuristicIndices newStates
        hash' = setHashKeys heuristics (fst x + 1) hash
        -- add the new states and their distances to the list to travel to
        visiting = xs ++ zip (repeat (fst x + 1)) newStates
