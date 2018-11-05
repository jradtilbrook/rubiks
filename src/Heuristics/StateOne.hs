module Heuristics.StateOne where

import Cube
import Data.Char
import Data.Hashable
import Moves ((-:))
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
    , front2
    , back2
    , left2
    , right2
    , up2
    , down2
    , front'
    , back'
    , left'
    , right'
    , up'
    , down'
    ]

{-
 - Used to unfold a hashmap key to a list of integers.
 - It just converts the numberic character at each index to its integer equivalent
 -}
unfoldToInt :: String -> Maybe (Int, String)
unfoldToInt [] = Nothing
unfoldToInt (x:xs) = Just (digitToInt x, xs)

{-
 - Used to fold a list of integers to a string.
 - It just prepends the character value of the integer to the string.
 - NOTE: Unfortunately this produces a string representation of the vector but in reverse order. This was specifically
 - chosen because it is used extensively in the solve algorithm so reversing the vector is unnecessary overhead.
 -}
foldString :: String -> Int -> String
foldString carry value = intToDigit value : carry

{-
 - Generate a list of edge states by applying all moves to the provided state.
 -}
nextStates :: Edge -> [Edge]
nextStates edge = map (edge -:) moves

{-
 - Map the provided list of edge states to their corresponding heuristic index value.
 -}
heuristicIndices :: [Edge] -> [String]
heuristicIndices = map $ V.foldl foldString "" . orien

{-
 - Set the list of keys to the value in the provided hashmap.
 - This uses `const` to ensure that duplicate keys already in the map are not overwritten.
 -}
setHashKeys :: (Eq a, Hashable a) => [a] -> Int -> M.HashMap a Int -> M.HashMap a Int
setHashKeys [] _ hash = hash
setHashKeys (x:xs) dist hash = setHashKeys xs dist $ M.insertWith (flip const) x dist hash

{-
 - Get a list of the keys for given value.
 -}
keysForDist :: Int -> M.HashMap a Int -> [a]
keysForDist dist = M.keys . M.filter (dist ==)

{-
 - This is a helper function to display the distance distribution of the heuristic map - mainly for debugging.
 -}
distribution :: M.HashMap a Int -> [(Int, Int)]
distribution hash = calculate 0
    where
        -- helper for calculating the number of occurrences of the given key
        count key = M.size $ M.filter (key ==) hash
        calculate dist = let a = count dist in if a <= 0 then [] else (dist, a) : calculate (dist + 1)

{-
 - The heuristic list for all possible edge orientation states.
 - This is calculated by visiting each possible state and indexing the number of moves required to reach it.
 - It is calculated when it is first evaluated but it is quite inexpensive and will be in memory afterwards anyway.
 -}
heuristic = generateLookup $ M.singleton (replicate numEdges '0') 0
    where
        generateLookup hash = if M.size hash >= 2048 then hash else generateLookup hash'
            where
                -- the current largest distance in the map
                dist = maximum $ M.elems hash
                -- get a list of vectors corresponding to the previously generated states at dist
                states = map (V.unfoldr unfoldToInt) $ keysForDist dist hash
                -- list of indices for the new edge states above
                indices = heuristicIndices $ concatMap (nextStates . (\s -> Edge s (V.fromList []))) states
                -- set those distance values in the hash
                hash' = setHashKeys indices (dist + 1) hash
