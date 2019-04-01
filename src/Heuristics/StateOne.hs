module Heuristics.StateOne where

import Cube
import Data.Char
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
heuristicIndex :: Edge -> String
heuristicIndex = V.foldl foldString "" . orien

{-
 - Set the list of keys to the value in the provided hashmap.
 - This uses `seq` to ensure that duplicate keys already in the map are not overridden
 -}
setHashKeys :: [String] -> Int -> M.HashMap String Int -> M.HashMap String Int
setHashKeys [] _ hash = hash
setHashKeys (x:xs) dist hash = setHashKeys xs dist $ M.insertWith seq x dist hash

{-
 - Get a list of the keys for given value.
 -}
keysForDist :: Int -> M.HashMap String Int -> [String]
keysForDist dist = M.keys . M.filter (dist ==)

{-
 - Ensure the given edge vector is the correct length.
 - This will pad vector with zeros up to 11 then calculate the final element.
 -}
makeOrientation v = V.replicate padLength 0 V.++ v V.++ V.singleton modulus
    where
        currentLength = V.length v
        padLength = numEdges - currentLength
        modulus = V.sum v `mod` 2

{-
 - This is a helper function to display the distance distribution of the heuristic map - mainly for debugging.
 -}
distribution :: M.HashMap String Int -> [(Int, Int)]
distribution hash = some [] 0
    where
        -- helper for calculating the number of occurences of the given key
        count key = M.size $ M.filter (key ==) hash
        some xs dist = let a = count dist in if a > 0 then xs ++ [(dist, a)] ++ some xs (dist + 1) else xs

{-
 - The heuristic list for all possible edge orientation states.
 - This is calculated by visiting each possible state and indexing the number of moves required to reach it.
 - It is calculated when it is first evaluated but it is quite inexpensive and will be in memory afterwards anyway.
 -}
heuristicList = generateLookup $ M.singleton (replicate numEdges '0') 0
    where
        generateLookup hash = if M.size hash >= 2048 then hash else generateLookup hash'
            where
                -- the current largest distance in the map
                dist = maximum $ M.elems hash
                -- get a list of vectors corresponding to the previously generated states at dist
                prevStates = map (V.unfoldr unfoldToInt) $ keysForDist dist hash
                -- ensure the edge vector is the correct length and calculate the final element
                states = map makeOrientation prevStates
                -- list of indices for the new edge states above
                indices = map heuristicIndex $ concatMap (nextStates . (\s -> Edge s (V.fromList []))) states
                -- set those distance values in the hash
                hash' = setHashKeys indices (dist + 1) hash

{-
 - Get the distance heuristic for the given cube
 -}
distanceToSolved :: Edges -> Int
distanceToSolved cube = M.lookupDefault 0 (heuristicIndex cube) heuristicList
