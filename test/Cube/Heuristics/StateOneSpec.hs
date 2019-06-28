module Cube.Heuristics.StateOneSpec (spec) where

import Cube
import Heuristics.StateOne
import Moves
import Test.Hspec
import System.Random

{-
 - Get a list of moves given by the input list of indices
 -}
filteredMoves :: [Int] -> [Edge -> Edge]
filteredMoves j = [x | (x, i) <- zip moves [1::Int ..], i `elem` j]

{-
 - Get a random number between [0, 7] for how many moves to apply to a cube
 -}
getMoveLimit = getStdRandom (randomR (0::Int, 7))

{-
 - Test the stage 1 heuristic by applying random moves and asserting the distance
 -}
randomTest = do
    lim <- getMoveLimit
    gen <- newStdGen -- global random number generator
    let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
        in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)

spec :: Spec
spec = describe "validate heuristicList for stage 1" $ do
    it "iteration 1" randomTest
    it "iteration 2" randomTest
    it "iteration 3" randomTest
    it "iteration 4" randomTest
    it "iteration 5" randomTest
    it "iteration 6" randomTest
    it "iteration 7" randomTest
    it "iteration 8" randomTest
