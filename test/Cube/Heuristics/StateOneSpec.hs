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

spec :: Spec
spec = describe "validate heuristicList for stage 1" $ do
    it "iteration 1" $ do
        lim <- getMoveLimit
        gen <- newStdGen -- global random number generator
        let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
            -- apply the moves to a solved cube and assert the distance is <= the number of moves applied
            in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)
    it "iteration 2" $ do
        lim <- getMoveLimit
        gen <- newStdGen -- global random number generator
        let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
            -- apply the moves to a solved cube and assert the distance is <= the number of moves applied
            in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)
    it "iteration 3" $ do
        lim <- getMoveLimit
        gen <- newStdGen -- global random number generator
        let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
            -- apply the moves to a solved cube and assert the distance is <= the number of moves applied
            in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)
    it "iteration 4" $ do
        lim <- getMoveLimit
        gen <- newStdGen -- global random number generator
        let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
            -- apply the moves to a solved cube and assert the distance is <= the number of moves applied
            in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)
    it "iteration 5" $ do
        lim <- getMoveLimit
        gen <- newStdGen -- global random number generator
        let ms = filteredMoves $ take lim $ randomRs (0::Int, length moves) gen
            -- apply the moves to a solved cube and assert the distance is <= the number of moves applied
            in foldl (-:) solvedEdges ms -: distanceToSolved `shouldSatisfy` (<= lim)
