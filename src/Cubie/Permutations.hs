module Cubie.Permutations where

import qualified Data.Vector.Unboxed as U

{- Permutations of corner cubies -}
cornerPermutationFront :: U.Vector Int
cornerPermutationFront = U.fromList [0, 6, 2, 4, 1, 5, 3, 7]

cornerPermutationBack :: U.Vector Int
cornerPermutationBack = U.fromList [5, 4, 2, 3, 0, 1, 6, 7]

cornerPermutationLeft :: U.Vector Int
cornerPermutationLeft = U.fromList [5, 4, 2, 3, 0, 1, 6, 7]

cornerPermutationRight :: U.Vector Int
cornerPermutationRight = U.fromList [0, 1, 7, 6, 4, 5, 2, 3]

cornerPermutationUp :: U.Vector Int
cornerPermutationUp = U.fromList [4, 1, 2, 7, 3, 5, 6, 0]

cornerPermutationDown :: U.Vector Int
cornerPermutationDown = U.fromList [0, 5, 6, 3, 4, 2, 1, 7]

{- Orientations of corner cubies -}
cornerOrientationFront :: U.Vector Int
cornerOrientationFront = U.fromList [0, 1, 0, 1, -1, 0, -1, 0]

cornerOrientationBack :: U.Vector Int
cornerOrientationBack = U.fromList [1, 0, 1, 0, 0, -1, 0, -1]

cornerOrientationUp :: U.Vector Int
cornerOrientationUp = U.fromList [-1, 0, 0, -1, 1, 0, 0, 1]

cornerOrientationDown :: U.Vector Int
cornerOrientationDown = U.fromList [0, -1, -1, 0, 0, 1, 1, 0]

{- Permutations of edge cubies -}
edgePermutationFront :: U.Vector Int
edgePermutationFront = U.fromList [0, 1, 2, 3, 4, 9, 8, 7, 5, 6, 10, 11]

edgePermutationBack :: U.Vector Int
edgePermutationBack = U.fromList [0, 1, 2, 3, 11, 5, 6, 10, 8, 9, 4, 7]

edgePermutationLeft :: U.Vector Int
edgePermutationLeft = U.fromList [4, 5, 2, 3, 1, 0, 6, 7, 8, 9, 10, 11]

edgePermutationRight :: U.Vector Int
edgePermutationRight = U.fromList [0, 1, 7, 6, 4, 5, 2, 3, 8, 9, 0, 11]

edgePermutationUp :: U.Vector Int
edgePermutationUp = U.fromList [8, 1, 2, 11, 4, 5, 6, 7, 3, 9, 10, 0]

edgePermutationDown :: U.Vector Int
edgePermutationDown = U.fromList [0, 10, 9, 3, 4, 5, 6, 7, 8, 1, 2, 11]

{- Orientations of edge cubies -}
edgeOrientationFront :: U.Vector Int
edgeOrientationFront = U.replicate 12 0 {- use a constant for 12 -}

edgeOrientationBack :: U.Vector Int
edgeOrientationBack = edgeOrientationFront

edgeOrientationUp :: U.Vector Int
edgeOrientationUp = U.fromList [1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1]

edgeOrientationDown :: U.Vector Int
edgeOrientationDown = U.fromList [0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0]
