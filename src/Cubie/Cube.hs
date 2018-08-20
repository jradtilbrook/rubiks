module Cubie.Cube where

import qualified Cubie.Corners as C
import qualified Cubie.Edges as E

data Cube = Cube { cornerOrientation :: C.OrientationVector
                 , cornerPermutation :: C.PermutationVector
                 , edgeOrientation :: E.OrientationVector
                 , edgePermutation :: E.PermutationVector
                 }
