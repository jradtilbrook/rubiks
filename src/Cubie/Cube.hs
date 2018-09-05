module Cubie.Cube where

import qualified Cubie.Corners as C
import qualified Cubie.Edges as E

data Cube = Cube { corners :: C.Corner
                 , edges :: E.Edge
                 }
