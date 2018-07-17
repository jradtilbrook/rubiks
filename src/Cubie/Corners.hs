module Cubie.Corners where

data Orientation = Solved | Clockwise | Anticlockwise deriving (Show, Read, Eq, Bounded, Enum)

data OrientationVector = OrientationVector Orientation Orientation Orientation Orientation Orientation Orientation Orientation Orientation deriving (Show, Read, Eq)
