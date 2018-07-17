module Lib
    ( someFunc
    ) where

import qualified Internal.Enum as I
import qualified Cubie.Corners as C

someFunc :: IO ()
someFunc = putStrLn $ show $ I.next C.Anticlockwise

