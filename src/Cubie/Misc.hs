module Cubie.Misc where

import qualified Data.Vector.Unboxed as U

type Vector = U.Vector

length :: U.Unbox a => U.Vector a -> Int
length v = U.length v
