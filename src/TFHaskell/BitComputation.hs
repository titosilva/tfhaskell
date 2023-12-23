{-# LANGUAGE Arrows #-} 
{-# LANGUAGE InstanceSigs #-}
module TFHaskell.BitComputation where

import Control.Category
import Control.Arrow

newtype BitComputation a b = BitComputation (a -> b)

runComputation :: BitComputation a b -> a -> b
runComputation (BitComputation f) = f

instance Category BitComputation where
    (.) :: BitComputation b c -> BitComputation a b -> BitComputation a c
    (BitComputation f) . (BitComputation g) = BitComputation (f Prelude.. g)
    id = arr Control.Category.id

instance Arrow BitComputation where
    arr = BitComputation
    first (BitComputation f) = BitComputation (mapFst f) where mapFst g (a, b) = (g a, b)
    second (BitComputation f) = BitComputation (mapSnd f) where mapSnd g (a, b) = (a, g b)
