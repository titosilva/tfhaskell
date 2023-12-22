{-# LANGUAGE NoImplicitPrelude #-}
module TFHaskell.BitComputation where
    import Control.Category
    import Control.Arrow

    newtype BitComputation a b = BitComputation (a -> b)

    runComputation :: BitComputation a b -> a -> b
    runComputation (BitComputation f) = f

    instance Arrow BitComputation where
        arr = BitComputation
        first (BitComputation f) = BitComputation (mapFst f) where mapFst g (a, b) = (g a, b)
        second (BitComputation f) = BitComputation (mapSnd f) where mapSnd g (a, b) = (a, g b)

    instance Category BitComputation where
        (BitComputation f) . (BitComputation g) = BitComputation (f . g)
        id = arr Control.Category.id