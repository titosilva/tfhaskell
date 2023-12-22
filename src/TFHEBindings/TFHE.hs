{-# LANGUAGE CApiFFI #-}
module TFHEBindings.TFHE where

import Data.Int
import Foreign.Ptr

type TFHEPtr = Ptr Int64

foreign import ccall "c/tfhe_bindings.h gen_key_pair" gen_key_pair :: Int -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h delete_key_pair" delete_key_pair :: TFHEPtr -> IO ()
