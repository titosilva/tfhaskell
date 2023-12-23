{-# LANGUAGE CApiFFI #-}
module TFHEBindings.TFHE (
    TFHEPtr,
    gen_key_pair,
    delete_key_pair,
    get_private_key_from_pair,
    get_public_key_from_pair,
    encrypt_bit,
    decrypt_bit,
    delete_ciphertext,
    encrypted_and,
    encrypted_or,
    encrypted_not,
    encrypted_xor,
    encrypted_constant,
    encryptBits,
    decryptBits,
    deleteCiphertexts
) where

import Data.Int
import Foreign.Ptr

type TFHEPtr = Ptr Int64

foreign import ccall "c/tfhe_bindings.h gen_key_pair" gen_key_pair :: Int -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h delete_key_pair" delete_key_pair :: TFHEPtr -> IO ()

foreign import ccall "c/tfhe_bindings.h get_private_key_from_pair" get_private_key_from_pair :: TFHEPtr -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h get_public_key_from_pair" get_public_key_from_pair :: TFHEPtr -> IO TFHEPtr

foreign import ccall "c/tfhe_bindings.h encrypt_bit" encrypt_bit :: TFHEPtr -> Int -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h decrypt_bit" decrypt_bit :: TFHEPtr -> TFHEPtr -> IO Int
foreign import ccall "c/tfhe_bindings.h delete_ciphertext" delete_ciphertext :: TFHEPtr -> IO ()

foreign import ccall "c/tfhe_bindings.h encrypted_and" encrypted_and :: TFHEPtr -> TFHEPtr -> TFHEPtr -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h encrypted_or" encrypted_or :: TFHEPtr -> TFHEPtr -> TFHEPtr -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h encrypted_not" encrypted_not :: TFHEPtr -> TFHEPtr -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h encrypted_xor" encrypted_xor :: TFHEPtr -> TFHEPtr -> TFHEPtr -> IO TFHEPtr
foreign import ccall "c/tfhe_bindings.h encrypted_constant" encrypted_constant :: TFHEPtr -> Int -> IO TFHEPtr

encryptBits :: TFHEPtr -> [Int] -> IO [TFHEPtr]
encryptBits priv = mapM (encrypt_bit priv)

decryptBits :: TFHEPtr -> [TFHEPtr] -> IO [Int]
decryptBits priv = mapM (decrypt_bit priv)

deleteCiphertexts :: [TFHEPtr] -> IO ()
deleteCiphertexts = mapM_ delete_ciphertext
