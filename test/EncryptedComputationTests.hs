{-# LANGUAGE Arrows #-}
module EncryptedComputationTests where

import Test.Hspec
import Data.Bits
import GHC.Natural (Natural)
import Data.List ( uncons )
import Data.Maybe
import Control.Arrow

import TFHEBindings.TFHE
import TFHaskell.EncryptedComputation
import TFHaskell.BitExpressionTree
import TFHaskell.BitComputation
import TFHaskell.Circuits

andcons :: (Arrow a, Bits c) => c -> a c c
andcons x = arr (x .&.)

orcons :: (Arrow a, Bits c) => c -> a c c
orcons x = arr (x .|.)

nots :: Bits a => BitComputation a a
nots = arr complement

mux2to1 :: (Bits a) => a -> a -> BitComputation a a
mux2to1 x y = proc z -> do
    a <- andcons x <<< nots -< z
    b <- andcons y -< z
    returnA -< (a .|. b)

-- testEncryptedComputation :: SpecWith ()
-- testEncryptedComputation = do
--     describe "Encrypted computation" $ do
--         it "x && 0" $ do
--             kp <- gen_key_pair 0
--             priv <- get_private_key_from_pair kp
--             pub <- get_public_key_from_pair kp

--             e1 <- encrypt_bit priv 1
--             r1 <- runExpressionEncrypted (compile (andcons (BECons False))) pub e1
--             d1 <- decrypt_bit priv r1

--             delete_key_pair kp
--             delete_ciphertext e1
--             delete_ciphertext r1

--             d1 `shouldBe` 0

--         it "mux2to1" $ let compiled_mux = compile (mux2to1 (bit 1) zeroBits) in do
--             kp <- gen_key_pair 0
--             priv <- get_private_key_from_pair kp
--             pub <- get_public_key_from_pair kp

--             e1 <- encrypt_bit priv 1
--             r1 <- runExpressionEncrypted compiled_mux pub e1
--             d1 <- decrypt_bit priv r1

--             e2 <- encrypt_bit priv 0
--             r2 <- runExpressionEncrypted compiled_mux pub e2
--             d2 <- decrypt_bit priv r2

--             delete_key_pair kp
--             delete_ciphertext e1
--             delete_ciphertext r1
--             delete_ciphertext e2
--             delete_ciphertext r2

--             d1 `shouldBe` 0
--             d2 `shouldBe` 1
