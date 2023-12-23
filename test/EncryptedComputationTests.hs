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
import Data.Foldable (Foldable(length))

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

oneBitAdderWrapper :: (Bits a) => BitComputation [a] [a]
oneBitAdderWrapper = proc l -> do
    a <- bind -< head l
    b <- bind -< l !! 1
    c <- bind -< l !! 2

    (s, cout) <- oneBitAdder -< (a, b, c)
    returnA -< [s, cout]

nBitAdderWrapper :: (Bits a) => Int -> BitComputation [a] [a]
nBitAdderWrapper n = proc l -> do
    x <- bind -< take n l
    y <- bind -< drop n l

    -- ignore carry
    (ss, _) <- nBitAdder n -< (x, y)
    returnA -< ss

testEncryptedComputation :: SpecWith ()
testEncryptedComputation = do
    describe "Encrypted computation" $ do
        it "x && 0" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            e1 <- encrypt_bit priv 1
            r1 <- runExpressionEncrypted (compile (andcons (BECons False))) pub [e1]
            d1 <- decrypt_bit priv r1

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext r1

            d1 `shouldBe` 0

        it "one bit adder - 1 + 1 + 1 = 1 + carry" $ let compiled_adder = compileArray 3 oneBitAdderWrapper in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv [1, 1, 1]
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 1]

        it "one bit adder - 1 + 0 + 0 = 1 (no carry)" $ let compiled_adder = compileArray 3 oneBitAdderWrapper in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv [1, 0, 0]
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 0]

        it "n bit adder - 1010 + 0101 = 1111" $ let compiled_adder = compileArray 8 (nBitAdderWrapper 4) in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv ([1, 0, 1, 0] ++ [0, 1, 0, 1])
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 1, 1, 1]

        it "n bit adder - 0011 + 0101 = 1000" $ let compiled_adder = compileArray 8 (nBitAdderWrapper 4) in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv ([0, 0, 1, 1] ++ [0, 1, 0, 1])
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 0, 0, 0]

        it "n bit adder - 1111 + 1111 = 1110" $ let compiled_adder = compileArray 8 (nBitAdderWrapper 4) in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv ([1, 1, 1, 1] ++ [1, 1, 1, 1])
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 1, 1, 0]
