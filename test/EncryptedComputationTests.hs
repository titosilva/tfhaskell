{-# LANGUAGE Arrows #-}
module EncryptedComputationTests where

import Test.Hspec
import Data.Bits
import GHC.Natural (Natural)
import Data.List ( uncons )
import Data.Maybe
import Data.Int
import Control.Arrow

import TFHEBindings.TFHE
import TFHaskell.EncryptedComputation
import TFHaskell.BitExpressionTree
import TFHaskell.BitComputation
import TFHaskell.Circuits
import Data.Foldable (Foldable(length), concat)
import TFHaskell.Circuits (int16ToBitArray, nBitMux)
import TFHaskell.BitComputation (BitComputation)

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
    -- Encode the inputs
    -- The first element of the array is a
    -- the next element is b
    -- the next element is carry in
    a <- bind -< head l
    b <- bind -< l !! 1
    c <- bind -< l !! 2

    (s, cout) <- oneBitAdder -< (a, b, c)
    returnA -< [s, cout]

nBitAdderWrapper :: (Bits a) => Int -> BitComputation [a] [a]
nBitAdderWrapper n = proc l -> do
    -- Encode the inputs
    -- The inputs are composed of two numbers
    -- The first n elements are the first n-bit number
    -- The remaining n elements are the second n-bit number
    x <- bind -< take n l
    y <- bind -< drop n l

    -- ignore carry
    (ss, _) <- nBitAdder n -< (x, y)
    returnA -< ss

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Not enough elements"

-- Encode the inputs
-- the first p elements of the array are the selector bits
-- the last bits are the input bits. Each input has n bits, hence we "chunkify" those
encodeForMuxWrapper :: [Int16] -> [Int] -> [Int]
encodeForMuxWrapper xs sel = sel ++ concatMap int16ToBitArray xs

nBitMuxWrapper :: (Bits a) => Int -> Int -> BitComputation [a] [a]
nBitMuxWrapper n p = proc l -> do
    sel <- bind -< take p l
    x <- bind -< chunksOf n (drop p l)

    k <- nBitMux n p -< (x, sel)

    returnA -< k

testEncryptedComputation :: SpecWith ()
testEncryptedComputation = do
    describe "EncryptedComputation" $ do
        it "x & 0 = 0" $ do
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

        it "4 bit adder - b0011 + b0101 = b1000" $ let compiled_adder = compileArray 8 (nBitAdderWrapper 4) in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv ([0, 0, 1, 1] ++ [0, 1, 0, 1])
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 0, 0, 0]

        it "4 bit adder - b1111 + b1111 = b1110" $ let compiled_adder = compileArray 8 (nBitAdderWrapper 4) in do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            es <- encryptBits priv ([1, 1, 1, 1] ++ [1, 1, 1, 1])
            rs <- runExpressionArrayEncrypted compiled_adder pub es
            ds <- decryptBits priv rs

            delete_key_pair kp
            deleteCiphertexts (es ++ rs)

            ds `shouldBe` [1, 1, 1, 0]

        it "16 bit adder - 5 + 2 = 7" $ let
                n = 16
                compiled_adder = compileArray (2 * n) (nBitAdderWrapper n)
            in do
                kp <- gen_key_pair 0
                priv <- get_private_key_from_pair kp
                pub <- get_public_key_from_pair kp

                es <- encryptBits priv (int16ToBitArray 5 ++ int16ToBitArray 2)
                rs <- runExpressionArrayEncrypted compiled_adder pub es
                ds <- decryptBits priv rs

                delete_key_pair kp
                deleteCiphertexts (es ++ rs)

                ds `shouldBe` int16ToBitArray 7

        it "16 bit mux 4 to 1 - selector 00 should select first option" $ let
                n = 16
                p = 2 -- 4 = 2 ^ 2
                -- The input will have p times n bits of inputs
                -- Plus 2 bits for selector
                compiled_mux = compileArray (2 + (2^p) * n) (nBitMuxWrapper n p)
            in do
                kp <- gen_key_pair 0
                priv <- get_private_key_from_pair kp
                pub <- get_public_key_from_pair kp

                es <- encryptBits priv (encodeForMuxWrapper [23, 45, 12, 64] [0, 0])
                rs <- runExpressionArrayEncrypted compiled_mux pub es
                ds <- decryptBits priv rs

                delete_key_pair kp
                deleteCiphertexts (es ++ rs)

                ds `shouldBe` int16ToBitArray 23
        
        it "16 bit mux 4 to 1 - selector 11 should select last option" $ let
                n = 16
                p = 2 -- 4 = 2 ^ 2
                -- The input will have p times n bits of inputs
                -- Plus 2 bits for selector
                compiled_mux = compileArray (2 + (2^p) * n) (nBitMuxWrapper n p)
            in do
                kp <- gen_key_pair 0
                priv <- get_private_key_from_pair kp
                pub <- get_public_key_from_pair kp

                es <- encryptBits priv (encodeForMuxWrapper [23, 45, 12, 64] [1, 1])
                rs <- runExpressionArrayEncrypted compiled_mux pub es
                ds <- decryptBits priv rs

                delete_key_pair kp
                deleteCiphertexts (es ++ rs)

                ds `shouldBe` int16ToBitArray 64

        it "16 bit mux 4 to 1 - selector 01 should select second option" $ let
                n = 16
                p = 2 -- 4 = 2 ^ 2
                -- The input will have p times n bits of inputs
                -- Plus 2 bits for selector
                compiled_mux = compileArray (2 + (2^p) * n) (nBitMuxWrapper n p)
            in do
                kp <- gen_key_pair 0
                priv <- get_private_key_from_pair kp
                pub <- get_public_key_from_pair kp

                es <- encryptBits priv (encodeForMuxWrapper [-1, -10, 90, 52] [0, 1])
                rs <- runExpressionArrayEncrypted compiled_mux pub es
                ds <- decryptBits priv rs

                delete_key_pair kp
                deleteCiphertexts (es ++ rs)

                ds `shouldBe` int16ToBitArray (-10)
