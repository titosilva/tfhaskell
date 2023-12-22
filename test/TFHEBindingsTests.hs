module TFHEBindingsTests where

import Test.Hspec
import TFHEBindings.TFHE

testTFHEBindings :: SpecWith ()
testTFHEBindings = do
    describe "TFHEBindings" $ do
        it "Encrypt then decrypt bit" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            encrypted <- encrypt_bit priv 1
            decrypted <- decrypt_bit priv encrypted
            delete_key_pair kp
            delete_ciphertext encrypted

            decrypted `shouldBe` 1
        it "Encrypted and" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            e1 <- encrypt_bit priv 1
            e2 <- encrypt_bit priv 0
            e3 <- encrypt_bit priv 1
            
            r1 <- encrypted_and pub e1 e2
            r2 <- encrypted_and pub e1 e3

            d1 <- decrypt_bit priv r1
            d2 <- decrypt_bit priv r2

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext e2
            delete_ciphertext e3
            delete_ciphertext r1
            delete_ciphertext r2

            d1 `shouldBe` 0
            d2 `shouldBe` 1
        it "Encrypted or" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            e1 <- encrypt_bit priv 1
            e2 <- encrypt_bit priv 0
            e3 <- encrypt_bit priv 0
            
            r1 <- encrypted_or pub e1 e2
            r2 <- encrypted_or pub e2 e3

            d1 <- decrypt_bit priv r1
            d2 <- decrypt_bit priv r2

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext e2
            delete_ciphertext e3
            delete_ciphertext r1
            delete_ciphertext r2

            d1 `shouldBe` 1
            d2 `shouldBe` 0
        it "Encrypted not" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            e1 <- encrypt_bit priv 1
            e2 <- encrypt_bit priv 0
            
            r1 <- encrypted_not pub e1
            r2 <- encrypted_not pub e2

            d1 <- decrypt_bit priv r1
            d2 <- decrypt_bit priv r2

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext e2
            delete_ciphertext r1
            delete_ciphertext r2

            d1 `shouldBe` 0
            d2 `shouldBe` 1
        it "Encrypted xor" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            e1 <- encrypt_bit priv 1
            e2 <- encrypt_bit priv 0
            e3 <- encrypt_bit priv 1
            
            r1 <- encrypted_xor pub e1 e2
            r2 <- encrypted_xor pub e1 e3

            d1 <- decrypt_bit priv r1
            d2 <- decrypt_bit priv r2

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext e2
            delete_ciphertext e3
            delete_ciphertext r1
            delete_ciphertext r2

            d1 `shouldBe` 1
            d2 `shouldBe` 0
        it "Encrypted constant" $ do
            kp <- gen_key_pair 0
            priv <- get_private_key_from_pair kp
            pub <- get_public_key_from_pair kp

            c1 <- encrypted_constant pub 1
            c2 <- encrypted_constant pub 0

            e1 <- encrypt_bit priv 1
            r1 <- encrypted_xor pub e1 c1
            r2 <- encrypted_xor pub e1 c2
            d1 <- decrypt_bit priv r1
            d2 <- decrypt_bit priv r2

            delete_key_pair kp
            delete_ciphertext e1
            delete_ciphertext r1
            delete_ciphertext r2
            delete_ciphertext c1
            delete_ciphertext c2

            d1 `shouldBe` 0
            d2 `shouldBe` 1