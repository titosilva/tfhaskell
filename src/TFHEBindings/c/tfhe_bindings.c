#include <tfhe/tfhe.h>
#include <tfhe/tfhe_io.h>
#include "tfhe_bindings.h"
#include <stdlib.h>
#include <stdio.h>

Ptr gen_key_pair(int seed) {
    const int minimum_lambda = 110;
    TFheGateBootstrappingParameterSet* params = new_default_gate_bootstrapping_parameters(minimum_lambda);

    uint32_t hseed[] = { 314, 1592, 657 };
    tfhe_random_generator_setSeed(hseed,3);
    TFheGateBootstrappingSecretKeySet* key = new_random_gate_bootstrapping_secret_keyset(params);

    TFHEKeyPair* kp = (TFHEKeyPair *) calloc(1, sizeof(TFHEKeyPair));

    kp->private_key = to_ptr(key);
    kp->public_key = to_ptr(&key->cloud);
    kp->params = to_ptr(params);

    return to_ptr(kp);
}

void delete_private_key(Ptr private_key) {
    delete_gate_bootstrapping_secret_keyset(from_ptr(private_key, TFheGateBootstrappingSecretKeySet));
}

void delete_public_key(Ptr public_key) {
    delete_gate_bootstrapping_cloud_keyset(from_ptr(public_key, TFheGateBootstrappingCloudKeySet));
}

void delete_params(Ptr params) {
    delete_gate_bootstrapping_parameters(from_ptr(params, TFheGateBootstrappingParameterSet));
}

void delete_key_pair(Ptr key_pair) {
    TFHEKeyPair* kp = from_ptr(key_pair, TFHEKeyPair);

    // delete private key here already deallocates the public key 
    // hence no need of calling delete_public_key
    delete_private_key(kp->private_key);
    delete_params(kp->params);
}

Ptr get_private_key_from_pair(Ptr key_pair) {
    TFHEKeyPair* kp = from_ptr(key_pair, TFHEKeyPair);
    TFHEKey* priv_key = (TFHEKey*) calloc(1, sizeof(TFHEKey));

    priv_key->key = kp->private_key;
    priv_key->params = kp->params;

    return to_ptr(priv_key);
}

Ptr get_public_key_from_pair(Ptr key_pair) {
    TFHEKeyPair* kp = from_ptr(key_pair, TFHEKeyPair);
    TFHEKey* pub_key = (TFHEKey*) calloc(1, sizeof(TFHEKey));

    pub_key->key = kp->public_key;
    pub_key->params = kp->params;

    return to_ptr(pub_key);
}

Ptr encrypt_bit(Ptr priv_key, int bit) {
    TFHEKey* key = from_ptr(priv_key, TFHEKey);

    LweSample* cipher = new_gate_bootstrapping_ciphertext(from_ptr(key->params, TFheGateBootstrappingParameterSet));
    bootsSymEncrypt(cipher, bit, from_ptr(key->key, TFheGateBootstrappingSecretKeySet));

    return to_ptr(cipher);
}

int decrypt_bit(Ptr priv_key, Ptr encrypted_bit) {
    TFHEKey* key = from_ptr(priv_key, TFHEKey);

    LweSample* cipher = from_ptr(encrypted_bit, LweSample);
    int result = bootsSymDecrypt(cipher, from_ptr(key->key, TFheGateBootstrappingSecretKeySet));

    return result;
}

void delete_ciphertext(Ptr ciphertext) {
    delete_gate_bootstrapping_ciphertext(from_ptr(ciphertext, LweSample));
}
