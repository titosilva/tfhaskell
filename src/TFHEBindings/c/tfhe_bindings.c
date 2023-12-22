#include <tfhe/tfhe.h>
#include <tfhe/tfhe_io.h>
#include "tfhe_bindings.h"
#include <stdlib.h>

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