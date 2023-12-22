#pragma once

#include <stdint.h>

#define Ptr int64_t*
#define to_ptr(x) (int64_t*)x
#define from_ptr(x, t) ((t*)x)

typedef struct __TFHEKeyPair_str {
    Ptr private_key;
    Ptr public_key;
    Ptr params;
} TFHEKeyPair;

typedef struct __TFHEKey_str {
    Ptr key;
    Ptr params;
} TFHEKey;

Ptr gen_key_pair(int seed);
void delete_private_key(Ptr private_key);
void delete_public_key(Ptr public_key);
void delete_params(Ptr params);
void delete_key_pair(Ptr key_pair);

Ptr get_private_key_from_pair(Ptr key_pair);
Ptr get_public_key_from_pair(Ptr key_pair);

Ptr encrypt_bit(Ptr priv_key, int bit);
int decrypt_bit(Ptr priv_key, Ptr encrypted_bit);
void delete_ciphertext(Ptr ciphertext);

Ptr encrypted_and(Ptr pub_key, Ptr node1, Ptr node2);
Ptr encrypted_or(Ptr pub_key, Ptr node1, Ptr node2);
Ptr encrypted_not(Ptr pub_key, Ptr node);
Ptr encrypted_xor(Ptr pub_key, Ptr node1, Ptr node2);
Ptr encrypted_constant(Ptr pub_key, int bit);
