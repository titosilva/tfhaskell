#pragma once

#include <stdint.h>

#define Ptr int64_t*
#define to_ptr(x) (int64_t*)x
#define from_ptr(x, t) ((t*)x)

void generate_key_pair(int ctx);
void create_encrypted_16bit_input_node(int ctx, int input_id, int16_t plaintext);
void compute_16bit_minimum(int ctx, int out_id, int input1, int input2);
int16_t decrypt_16bit_node(int ctx, int node_id);

typedef struct TFHEKeyPair_str {
    Ptr private_key;
    Ptr public_key;
    Ptr params;
} TFHEKeyPair;

Ptr gen_key_pair(int seed);
void delete_private_key(Ptr private_key);
void delete_public_key(Ptr public_key);
void delete_params(Ptr params);
void delete_key_pair(Ptr key_pair);
