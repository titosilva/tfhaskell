#pragma once

#include <stdint.h>

#define Ptr int64_t*
#define to_ptr(x) (int64_t*)x
#define ptr_to_type()

void generate_key_pair(int ctx);
void create_encrypted_16bit_input_node(int ctx, int input_id, int16_t plaintext);
void compute_16bit_minimum(int ctx, int out_id, int input1, int input2);
int16_t decrypt_16bit_node(int ctx, int node_id);

typedef struct TFHEKeyPair_str {
    Ptr public_key;
    Ptr private_key;
} TFHEKeyPair;

TFHEKeyPair gen_key_pair(int seed);

