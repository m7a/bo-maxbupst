#pragma once
#include <cstdint>
#include <string>
#include <map>
#include <vector>

// SERDE
// -----
//
// Original structures are from a Rust application and may contain
// variable-length fields. Decoding is not straight-forward. Relevant fields
// have been marked with `SERDE` and need to be decoded explicitly.
//
// Specification -> https://www.ietf.org/archive/id/draft-devault-bare-01.txt

// keys.rs | https://bupstash.io/doc/man/bupstash-keyfiles.html
struct bupstash_primary_key {
	uint8_t key_type; // 0: primary key v1, 1: sub key v1

	uint8_t data[16];
	uint8_t rollsum_key[32];
	uint8_t data_hash_key_part_1[32];
	uint8_t data_hash_key_part_2[32];
	uint8_t data_pk[32];
	uint8_t data_sk[32];
	uint8_t data_psk[32];
	uint8_t idx_hash_key_part_1[32];
	uint8_t idx_hash_key_part_2[32];
	uint8_t idx_pk[32];
	uint8_t idx_sk[32];
	uint8_t idx_psk[32];
	uint8_t metadata_pk[32];
	uint8_t metadata_sk[32];
	uint8_t metadata_psk[32];
};

// address.rs
#define BUPSTASH_ADDRESS_SZ 32

struct bupstash_address {
	uint8_t bytes[BUPSTASH_ADDRESS_SZ];
} __attribute__ ((packed));

// oplog.rs
struct bupstash_htree_metadata {
	uint64_t height;                             // SERDE UInt
	uint64_t data_chunk_count;                   // SERDE UInt
	struct bupstash_address address;
};

// xid.rs
struct bupstash_xid {
	uint8_t bytes[16];
} __attribute__ ((packed));

// oplog.rs
struct bupstash_v3_plain_text_item_metadata {
	uint8_t version; // 0: v1, 1: v2, 2: v3 here
	struct bupstash_xid primary_key_id;
	uint64_t unix_timestamp_millis;
	struct bupstash_htree_metadata data_tree;    // contains SERDE
	uint8_t optional_is_index_tree_present;      // 0: no, 1: yes
	struct bupstash_htree_metadata index_tree;   // contains SERDE
};

struct bupstash_v3_item_metadata {
	struct bupstash_v3_plain_text_item_metadata; // contains SERDE
	std::vector<uint8_t> encrypted_metadata;     // contains SERDE, vec<u8>
};

// crypto.rs
#define BUPSTASH_HASH_BYTES 32

struct bupstash_partial_hash_key {
	uint8_t bytes[BUPSTASH_HASH_BYTES];
} __attribute__ ((packed));

// oplog.rs
struct bupstash_v3_secret_item_metadata {
	uint8_t plain_text_hash[BUPSTASH_HASH_BYTES];
	struct bupstash_xid send_key_id;
	struct bupstash_partial_hash_key index_hash_key_part_2;
	struct bupstash_partial_hash_key data_hash_key_part_2;
	std::map<std::string, std::string> tags;     // BTreeMap<String, String>
	uint64_t data_size;                          // SERDE Uint
	uint64_t index_size;                         // SERDE Uint
};

// crypto.rs
#define BUPSTASH_BOX_BEFORENMBYTES  32
struct bupstash_box_key {
	uint8_t bytes[BUPSTASH_BOX_BEFORENMBYTES];
};
#define BUPSTASH_BOX_PUBLICKEYBYTES 32
struct bupstash_box_publickey {
	uint8_t bytes[BUPSTASH_BOX_PUBLICKEYBYTES];
};
#define BUPSTASH_BOX_NONCEBYTES     24
#define BUPSTASH_BOX_MACBYTES       16
