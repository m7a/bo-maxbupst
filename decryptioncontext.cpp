#include <stdexcept>
#include <cstring>
#include <cstdint>

#include <cryptopp/donna.h>

#include "decryptioncontext.hpp"
#include "zblake3.hpp"

// https://libsodium.gitbook.io/doc/public-key_cryptography/authenticated_encryption
// /usr/include/sodium/crypto_box_curve25519xchacha20poly1305.h

		// crypto::DecryptionContext::new(k.metadata_sk, k.metadata_psk)
		//struct bupstash_box_key ephemeral_bk = {}; // init to 0
		// beforenm := shared key for multiple messages?
		// TODO DECRYPT HERE
/*
        sodium::crypto_box_curve25519xchacha20poly1305_beforenm(
            unmixed_key_bytes.as_mut_ptr(),
            pk.bytes.as_ptr(),
            sk.bytes.as_ptr(),
        )
*/
/*
pub const HASH_BYTES: usize = 32;

pub const BOX_NONCEBYTES: usize = 24
    sodium::crypto_box_curve25519xchacha20poly1305_NONCEBYTES as usize;
pub const BOX_PUBLICKEYBYTES: usize = 32
    sodium::crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES as usize;
pub const BOX_SECRETKEYBYTES: usize = 32
    sodium::crypto_box_curve25519xchacha20poly1305_SECRETKEYBYTES as usize;
pub const BOX_MACBYTES: usize = sodium::crypto_box_curve25519xchacha20poly1305_MACBYTES as usize; = 16

pub const BOX_PRE_SHARED_KEY_BYTES: usize = 32;

pub const RANDOM_SEED_BYTES: usize = 32;
impl DecryptionContext {
    pub fn decrypt_data(&mut self, ct: Vec<u8>) -> Result<Vec<u8>, anyhow::Error> {
	// ...

        if !box_decrypt(
            &mut pt,
            &ct[..ct.len() - BOX_PUBLICKEYBYTES],
            &self.ephemeral_bk,
        ) {
            anyhow::bail!("data corrupt");
        }

        compression::decompress(pt)
    }
}
#[inline(always)]
pub fn box_compute_key(pk: &BoxPublicKey, sk: &BoxSecretKey, psk: &BoxPreSharedKey) -> BoxKey {
    let mut unmixed_key_bytes = [0; BOX_BEFORENMBYTES];
    if unsafe {
        sodium::crypto_box_curve25519xchacha20poly1305_beforenm(
            unmixed_key_bytes.as_mut_ptr(),
            pk.bytes.as_ptr(),
            sk.bytes.as_ptr(),
        )
    } == 0
    {
        BoxKey {
            bytes: blake3::keyed_hash(&psk.bytes, &unmixed_key_bytes[..]).into(),
        }
    } else {
        BoxKey {
            bytes: [0; BOX_BEFORENMBYTES],
        }
    }
}
*/

namespace Bupstash {

	DecryptionContext::DecryptionContext(const uint8_t* sk,
				const uint8_t* psk): paramSK(sk), paramPSK(psk)
	{
		ephemeralBK = {}; // init to zero
	}

	std::size_t DecryptionContext::getPlaintextLength(
						std::size_t ciphertextLength)
	{
		return ciphertextLength - BUPSTASH_BOX_NONCEBYTES -
			BUPSTASH_BOX_MACBYTES - BUPSTASH_BOX_PUBLICKEYBYTES;
	}

	void DecryptionContext::decrypt(uint8_t* out, const uint8_t* ciphertext,
						std::size_t ciphertextLength)
	{
		const uint8_t* pkSlice = ciphertext + (ciphertextLength -
						BUPSTASH_BOX_PUBLICKEYBYTES);
		if(memcmp(pkSlice, &ephemeralPK,
					BUPSTASH_BOX_PUBLICKEYBYTES) != 0) {
			memcpy(&ephemeralPK, pkSlice,
						BUPSTASH_BOX_PUBLICKEYBYTES);
			boxComputeKey();
		}

		boxDecrypt(out, ciphertext, ciphertextLength -
						BUPSTASH_BOX_PUBLICKEYBYTES);

		// TODO COMPRESSION::DECOMPRESS (compression.rs)!
	}

	void DecryptionContext::boxComputeKey()
	{
		uint8_t unmixedKeyBytes[BUPSTASH_BOX_BEFORENMBYTES] = {};
		box_curve25519xchacha20poly1305_beforenm(unmixedKeyBytes);

		// TODO z ONCE CRYPTO++ SUPPORTS IT, SWITCH TO ITS IMPLEMENTATION HERE!
		blake3_hasher ctx;
		blake3_hasher_init_keyed(&ctx, paramPSK);
		blake3_hasher_update(&ctx, unmixedKeyBytes,
						BUPSTASH_BOX_BEFORENMBYTES);
		blake3_hasher_finalize(&ctx, ephemeralBK.bytes,
						sizeof(ephemeralBK.bytes));
	}

	void DecryptionContext::box_curve25519xchacha20poly1305_beforenm(
						uint8_t* unmixedKeyBytes)
	{
		uint8_t s[32];
		if(CryptoPP::Donna::curve25519_mult(s, paramSK,
						 ephemeralPK.bytes) != 0)
			throw std::runtime_error(
				"Curve25519_mult failed. Invalid signature?");

		// TODO NOW crypto_core_hchacha20(k, zero, s, NULL)
		// https://github.com/jedisct1/libsodium/blob/master/src/libsodium/crypto_core/hchacha20/core_hchacha20.c
		// COMPARE
		// https://sources.debian.org/src/libcrypto++/8.4.0-1/chacha.cpp/
		// IT MIGHT BE WE DO NOT WANT TO PRECOMPUTE THIS HACHACHA AS IT MIGHT RATHER BE PART OF THE ALGORITHM ALREADY AND UNLIKW WITH LIBSODIUM ONE CANNOT RE-USE A PRECOMPUTED VALUE HERE?
/*

int
crypto_box_curve25519xchacha20poly1305_beforenm(unsigned char *k,
                                                const unsigned char *pk,
                                                const unsigned char *sk)
{
    static const unsigned char zero[16] = { 0 };
    unsigned char s[32];

    if (crypto_scalarmult_curve25519(s, sk, pk) != 0) {
        return -1;
    }
    return crypto_core_hchacha20(k, zero, s, NULL);
}

*/
	}

	// https://sources.debian.org/src/libsodium/1.0.18-1/src/libsodium/crypto_box/curve25519xchacha20poly1305/box_curve25519xchacha20poly1305.c/
	void DecryptionContext::boxDecrypt(uint8_t* out, const uint8_t* bt,
				const std::size_t btl)
	{
		// box_decrypt
		if(btl < BUPSTASH_BOX_NONCEBYTES + BUPSTASH_BOX_MACBYTES)
			throw std::runtime_error(
				"Boxed ciphertext length must at least "
				"contain nonce and MAC. Expected length " +
				std::to_string(BUPSTASH_BOX_NONCEBYTES +
				BUPSTASH_BOX_MACBYTES) + ", but found only " +
				std::to_string(btl) + " bytes."
			);
		// crypto_box_curve25519xchacha20poly1305_open_easy_afternm
		const uint8_t* nonce = bt;
		const uint8_t* ciphertextWoNonce = bt + BUPSTASH_BOX_NONCEBYTES;
		const std::size_t lengthWoNonce = btl - BUPSTASH_BOX_NONCEBYTES;
		// crypto_box_curve25519xchacha20poly1305_open_detached_afternm
		const uint8_t* mac = ciphertextWoNonce;
		const uint8_t* ciphertext = ciphertextWoNonce +
							BUPSTASH_BOX_MACBYTES;
		const std::size_t lengthCiphertext =
					lengthWoNonce - BUPSTASH_BOX_MACBYTES;
		// crypto_secretbox_xchacha20poly1305_open_detached
		secretbox_xchacha20poly1305_open_detached(out,
			ciphertext, mac, lengthCiphertext, nonce);
	}

	// https://sources.debian.org/src/libsodium/1.0.18-1/src/libsodium/crypto_secretbox/xchacha20poly1305/secretbox_xchacha20poly1305.c/
	void DecryptionContext::secretbox_xchacha20poly1305_open_detached(
		uint8_t* out, const uint8_t* ct, const uint8_t* mac,
		const std::size_t ctl, const uint8_t* nonce)
	{
		// bk = member ephemeralBK
		// TODO TRANSLATE FROM C / see link
	}
}
