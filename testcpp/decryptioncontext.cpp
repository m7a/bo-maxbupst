#include <stdexcept>
#include <cstring>
#include <cstdint>
#include <iostream>

#include <cryptopp/cryptlib.h>
#include <cryptopp/donna.h>
#include <cryptopp/chacha.h>
#include <cryptopp/chachapoly.h>

#include "decryptioncontext.hpp"
#include "zblake3.hpp"

// https://libsodium.gitbook.io/doc/public-key_cryptography/authenticated_encryption
// /usr/include/sodium/crypto_box_curve25519xchacha20poly1305.h

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
			std::cout << "boxComputeKey() -- BEGIN" << std::endl;
			boxComputeKey();
			std::cout << "boxComputeKey() -- END" << std::endl;
		}

		std::cout << "boxDecrypt() -- BEGIN" << std::endl;
		boxDecrypt(out, ciphertext, ciphertextLength -
						BUPSTASH_BOX_PUBLICKEYBYTES);
		std::cout << "boxDecrypt() -- END" << std::endl;

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
		const static uint8_t ZERO[16] = { 0 };

		uint8_t s[32]; // do not change to pointer to make sizeof work!
		if(CryptoPP::Donna::curve25519_mult(s, paramSK,
						 ephemeralPK.bytes) != 0)
			throw std::runtime_error(
				"Curve25519_mult failed. Invalid signature?");

		// crypto_core_hchacha(k, zero, s, NULL);
		//                    out  in  key init-parameters
		// TODO CSTAT SUBSTAT GETTING ERROR "THIS OBJECT REQUIRES AN IV" -- WOULD NEED TO CHECK IF AN IV = 0 IS A SUITABLE CHOICE TO ACHIVE THE SAME RESULTS AS THE LOW LEVEL crypto_core_hchacha routine?
		CryptoPP::ChaCha::Decryption hchacha;
		hchacha.SetKeyWithIV(s, sizeof(s), ZERO, 8); // TODO CSTAT GUESSED!
		hchacha.ProcessData(unmixedKeyBytes, ZERO, sizeof(ZERO));
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
		// byte *message, const byte *mac, size_t macSize, const byte *iv, int ivLength, const byte *aad, size_t aadLength, const byte *ciphertext, size_t ciphertextLength
		// TODO SMALL PROBLEM: IT MIGHT BE THAT THE DecryptAndVerify routine here differs in how it is implemented in Sodium. This will lead to a decryption failure? / NEED TO CHECK THE INPUT VALUES WRT. BUPSTASH'S IMPLEMENTATION?
		CryptoPP::XChaCha20Poly1305::Decryption decryptCtx;
		// TODO z IS IV = NONCE?
		decryptCtx.SetKeyWithIV(ephemeralBK.bytes,
					sizeof(ephemeralBK.bytes),
					nonce, BUPSTASH_BOX_NONCEBYTES);
		// TODO CSTAT BROKEN?
		if(!decryptCtx.DecryptAndVerify(out, mac, BUPSTASH_BOX_MACBYTES,
					nonce, BUPSTASH_BOX_NONCEBYTES,
					NULL, 0, ct, ctl))
			throw std::runtime_error("AEAD Decryption failed. "
					"This might indicate the data has "
					"been tampered with.");
	}
}
