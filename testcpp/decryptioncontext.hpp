#pragma once

#include "bupstash_structures.hpp"

namespace Bupstash {

	class DecryptionContext {
	public:
		DecryptionContext(const uint8_t* pk, const uint8_t* sk);

		std::size_t getPlaintextLength(std::size_t ciphertextLength);

		void decrypt(uint8_t* out, const uint8_t* ciphertext,
						std::size_t cipertextLength);

	private:
		const uint8_t*                paramSK;
		const uint8_t*                paramPSK;
		struct bupstash_box_publickey ephemeralPK;
		struct bupstash_box_key       ephemeralBK;

		void boxComputeKey();
		void box_curve25519xchacha20poly1305_beforenm(
						uint8_t* unmixedKeyBytes);

		/**
		 * crypto.rs box_decrypt
		 * @param out -- plaintext output `m`
		 * @param bt  -- boxed ciphertext `c`
		 * @param btl -- boxed ciphertext length `bt.len()`
		 */
		void boxDecrypt(uint8_t* out, const uint8_t* bt,
							std::size_t btl);
		void secretbox_xchacha20poly1305_open_detached(
			uint8_t* out, const uint8_t* ct, const uint8_t* mac,
			std::size_t ctl, const uint8_t* nonce);
	};

}
