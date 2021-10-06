#pragma once
#include <string>
#include <cryptopp/base64.h>

#include "bupstash_structures.hpp"

namespace Bupstash {

	class Key {
	public:
		Key(const std::string& path);
	private:
		struct bupstash_primary_key data;
		void readFile(const std::string& path,
					CryptoPP::Base64Decoder& decoder);
	};

}
