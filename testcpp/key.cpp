#include <string>
#include <iostream>
#include <fstream>

#include "key.hpp"

namespace Bupstash {

	Key::Key(const std::string& path)
	{
		CryptoPP::Base64Decoder decoder;
		readFile(path, decoder);
		decoder.MessageEnd();

		std::size_t len = decoder.MaxRetrievable();
		if(len != sizeof(struct bupstash_primary_key))
			throw std::runtime_error(
				"Invalid primary key length: " +
				std::to_string(len) + ", expected: " +
				std::to_string(sizeof(
						struct bupstash_primary_key))
			);

		decoder.Get((CryptoPP::byte*)&data, len);

		if(data.key_type != 0)
			throw std::runtime_error("Detected non-primary key. "
						"This program can only "
						"process primary keys!");
	}

	void Key::readFile(const std::string& path,
					CryptoPP::Base64Decoder& decoder)
	{
		bool process = 0;
		std::ifstream in;
		in.exceptions(std::ifstream::badbit | std::ifstream::failbit);
		try {
			in.open(path);
		} catch(std::exception& ex) {
			throw std::runtime_error("Failed to open file <" +
						path + ">: " + ex.what());
		}
		// reduce number of eceptions after open because we need to
		// explicitly exclude the case of eof && failbit
		in.exceptions(std::ifstream::badbit);
		for(std::string line; std::getline(in, line); ) {
			// trim -- https://stackoverflow.com/questions/216823
			line.erase(line.find_last_not_of("\r\n") + 1);

			if(line == "-----BEGIN BUPSTASH KEY-----")
				process = 1;
			else if(line == "-----END BUPSTASH KEY-----")
				process = 0;
			else if(process)
				decoder.Put((CryptoPP::byte*)line.data(),
								line.size());
		}
		if(in.fail() && !in.eof()) 
			throw std::runtime_error("Problems occurred while "
					"reading key file <" + path + ">.");
		// close file by leaving scope
	}

	const struct bupstash_primary_key& Key::getData() const {
		return data;
	}

}
