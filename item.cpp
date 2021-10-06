#include <iostream>
#include <fstream>

#include "item.hpp"

namespace Bupstash {

	Item::Item(const Key& key, const std::string& itemPath)
	{
		std::ifstream in;
		in.exceptions(std::ifstream::badbit | std::ifstream::failbit);
		try {
			in.open(itemPath, std::ios_base::binary |
							std::ios_base::in);
		} catch(std::exception& ex) {
			throw std::runtime_error("Failed to open file <" +
						itemPath + ">: " + ex.what());
		}

		// Deliberately leave the stream to throw exception if we
		// reach EOF to protect the readSerde operations.

		in.read((char*)&metaPlain.version, sizeof(uint8_t));
		if(metaPlain.version != 2)
			throw std::runtime_error("Failed to item metadata:"
					"Found version " +
					std::to_string(metaPlain.version) +
					", expected version 2.");

		in.read((char*)&metaPlain.primary_key_id,
						sizeof(struct bupstash_xid));
		in.read((char*)&metaPlain.unix_timestamp_millis,
						sizeof(uint64_t));

		readHtreeMetadata(metaPlain.data_tree, in);
		in.read((char*)&metaPlain.optional_is_index_tree_present,
							sizeof(uint8_t));
		if(metaPlain.optional_is_index_tree_present)
			readHtreeMetadata(metaPlain.index_tree, in);
	}

	void Item::readHtreeMetadata(struct bupstash_htree_metadata& htree,
							std::ifstream& in)
	{
		readSerde(htree.height, in);
		readSerde(htree.data_chunk_count, in);
		in.read((char*)&htree.address, sizeof(struct bupstash_address));
	}

	void Item::readSerde(uint64_t& target, std::ifstream& in)
	{
		target = 0;
		int numproc = 0;
		int octet;
		do {
			octet = in.get(); // assumption: not EOF!

			// By default, we read 7 bit.
			// Special case that we are just processing the tenth
			// octet, then the remaining 6 bits are defined `0` and
			// hence we only effectively retrieve one bit!
			target <<= (++numproc == 10)? 1: 7;

			target |= (octet & 0x7f);
		} while(octet & 0x80);
	}

	void Item::print()
	{
		printPlainTextItemMetadata(metaPlain);
	}

	void Item::printPlainTextItemMetadata(
			struct bupstash_v3_plain_text_item_metadata& metaPlain)
	{
		std::cout << "metaPlain=(" << std::endl;
		printXid("primary_key_id", metaPlain.primary_key_id);
		std::cout << "unix_timestamp_millis=" <<
				metaPlain.unix_timestamp_millis << std::endl;
		printHtreeMetadata("data_tree", metaPlain.data_tree);
		printHtreeMetadata("index_tree", metaPlain.index_tree);
		std::cout << ")" << std::endl;
	}

	void Item::printXid(const std::string& lbl,
						const struct bupstash_xid& xid)
	{
		std::cout << lbl << "=(";
		for(int i = 0; i < 16; i++)
			printf("%02x ", xid.bytes[i]);
		std::cout << ")" << std::endl;
	}

	void Item::printHtreeMetadata(const std::string& lbl,
				const struct bupstash_htree_metadata& htree)
	{
		std::cout << lbl << "=(height=" << htree.height << "," <<
			"data_chunk_count=" << htree.data_chunk_count << "," <<
			"address=(";
		for(int i = 0; i < BUPSTASH_ADDRESS_SZ; i++)
			printf("%02x ", htree.address.bytes[i]);
		std::cout << "))" << std::endl;
	}

}
