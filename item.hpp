#pragma once
#include "bupstash_structures.hpp"
#include "key.hpp"

namespace Bupstash {

	class Item {
	public:
		Item(const Key& key, const std::string& itemPath);
		void print();
	private:
		struct bupstash_v3_plain_text_item_metadata metaPlain;
		struct bupstash_v3_secret_item_metadata metaSecret;

		void readHtreeMetadata(struct bupstash_htree_metadata& htree,
							std::ifstream& in);
		void readSerde(uint64_t& target, std::ifstream& in);
		void printPlainTextItemMetadata(
			struct bupstash_v3_plain_text_item_metadata& metaPlain);
		void printXid(const std::string& lbl,
						const struct bupstash_xid& xid);
		void printHtreeMetadata(const std::string& lbl,
				const struct bupstash_htree_metadata& htree);
	};

}
