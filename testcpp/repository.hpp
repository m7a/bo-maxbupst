#pragma once
#include <string>

#include "key.hpp"

namespace Bupstash {

	class Repository {
	public:
		Repository(const Key& key, const std::string& path);
	};

}
