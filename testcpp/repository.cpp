#include "repository.hpp"
#include "item.hpp"

namespace Bupstash {

	Repository::Repository(const Key& key, const std::string& path)
	{
		// TODO ... N_IMPL should scan the items?
		Item test(key, path + "/items/b52cb4e46ccbb1ff0fbb5eccb340c852");
		test.print();
	}

}
