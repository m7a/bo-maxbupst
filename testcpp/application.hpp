#pragma once
#include <string>
#include <memory>
#include "key.hpp"
#include "repository.hpp"

namespace Bupstash {

	class Application {
	public:
		Application(int argc, char* const argv[]);
		void run();
	private:
		std::string action;
		std::string keyFile;
		std::string repositoryPath;
		std::string selectedID;
		std::unique_ptr<Key> key;
		std::unique_ptr<Repository> repository;

		void help();
		void actionList();
		void actionGet();
	};

}
