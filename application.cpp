#include <iostream>

#include "application.hpp"
#include "key.hpp"

// TODO z NB: Existing backups use schema version 5, new repository uses schema version 6!

namespace Bupstash {

	Application::Application(int argc, char* const argv[])
	{
		if(argc < 2)
			throw std::runtime_error("Need at least one "
					"argument. Re-invoke with "
					"--help to display help screen.");

		action = argv[1];

		if(const char* ek = std::getenv("BUPSTASH_KEY"))
			keyFile = ek;
		if(const char* er = std::getenv("BUPSTASH_REPOSITORY"))
			repositoryPath = er;

		for(int i = 3; i < argc; i += 2) {
			if(argv[i - 1][0] == 0 || argv[i - 1][1] == 0)
				throw std::runtime_error("Argument " +
							std::to_string(i) +
							" may not be empty.");
				
			switch(argv[i - 1][1]) {
			case 'k': keyFile        = argv[i]; break;
			case 'r': repositoryPath = argv[i]; break;
			case 'i': selectedID     = argv[i]; break;
			default:  throw std::runtime_error(
					"Unknown argument: <" +
					std::string(argv[i - 1]) + ">");
			}
		}
	}

	void Application::run()
	{
		if(action == "-h" || action == "help" || action == "--help") {
			help();
			return;
		}

		if(keyFile.empty())
			throw std::runtime_error("Need to specify "
				"keyfile. Use -k KEY or "
				"BUPSTASH_KEY environment variable.");

		if(repositoryPath.empty())
			throw std::runtime_error("Need to specify "
				"repository directory. Use -r REPO or "
				"BUPSTASH_REPOSITORY environment variable.");
				
		key.reset(new Key(keyFile));
		repository.reset(new Repository(*key, repositoryPath));

		if(action == "-l" || action == "list")
			actionList();
		else if(action == "-g" || action == "get")
			actionGet();
	}

	void Application::help()
	{
		std::cout << "Ma_Sys.ma Bupstash Extraction Tool 1.0.0,"
				"Copyright (c) 2021 Ma_Sys.ma" << std::endl <<
			"For further info send an e-mail to Ma_Sys.ma@web.de."
			<< std::endl << std::endl <<
			"USAGE $0 --help                            "
				"Display help screen." << std::endl <<
			"USAGE $0 -l|list [-k KEY] [-r REPO]        "
				"List backups." << std::endl <<
			"USAGE $0 -g|get  [-k KEY] [-r REPO] -i ID  "
				"Restore backup. Append | tar -x." <<
				std::endl << std::endl <<
			"Environment variable BUPSTASH_KEY        "
				"can supply value for KEY." << std::endl <<
			"Environment variable BUPSTASH_REPOSITORY "
				"can supply value for REPOSITORY."
				<< std::endl << std::endl <<
			"Current parameter values:" << std::endl << std::endl <<
			"        KEY  = \"" << keyFile << "\""
				<< std::endl <<
			"        REPO = \"" << repositoryPath << "\""
				<< std::endl <<
			"        ID   = \"" << selectedID << "\""
				<< std::endl << std::endl <<
			"Arguments take precedence over environment." <<
			std::endl;
	}

	void Application::actionList()
	{
		// TODO ...
	}

	void Application::actionGet()
	{
		// TODO ...
	}
}
