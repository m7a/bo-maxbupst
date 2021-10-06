#include <iostream>

#include "application.hpp"

int main(int argc, char* argv[])
{
	try {
		Bupstash::Application appl(argc, argv);
		appl.run();
		return EXIT_SUCCESS;
	} catch(const std::runtime_error& er) {
		std::cerr << er.what() << std::endl <<
				"Exiting with error code " << EXIT_FAILURE <<
				"." << std::endl;
		return EXIT_FAILURE;
	}
}
