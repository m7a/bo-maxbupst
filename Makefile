# DEP libcrypto++-dev

extract_bupstash: Makefile *.cpp *.hpp
	g++ -Wall -o extract_bupstash *.cpp -lcrypto++

test-list:
	./extract_bupstash list -k test-bupstash.key -r testrepo
