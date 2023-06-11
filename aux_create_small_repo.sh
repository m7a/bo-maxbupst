#!/bin/sh -exu

export BUPSTASH_KEY=./testdata/maxbupst-testkey.key
export BUPSTASH_REPOSITORY=./testdata/small-0.10.3

bupstash init
bupstash put "masysma=singlefile-bupstash-key.ads" - < bupstash_key.ads
bupstash put --exclude "*xond*" --exclude "*testdata*" --exclude "*.git*" \
							"masysma=repocnt" .
dd if=/dev/zero of=zero8m.bin bs=1M count=8
bupstash put "masysma=zero8m" ./zero8m.bin
rm zero8m.bin
