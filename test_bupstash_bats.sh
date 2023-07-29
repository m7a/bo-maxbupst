#!/bin/sh -eu
# Ma_Sys.ma Bupstash Extractor Test using Original Bupstash Test Suite
# (c) 2023 Ma_Sys.ma <info@masysma.net>

scriptroot="$(cd "$(dirname "$0")" && pwd)"
MDVL_CI_PHOENIX_ROOT="${MDVL_CI_PHOENIX_ROOT:-$scriptroot/..}"

if [ -f "$scriptroot/.env" ]; then
	. "$scriptroot/.env"
	unset BUPSTASH_REPOSITORY
	unset BUPSTASH_KEY
fi

MAXBT_LOCAL_TMP="${MAXBT_LOCAL_TMP:-/var/tmp/maxbt_local}"
MAXBT_UUT="${MAXBT_UUT:-$scriptroot/maxbupst}"

[ -d "$MAXBT_LOCAL_TMP" ] || mkdir "$MAXBT_LOCAL_TMP"
if [ -d "$MAXBT_LOCAL_TMP/bats" ]; then
	rm -rf "$MAXBT_LOCAL_TMP/bats"
fi

mkdir "$MAXBT_LOCAL_TMP/bats"
"$MDVL_CI_PHOENIX_ROOT/co-maartifact/maartifact.pl" extract bupstash.git \
			"$MAXBT_LOCAL_TMP/bats/bupstash.git" -b v0.10.3 \
			https://github.com/andrewchambers/bupstash
mv "$MAXBT_LOCAL_TMP/bats/bupstash.git/cli-tests"/* "$MAXBT_LOCAL_TMP/bats"
rm -rf "$MAXBT_LOCAL_TMP/bats/bupstash.git"

# edit the commands to make use of maxbupst over the real bupstash
replstr="\"$MAXBT_UUT\" -g -k "'"$SCRATCH/bupstash-test-primary.key"'" -i "
sed "s#bupstash get id=#$replstr#g" < "$MAXBT_LOCAL_TMP/bats/cli-tests.bats" \
				> "$MAXBT_LOCAL_TMP/bats/cli-tests-edit.bats"
sed "s#bupstash get -q id=#$replstr#g" \
				< "$MAXBT_LOCAL_TMP/bats/parallel-thrash.sh" \
				> "$MAXBT_LOCAL_TMP/bats/parallel-thrash.sh.new"
mv "$MAXBT_LOCAL_TMP/bats/parallel-thrash.sh.new" \
				"$MAXBT_LOCAL_TMP/bats/parallel-thrash.sh"

cd "$MAXBT_LOCAL_TMP/bats"
ulimit -s unlimited

# run only a subset of the tests where bupstash get is involved
exec bats --filter "^(simple put|put name|random data|highly crompre|key mism|corruption|concurrent|simple search|rm and|query sync|send directory|stat cache|key command|long path|long link target|directory exclusions|checkpoint|rm from|concurrent dir|list and rm|multi dir|hard link short|long hard link|hard link to|parallel thrash)" cli-tests-edit.bats
